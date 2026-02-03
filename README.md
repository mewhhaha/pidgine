# redatared

Small, pure data layer for game engines with a minimal FRP core and a tiny ECS.
Pattern name: **ECP** (Entity‑Component‑Program). The API is intentionally simple and favors short, single‑word names.

See `docs/first-principles.md` for the core model and runtime semantics.

Suggested imports (to avoid name clashes with Prelude):

```haskell
import qualified Engine.Data.FRP as F
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Input as I
import qualified Engine.Data.Transform as T
```

Note: Some examples use `do` with Applicative only; enable `ApplicativeDo`.

## Philosophy (the short version)

- Continuous, async-first: programs are coroutines that run across frames; `await` is explicit and everything else stays pure.
- `await` is overloaded: you can `await` a handle, an event predicate, or an explicit `Await` value.
- Time is first-class: `Step`/`Signal`/`Tween` model time directly and can be used inside programs via `S.step`.
- Per-entity programs live in program locals: `eachM @Key` creates per-entity programs stored in program locals (not on entities), so entities only store components.
- One sync point: `batch ...` gathers `each`/`collect` and runs them in one pass.
- Composition everywhere: queries are Applicative, patches are Semigroup/Monoid, steps compose, programs can `await` other programs/events.

## FRP: Practical examples

These `Step`s can run standalone via `F.run` or inside programs via `S.step`.

### 1) A constant signal

```haskell
sig :: F.Signal Int
sig = pure 7

out :: [Int]
out = F.run sig [(1, ()), (1, ()), (1, ())]
-- [7,7,7]
```

### 2) Delta time and integration

```haskell
dtSamples :: [Double]
dtSamples = F.run F.dt [(0.1, ()), (0.2, ()), (0.3, ())]
-- [0.1,0.2,0.3]

timeSamples :: [Double]
timeSamples = F.run F.time [(1, ()), (0.5, ()), (2, ())]
-- [1.0,1.5,3.5]

pos :: F.Step Double Double
pos = F.int

posOut :: [Double]
posOut = F.run pos [(1, 2), (1, 2), (0.5, 2)]  -- velocity = 2
-- [2.0,4.0,5.0]
```

### 2b) Ranges and timers

```haskell
-- True while time is in [0.5, 1.0)
inRange :: [Bool]
inRange = F.run (F.during (0.5, 1.0)) [(0.25,()), (0.25,()), (0.25,()), (0.25,())]
-- [False,True,True,False]

-- Progress 0..1 while inside a range
prog :: [Maybe Double]
prog = F.run (F.range (1, 2)) [(0.5,()), (0.5,()), (0.5,())]
-- [Nothing,Just 0.0,Just 0.5]

-- Emit once per second
pulse :: [F.Events ()]
pulse = F.run (F.every 1) [(0.4,()), (0.4,()), (0.4,()), (0.4,())]
-- [[],[],[()],[]]

-- Move for 1 second at 5 units/sec (progress-based, clamped)
move1s :: [Double]
move1s = map (*5) (F.run (F.progress (0, 1)) [(0.5,()), (0.5,()), (0.5,())])
-- [2.5,5.0,5.0]   -- progress clamps after the range

-- Gate any step to a time window
gated :: [Maybe Int]
gated = F.run (F.window (1,2) (pure 9)) [(0.5,()), (0.6,()), (0.6,())]
-- [Nothing,Just 9,Just 9]

-- First-class time: Span + Tween
easeInOut :: Double -> Double
easeInOut u = u*u*(3 - 2*u)

lerp :: Double -> Double -> Double -> Double
lerp a b u = a + (b - a) * u

moveTween :: F.Tween Double
moveTween = F.tween (F.Span 0 1) easeInOut (lerp 0 10)

moveOut :: [Double]
moveOut = F.run (F.sample moveTween) [(0.5,()), (0.5,()), (0.5,())]
-- [5.0,10.0,10.0]

-- Platform ping-pong tween (back and forth)
pingPong :: Double -> Double
pingPong x =
  let u = x - fromIntegral (floor x :: Int)
  in if u <= 0.5 then 2 * u else 2 * (1 - u)

platformX :: F.Step () Double
platformX =
  let period = 2.0
      tweenX = F.tween (F.Span 0 1) easeInOut (lerp (-3) 3)
  in F.at tweenX . pingPong . (/ period) <$> F.time

platformOut :: [Double]
platformOut = F.run platformX [(0.5,()), (0.5,()), (0.5,()), (0.5,())]
-- [~0.0,~3.0,~0.0,~-3.0]
```

### 2c) Relative timers (event-driven)

```haskell
-- Time since "dash" was pressed.
dashTime :: [Double]
dashTime = F.run (F.since (I.press (I.Button "dash")))
  [ (0.1, I.Input [] [] [] [])
  , (0.1, I.Input [I.Button "dash"] [] [] [])
  , (0.1, I.Input [] [] [] [])
  ]
-- [0.1,0.0,0.1]

-- Invulnerable for 1 second after a hit.
invuln :: F.Step I.Input Bool
invuln = F.forFrom (I.press (I.Button "hit")) 1.0
```

### 2d) Step inside Program (composition)

```haskell
-- Run a Step inside a Program (state is kept per-program).
data Pos = Pos Double
data WiggleTween

wiggleProg :: Program msg
wiggleProg = S.program (S.handle 0) $ do
  let tweenX = F.tween (F.Span 0 1) easeInOut (lerp (-2) 2)
  x <- S.step @WiggleTween (F.sample tweenX) ()
  _ <- S.batch $ do
    S.each (E.comp @Pos) $ \_ ->
      S.set (Pos x)
```

Note: `S.step @Key` stores Step state under a type key. Use a unique key per independent Step.
Inside `eachM @Key`, `S.step @Key` becomes per‑entity automatically.
Bind `S.step`/`S.time` once per tick if you need the value multiple times.

### 3) Delay and hold

```haskell
prev :: F.Step Int Int
prev = F.delay 0

prevOut :: [Int]
prevOut = F.run prev [(1, 1), (1, 2), (1, 3)]
-- [0,1,2]

held :: F.Step (F.Events Int) Int
held = F.hold 0

heldOut :: [Int]
heldOut = F.run held [(1, [1,2]), (1, []), (1, [5])]
-- [2,2,5]
```

### 4) Accumulate state

```haskell
acc :: F.Step (F.Events (Int -> Int)) Int
acc = F.acc 0

accOut :: [Int]
accOut =
  F.run acc
    [ (1, [(+1), (+1)])
    , (1, [])
    , (1, [(*10)])
    ]
-- [2,2,20]
```

### 5) Events: lists per tick

```haskell
tick :: F.Signal (F.Events Int)
tick = pure [1]

evtOut :: [F.Events Int]
evtOut = F.run tick [(1, ()), (1, ())]
-- [[1],[1]]

merged :: F.Events Int
merged = [1,2] <> [3]
-- [1,2,3]

scaled :: F.Events Int
scaled = map (*2) [1,2,3]
-- [2,4,6]

onlyEven :: F.Events Int
onlyEven = Prelude.filter even [1,2,3,4]
-- [2,4]
```

### 6) Switching

```haskell
-- Switch to a new step when an event fires.
switcher :: F.Step Int (Int, F.Events (F.Step Int Int))
switcher = do
  b <- pure 0
  ev <- F.after 1
  pure (b, fmap (const (pure 999)) ev)

out :: [Int]
out = F.run (F.switch switcher) [(0.5, 10), (0.5, 10), (1.0, 10)]
-- [0,999,999]

-- Delayed switch (current output uses old step).
outD :: [Int]
outD = F.run (F.switchd switcher) [(0.5, 10), (0.5, 10), (1.0, 10)]
-- [0,0,999]
```

### 7) Parallel composition

```haskell
pair :: F.Step Int (Int, Int)
pair = do
  a <- pure 1
  b <- pure 2
  pure (a, b)

out :: [(Int, Int)]
out = F.run pair [(1, 10), (1, 10)]
-- [(1,2),(1,2)]
```

---

## Input: Practical examples

```haskell
jump :: F.Step I.Input (F.Events ())
jump = I.press (I.Button "jump")

left :: F.Step I.Input Bool
left = I.held (I.Button "left")

moveX :: F.Step I.Input Double
moveX = I.axis "x"

waitJump :: Program I.Input
waitJump = S.program (S.handle 0) $ do
  _ <- S.await (I.justPressed (I.Button "jump"))
  pure ()

waitDash :: Program I.Input
waitDash = S.program (S.handle 0) $ do
  _ <- S.await (I.axisAbove "x" 0.8)
  pure ()

sample :: I.Input
sample = I.Input
  { I.down = [I.Button "jump"]
  , I.up = []
  , I.holds = [I.Button "left"]
  , I.axes = [("x", 0.5)]
  }
```

---

## Composability patterns

### 1) Applicative steps (parallel programs)

```haskell
sysA :: F.Step I.Input Int
sysA = pure 1

sysB :: F.Step I.Input Int
sysB = pure 2

sysAB :: F.Step I.Input (Int, Int)
sysAB = do
  a <- sysA
  b <- sysB
  pure (a, b)
```

### 1b) Applicative do (no monad needed)

```haskell
-- Independent inputs compose in parallel.
stick :: F.Step I.Input (Double, Double)
stick = do
  x <- I.axis "x"
  y <- I.axis "y"
  pure (x, y)
```

### 2) Queries as Applicatives

```haskell
q :: Query (Pos, Maybe Vel)
q = do
  p <- E.comp
  v <- E.opt
  pure (p, v)
```

### 3) Patches as Monoid (merge programs)

```haskell
data Game
data Physics
data AI

physics :: F.Step () Patch
physics = pure (S.patch id)

ai :: F.Step () Patch
ai = pure (S.patch id)

gameProg :: Program msg
gameProg = S.program (S.handle 0) $ do
  p <- S.step @Physics physics ()
  a <- S.step @AI ai ()
  S.world (p <> a)
```

---

## ECS: Practical examples

These examples use direct `E.spawn` calls. It keeps world building explicit and easy to reason about.

### World layout (ownership)

```
World
  entities  : [(EntityId, Sig, Bag C)]   -- newest first (descending EntityId); Sig is a component bitset
  resources : IntMap Any
  relations : out/in (edge type -> adjacency)

Graph
  programs   : [Program]

Program (coroutine)
  - handle id
  - step state
  - local state (per-program)
  - per-entity programs (each @Key) stored in program locals (keyed by EntityId)
```

Entities own only components; program-local programs/state are not stored on entities.

### 1) Spawn and access components

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

data Pos = Pos Double Double deriving (Show, Eq)
data Vel = Vel Double Double deriving (Show, Eq)
data Player = Player deriving (Show, Eq)
data Enemy = Enemy deriving (Show, Eq)

-- Component wrapper (extend with your game's components).
data C
  = CPos Pos
  | CVel Vel
  | CPlayer Player
  | CEnemy Enemy
  deriving (Generic)

instance E.ComponentId C

-- Note: component wrappers should be a sum of unary constructors so the
-- bitset index can be derived automatically.
-- No `Component` instances are required; they are inferred from the wrapper.

type World = E.World C
type Query a = E.Query C a
type Graph msg = S.Graph C msg
type Program msg a = S.Program C msg a
type Patch = S.Patch C

(e, w1) = E.spawn (Pos 0 0, Vel 1 0) (E.emptyWorld :: World)

E.get e w1 :: Maybe Pos
-- Just (Pos 0 0)

w2 = E.set e (Pos 10 5) w1
E.get e w2 :: Maybe Pos
-- Just (Pos 10 5)

E.has @Vel e w2
-- True
```

Bundles let you reuse component sets and compose them in tuples:

```haskell
data PlayerBundle = PlayerBundle Pos Vel

instance E.Bundle PlayerBundle where
  bundle (PlayerBundle p v) = E.bundle (p, v)

(p, w3) = E.spawn (PlayerBundle (Pos 0 0) (Vel 1 0)) w2
(q, w4) = E.spawn (PlayerBundle (Pos 2 3) (Vel 0 1), Player) w3
```

### 2) Simple queries

```haskell
-- All entities with Pos
posQ :: Query Pos
posQ = E.comp

E.runq posQ w2
-- [(Entity 0, Pos 10 5)]
```

### 3) Join two components

```haskell
pairQ :: Query (Pos, Vel)
pairQ = do
  p <- E.comp
  v <- E.comp
  pure (p, v)

E.runq pairQ w2
-- [(Entity 0, (Pos 10 5, Vel 1 0))]
```

### 4) Optional component

```haskell
optQ :: Query (Pos, Maybe Vel)
optQ = do
  p <- E.comp
  v <- E.opt
  pure (p, v)
```

### 5) Auto-derived record queries

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics (Generic)

data Player = Player deriving (Eq, Show)
data Enemy = Enemy deriving (Eq, Show)
data Position = Position Double Double deriving (Eq, Show)
data Velocity = Velocity Double Double deriving (Eq, Show)

data PlayerQuery = PlayerQuery
  { player :: Player
  , position :: Position
  , velocity :: Velocity
  } deriving (Generic)

instance E.Queryable C PlayerQuery

data PlayerMaybe = PlayerMaybe
  { player :: E.Has Player
  , position :: Maybe Position
  } deriving (Generic)

instance E.Queryable C PlayerMaybe

data NotEnemy = NotEnemy
  { enemy :: E.Not Enemy
  , position :: Position
  } deriving (Generic)

instance E.Queryable C NotEnemy

players = E.runq (E.query @PlayerQuery) w
```

Queryable auto-derives for product types. For sum types, use `QueryableSum`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

data Who = P Player | E Enemy
  deriving (Generic)

instance E.QueryableSum C Who

whoQ = E.querySum @Who
```

Sum queries use `<|>` and therefore skip signature pruning.

### 6) Filter and map queries

```haskell

fastQ :: Query Vel
fastQ = E.filterQ (\(Vel x y) -> x*x + y*y > 1) E.comp

namesQ :: Query String
namesQ = do
  p <- E.comp
  pure (case p of Pos x y -> show (x, y))
```

### 7) Fold without building lists

```haskell
count :: Int
count = E.foldq (E.comp :: Query Pos) (\_ _ n -> n + 1) 0 w2
```

### 8) Update a world in place

```haskell
move :: Double -> World -> World
move dt w =
  E.foldq (do p <- (E.comp :: Query Pos); v <- (E.comp :: Query Vel); pure (p, v))
    (\e (Pos x y, Vel vx vy) acc ->
      E.set e (Pos (x + vx * dt) (y + vy * dt)) acc
    ) w w
```

### 9) Relationships (typed edges)

```haskell
data Owns
data Targets

-- a owns b
w3 = E.relate @Owns a b w2

-- outgoing relationships
E.out @Owns a w3

-- incoming relationships
E.inn @Owns b w3

-- remove relation
w4 = E.unrelate @Owns a b w3
```

### 10) Parent/child transforms (Bevy‑style)

```haskell
-- attach parent -> child
w3 = T.attach parent child w2

-- propagate transforms (global from local + parent)
w4 = T.propagate w3

-- Example locals
w2 =
  E.set parent (T.Local (T.translate (0,0,0))) $
  E.set child  (T.Local (T.translate (2,0,0))) w1

-- Resulting globals:
-- parent = translate (0,0,0), child = translate (2,0,0)
```

### 11) Matrix helpers

```haskell
-- Build a local transform: translate * rotate * scale
local = T.compose [T.translate (1,2,3), T.rotateZ 0.5, T.scale (2,2,2)]

-- Invert a transform (affine)
inv = T.inverse local

-- Maybe-returning inverse (fallback to identity)
invOrId = maybe T.identity id inv

-- Typeclass composition
world = T.ident T.<.> local
```

---

## Programs: FRP + ECS together (async list)

Note: programs use explicit typed handles. You can create them manually with
`S.handle n` (unique per graph), or use the graph builder `S.graphM` to auto‑allocate
handles for you. `await` accepts handles directly (no `awaitProgram` needed).
`Program msg a` is the program type (with `C` baked in); use `Program msg ()` for
side‑effect programs, and any `a` if you want to `await` a value.

Programs accumulate changes via `S.edit` (entity‑local) and `S.world` (global). Use
`S.set`/`S.update`/`S.del` to build entity patches, and `S.setAt`/`S.updateAt`/`S.delAt`,
`S.put`, `S.relate`, etc for global patches.
The runtime
re-runs only programs that return `wait` (directly or via `await` gating) until no
new messages appear, so `await` can be satisfied within the same frame. The returned outbox is the
aggregate of all messages emitted this frame.
When an `await` is not ready, the rest of that program does not run for the tick.
Programs are coroutines: they resume at the suspended `await` on later frames,
and restart from the top once they return.

Entity iteration lives inside `S.batch $ do ...` (it already waits for the sync point).
`S.each` (pure patch) and `S.collect` are only usable in a `batch`, so you can’t
accidentally run them outside.
A `batch` reads one snapshot, fuses the work into one pass, and applies patches once.
If you want `do`‑notation inside `batch`, enable `ApplicativeDo`.

For per‑entity continuations, use `eachM @Key` with a monadic body. This gives each
entity its own time/state machine without storing Steps in components.
`eachM @Key` resumes the per‑entity program; it is intentionally entity‑local (edit/await/send/step).
Note: some longer examples below still show `gather` or `each` with `edit`; translate
them to `batch ...` and `eachM` when you need monadic actions.
If you need fresh data or cross‑entity effects, split into another program and communicate
via events, then `await` the sync point you need.

Graph construction is variadic: `S.graph sys1 sys2 sys3` (no list needed).

Within a single `batch`, all `each`/`collect` calls read the same snapshot;
their patches are merged and applied once. If you need “see the result of a
previous update,” split it into another program and `await x`, then `batch` again.

### Graph builder (auto handles)

If you don’t want to manage handle integers manually, use `graphM`:

```haskell
build :: (S.Handle Double, Graph ())
build =
  S.graphM $ do
    speedH <- S.programM (pure 2.5)
    _moveH <- S.programM $ do
      speed <- S.await speedH
      dt <- S.dt
      _ <- S.batch $ do
        S.each (E.comp :: Query Pos) $ \(Pos x y) ->
          S.set (Pos (x + speed * dt) y)
      pure ()
    pure speedH

(speedH, g0) = build
```

```haskell
data Move

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic)

instance E.Queryable C MoveQ

moveProg :: Program ()
moveProg = S.program (S.handle 0) $ do
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \q ->
      let Pos x y = pos q
          Vel vx vy = vel q
      in S.set (Pos (x + vx * dt) (y + vy * dt))
  pure ()

g0 :: Graph ()
g0 = S.graph moveProg

runFrame :: F.DTime -> World -> Graph () -> (World, Graph ())
runFrame dt w g =
  let (w', _out, g') = S.run dt w [] g
  in (w', g')
```

Note: Each round can be parallelized by evaluating all runnable programs
against the same snapshot/inbox and then merging patches in list order.
The current `run` executes sequentially for simplicity.

Patch helpers also support update-in-place:

```haskell
data Score = Score Int

bumpScore :: E.Entity -> Patch
bumpScore e = S.updateAt e (\(Score n) -> Score (n + 1))

resetScore :: E.Entity -> Patch
resetScore e = S.updateAt e (const (Score 0)) -- or S.setAt e (Score 0)
```

---

## Program patterns

### 1) Independent programs in one frame

```haskell
data Move
data AI
data Anim

moveProg :: Program ()
moveProg = S.program (S.handle 0) (pure ())

aiProg :: Program ()
aiProg = S.program (S.handle 1) (pure ())   -- placeholder

animProg :: Program ()
animProg = S.program (S.handle 2) (pure ()) -- placeholder

g0 :: Graph ()
g0 = S.graph moveProg aiProg animProg
```

### 2) Deterministic patch order

```haskell
data Physics
data Collision
data Damage

physicsProg :: Program ()
physicsProg = S.program (S.handle 0) (pure ())

collisionProg :: Program ()
collisionProg = S.program (S.handle 1) (pure ())  -- resolve contacts

damageProg :: Program ()
damageProg = S.program (S.handle 2) (pure ())

g0 :: Graph ()
g0 = S.graph physicsProg aiProg collisionProg damageProg animProg
```

### 3) Snapshot semantics

```haskell
-- All programs read the same snapshot; list order only affects patch application.
data WritePos
data ReadPos

writePos :: Program ()
writePos = S.program (S.handle 0) $ do
  _ <- S.batch $ do
    S.each (E.comp :: Query Pos) $ \(Pos x y) ->
      S.set (Pos (x + 1) y)
  pure ()

readPos :: Program ()
readPos = S.program (S.handle 1) (pure ()) -- build a spatial index

g0 :: Graph ()
g0 = S.graph writePos readPos
```

### 4) Messaging between programs

```haskell
data Msg = Fire | SpawnBullet deriving (Eq, Show)
data FireProg
data SpawnProg

fireProg :: Program Msg
fireProg = S.program (S.handle 0) $ do
  _fires <- S.awaitEvent (== Fire)
  S.send [SpawnBullet]

spawnProg :: Program Msg
spawnProg = S.program (S.handle 1) $ do
  spawns <- S.awaitEvent (== SpawnBullet)
  let p = foldMap (const (S.setAt e Bullet)) spawns  -- assume e exists
  S.world p
  pure ()

-- `run` already feeds outbox back into inbox within the frame (bounded).
-- The returned outbox is the aggregate of all messages emitted this frame.
```

### 4b) Movement waits for velocity update

This pattern lets you write programs without rigid ordering: a program can
use `await` to gate until another program has run in‑frame, and `run` will re‑run it in‑frame.

```haskell
data Vel = Vel Double Double
data Pos = Pos Double Double

data Speed
data Move

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic)

instance E.Queryable C MoveQ

speedH :: S.Handle ()
speedH = S.handle 0

moveH :: S.Handle ()
moveH = S.handle 1

speedProg :: Program ()
speedProg = S.program speedH $ do
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \q ->
      let Vel vx vy = vel q
      in S.set (Vel (vx * 1.1) (vy * 1.1))
  pure ()

moveProg :: Program ()
moveProg = S.program moveH $ do
  _ready <- S.await speedH
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \q ->
      let Pos x y = pos q
          Vel vx vy = vel q
      in S.set (Pos (x + vx * dt) (y + vy * dt))
  pure ()

g0 :: Graph ()
g0 = S.graph moveProg speedProg  -- order doesn’t matter: moveProg waits
```

### 4c) Await a program value

`await` returns the value produced by the program (for its last in‑frame run).
Use `Program msg a` for non‑`()` values; `graph` accepts any mix of return types.

```haskell
data Speed
data Move

speedH :: S.Handle Double
speedH = S.handle 0

moveH :: S.Handle ()
moveH = S.handle 1

speedProg :: Program () Double
speedProg = S.program speedH $ pure 2.5

moveProg :: Program ()
moveProg = S.program moveH $ do
  speed <- S.await speedH
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.comp :: Query Pos) $ \(Pos x y) ->
      S.set (Pos (x + speed * dt) y)
  pure ()

g0 :: Graph ()
g0 = S.graph speedProg moveProg
```

### 4c.1) Batching entity work

Use `batch ...` to run entity work (it waits for the sync point). The batch
reads one snapshot and applies patches once.

```haskell
hits <- S.batch $ do
  S.each (E.comp @Pos) $ \(Pos x y) ->
    S.set (Pos (x + 1) y)
  S.collect (E.comp @Pos)
```

Another common pattern is “compute collisions once, then consume them”:

```haskell
data Collide
data Damage

data Collision = Collision Entity Entity

collideH :: S.Handle [Collision]
collideH = S.handle 0

damageH :: S.Handle ()
damageH = S.handle 1

collideProg :: Program () [Collision]
collideProg = S.program collideH $ do
  pairs <- S.batch $ S.collect (E.query @PairQ)  -- PairQ omitted for brevity
  pure (detect pairs)                 -- detect :: [(Entity, PairQ)] -> [Collision]

damageProg :: Program ()
damageProg = S.program damageH $ do
  collisions <- S.await collideH
  let p = foldMap applyDamage collisions
  S.world p
  pure ()

g1 :: Graph ()
g1 = S.graph collideProg damageProg
```

### 4d) Collect after sync point

`await Update` waits until all programs have run once in the frame, then `collect` returns a snapshot.

```haskell
data EnemyQ = EnemyQ
  { enemy :: Enemy
  , posE :: Pos
  } deriving (Generic)

instance E.Queryable C EnemyQ

data CountEnemies

countEnemies :: Program ()
countEnemies = S.program (S.handle 0) $ do
  enemies <- S.batch $ S.collect (E.query @EnemyQ)
  let n = length enemies
  pure ()
```

### 5) Task queue as data (async runtime)

```haskell
data Job = LoadTex String | PlaySound String

jobsProg :: Program I.Input
jobsProg = S.program (S.handle 0) $ do
  _ <- S.await (I.justPressed (I.Button "jump"))
  S.send [PlaySound "jump.wav"]

-- Runtime: drain Events Job and perform IO outside the pure core.
```

---

## Common scenarios

### 1) Animating any value (tween)

```haskell
-- Linear tween from 0 to target over duration seconds.
tween :: Double -> Double -> F.Step a Double
tween duration target = do
  u <- F.progress (0, duration)
  pure (u * target)

-- Example: animate from 0 to 10 over 2 seconds
-- F.run (tween 2 10) [(0.5, ()), (0.5, ()), (1.0, ())]
-- [2.5,5.0,10.0]
--
-- From/to tween:
tween2 :: Double -> Double -> Double -> F.Step a Double
tween2 duration from to = do
  u <- F.progress (0, duration)
  pure (from + u * (to - from))

-- Program form (sample a tween with program time)
-- assume Pos component
-- tweenProg = S.program (S.handle 0) $ do
--   x <- S.sample (F.tween (F.Span 0 duration) id (lerp 0 target))
--   _ <- S.batch $ do
--     S.each (E.comp @Pos) $ \_ -> S.set (Pos x)
--   pure ()
```

### 2) Animated sprites (frame selection)

```haskell
sprite :: Double -> [a] -> F.Step () (Maybe a)
sprite fps frames = do
  t <- F.time
  pure (pick t)
  where
    n = length frames
    pick t =
      if n == 0
        then Nothing
        else Just (frames !! (floor (t * fps) `mod` n))

-- Example: 4 frames at 12 fps
-- F.run (sprite 12 [0,1,2,3]) [(0.1,()), (0.1,()), (0.1,())]

-- Program form:
-- spriteProg = S.program (S.handle 0) $ do
--   t <- S.time
--   let i = floor (t * fps) `mod` n
--   S.send [Frame (frames !! i)]
```

### 3) Collisions (AABB)

```haskell
data Pos = Pos Double Double
data Size = Size Double Double

aabb :: Pos -> Size -> Pos -> Size -> Bool
aabb (Pos x1 y1) (Size w1 h1) (Pos x2 y2) (Size w2 h2) =
  abs (x1 - x2) * 2 < (w1 + w2) &&
  abs (y1 - y2) * 2 < (h1 + h2)

-- Find all colliding pairs (naive)
collisions :: World -> [(E.Entity, E.Entity)]
collisions w =
  pairs
    & Prelude.filter (\((e1,p1,s1),(e2,p2,s2)) -> aabb p1 s1 p2 s2)
    & Prelude.map (\((e1,_,_),(e2,_,_)) -> (e1, e2))
  where
    (&) = flip ($)
    xs =
      E.foldq (do p <- (E.comp :: Query Pos); s <- (E.comp :: Query Size); pure (p, s))
        (\e (p, s) acc -> (e, p, s) : acc) [] w
    pairs = [(a,b) | (i,a) <- zip [0..] xs, (j,b) <- zip [0..] xs, i < j]
```

### 4) Parallelism (compose independent programs)

```haskell
physicsProg :: Program msg
physicsProg = S.program (S.handle 0) (pure ())

aiProg :: Program msg
aiProg = S.program (S.handle 1) (pure ())

gameGraph :: Graph msg
gameGraph = S.graph physicsProg aiProg
```

### 5) Fixed timestep accumulator

```haskell
-- Convert variable dt to fixed 60Hz steps.
fixed60 :: F.Step a (F.Events ())
fixed60 = F.every (1 / 60)

-- Program form:
-- fixedProg = S.program (S.handle 0) $ do
--   ticks <- S.step @Fixed fixed60 ()
--   S.send (map (const Tick) ticks)
```

### 6) Cooldown / rate limiter

```haskell
cooldown :: Double -> F.Step (F.Events a) (F.Events a)
cooldown t = F.switch idle
  where
    idle = do
      evs <- F.id
      let out = take 1 evs
          next = if null out then [] else [cooling]
      pure (out, next)

    cooling = do
      done <- F.after t
      pure ([], map (const idle) done)
```

### 7) Camera follow

```haskell
data Cam = Cam { cx :: Double, cy :: Double }

follow :: Double -> F.Step (Pos, Cam) Cam
follow stiffness = do
  (Pos x y, Cam cx0 cy0) <- F.id
  let cx1 = cx0 + (x - cx0) * stiffness
      cy1 = cy0 + (y - cy0) * stiffness
  pure (Cam cx1 cy1)
```

### 8) Network interpolation (pure)

```haskell
data Snap = Snap { sx :: Double, sy :: Double, st :: Double }

interp :: Snap -> Snap -> Double -> Pos
interp (Snap x0 y0 t0) (Snap x1 y1 t1) t =
  let u = (t - t0) / max 1e-6 (t1 - t0)
  in Pos (x0 + u * (x1 - x0)) (y0 + u * (y1 - y0))
```

### 9) Animation blending

```haskell
blend :: Double -> Double -> Double -> Double
blend a b t = a * (1 - t) + b * t
```

---

## Enemy state machines (timed behaviors)

```haskell
data Sense = Sense { alert :: Bool } deriving (Eq, Show)
data AI = Idle | Windup | Attack | Recover deriving (Eq, Show)

tag :: b -> F.Events a -> F.Events b
tag b = fmap (const b)

idle :: F.Step Sense (AI, F.Events (F.Step Sense AI))
idle = do
  s <- F.id
  ev <- F.after 5
  pure (Idle, (if alert s then [windup] else []) <> tag windup ev)

windup :: F.Step Sense (AI, F.Events (F.Step Sense AI))
windup = do
  s <- F.id
  ev <- F.after 0.5
  pure (Windup, (if not (alert s) then [idle] else []) <> tag attack ev)

attack :: F.Step Sense (AI, F.Events (F.Step Sense AI))
attack = do
  ev <- F.after 0.2
  pure (Attack, tag recover ev)

recover :: F.Step Sense (AI, F.Events (F.Step Sense AI))
recover = do
  ev <- F.after 1.0
  pure (Recover, tag idle ev)

enemy :: F.Step Sense AI
enemy = F.switch idle
```

This pattern is explicit: each state returns `(state, events)` where events
carry the next state. Time‑based transitions use `F.after`. The state machine
runs continuously during gameplay; it is not a one‑time “startup” action.

Program form:

```haskell
data EnemyAIKey
data EnemyLoop

data EnemyQ = EnemyQ { sense :: Sense }
  deriving (Generic)

instance E.Queryable C EnemyQ

enemyProg :: Program msg
enemyProg = S.program (S.handle 0) $ do
  _ <- S.batch $ do
    S.eachM @EnemyLoop (E.query @EnemyQ) $ \q -> do
      st <- S.step @EnemyAIKey enemy (sense q)
      S.edit (S.set st)
  pure ()
```

Tip: for per‑entity time, use `S.step @Key F.time ()` inside `each @Key`.

---

## Mini‑games (practical sketches)

The examples below are intentionally small and practical. They show how you can
structure a game loop using `Program` (Step‑style time), `World`, and `Query`.

### 1) Pong (ECS + FRP)

```haskell
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Pos = Pos Double Double deriving (Show, Eq)
data Vel = Vel Double Double deriving (Show, Eq)
data Paddle = Paddle deriving (Show, Eq)
data Ball = Ball deriving (Show, Eq)

initWorld :: World
initWorld =
  let (_, w1) = E.spawn (Paddle, Pos (-8) 0) E.emptyWorld
      (_, w2) = E.spawn (Paddle, Pos 8 0) w1
      (_, w3) = E.spawn (Ball, Pos 0 0, Vel 5 2) w2
  in w3

data Move
data Bounce

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic)

instance E.Queryable C MoveQ
moveH :: S.Handle ()
moveH = S.handle 0

bounceH :: S.Handle ()
bounceH = S.handle 1

moveProg :: Program ()
moveProg = S.program moveH $ do
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \q ->
      let Pos x y = pos q
          Vel vx vy = vel q
      in S.set (Pos (x + vx * dt) (y + vy * dt))
  pure ()

bounceProg :: Program ()
bounceProg = S.program bounceH $ do
  _ <- S.await moveH
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \q ->
      let Pos _ y = pos q
          Vel vx vy = vel q
      in S.set (Vel vx (if y > 9 || y < (-9) then -vy else vy))
  pure ()

pongGraph0 :: Graph ()
pongGraph0 = S.graph moveProg bounceProg

frame :: F.DTime -> World -> Graph () -> (World, Graph ())
frame dt w g =
  let (w', _out, g') = S.run dt w [] g
  in (w', g')
```

### 2) Snake (grid + events)

```haskell
import qualified Engine.Data.FRP as F

type Cell = (Int, Int)

data Dir = U | D | L | R deriving (Eq, Show)
data Snake = Snake { body :: [Cell], dir :: Dir } deriving (Eq, Show)

stepSnake :: Snake -> Snake
stepSnake (Snake cells d) =
  let (x, y) = head cells
      head' = case d of
        U -> (x, y + 1)
        D -> (x, y - 1)
        L -> (x - 1, y)
        R -> (x + 1, y)
  in Snake (head' : init cells) d

applyTurns :: F.Events Dir -> Snake -> Snake
applyTurns evs s =
  case reverse evs of
    [] -> s
    (d : _) -> s { dir = d }

snake :: Snake -> F.Step (F.Events (Snake -> Snake)) Snake
snake = F.acc

tick :: F.Events Dir -> F.Events (Snake -> Snake)
tick evs = [stepSnake . applyTurns evs]

-- Usage (per tick, supply direction events):
-- F.run (snake (Snake [(0,0),(0,-1),(0,-2)] R))
--   [ (1, tick [U])
--   , (1, tick [])
--   , (1, tick [L])
--   ]
```

### 3) Sudoku (pure data logic)

```haskell
type Cell = (Int, Int)
type Value = Int
type Board = [(Cell, Value)]

row :: Cell -> Int
row (r, _) = r

col :: Cell -> Int
col (_, c) = c

box :: Cell -> (Int, Int)
box (r, c) = (r `div` 3, c `div` 3)

valid :: Board -> Bool
valid b =
  and
    [ unique [v | ((r, _), v) <- b, r == r0] | r0 <- [0..8] ]
  && and
    [ unique [v | ((_, c), v) <- b, c == c0] | c0 <- [0..8] ]
  && and
    [ unique [v | (cell, v) <- b, box cell == (br, bc)] | br <- [0..2], bc <- [0..2] ]
  where
    unique xs = length xs == length (nub xs)
    nub [] = []
    nub (x:xs) = x : nub (Prelude.filter (/= x) xs)

place :: Cell -> Value -> Board -> Board
place cell v b = (cell, v) : Prelude.filter (\(c, _) -> c /= cell) b
```

### 4) Tetris (piece drop sketch)

```haskell
type Cell = (Int, Int)

data Piece = Piece { cells :: [Cell] } deriving (Eq, Show)
data Tetris = Tetris { fixed :: [Cell], falling :: Piece } deriving (Eq, Show)

down :: Piece -> Piece
down (Piece cs) = Piece [ (x, y - 1) | (x, y) <- cs ]

blocked :: [Cell] -> Piece -> Bool
blocked fixed (Piece cs) =
  any (\(_, y) -> y < 0) cs || any (`elem` fixed) cs

stepTetris :: Tetris -> Tetris
stepTetris (Tetris fixed p) =
  let p' = down p
  in if blocked fixed p'
        then Tetris (cells p ++ fixed) p  -- lock piece (simplified)
        else Tetris fixed p'
```

### 5) Breakout (ball + bricks)

```haskell
data Brick = Brick { bpos :: (Double, Double) } deriving (Eq, Show)
data BallS = BallS { bpos' :: (Double, Double), bvel :: (Double, Double) } deriving (Eq, Show)

hit :: BallS -> Brick -> Bool
hit (BallS (x, y) _) (Brick (bx, by)) =
  abs (x - bx) < 1 && abs (y - by) < 1

stepBreakout :: [Brick] -> BallS -> ([Brick], BallS)
stepBreakout bricks ball =
  let bricks' = Prelude.filter (not . hit ball) bricks
  in (bricks', ball)
```

### 6) Asteroids (ship + bullets)

```haskell
data Ship = Ship { spos :: (Double, Double), svel :: (Double, Double) } deriving (Eq, Show)
data Bullet = Bullet { bpos :: (Double, Double), bvel :: (Double, Double) } deriving (Eq, Show)

-- Time-based movement (no explicit dt): position = p0 + v * t
advance :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
advance t (x, y) (vx, vy) = (x + vx * t, y + vy * t)

stepShip :: Ship -> F.Step () Ship
stepShip s0 = do
  t <- F.time
  let Ship p v = s0
  pure (Ship (advance t p v) v)

-- Bullet lives for 2 seconds, then disappears.
stepBullet :: Bullet -> F.Step () (Maybe Bullet)
stepBullet b0 = do
  u <- F.progress (0, 2)  -- 0..1 over 2 seconds
  let Bullet p v = b0
      t = u * 2
  pure (if u >= 1 then Nothing else Just (Bullet (advance t p v) v))

-- Program form:
-- shipProg = S.program (S.handle 0) $ do
--   ship <- S.step @Ship (stepShip s0) ()
--   ...
-- bulletProg = S.program (S.handle 1) $ do
--   b <- S.step @Bullet (stepBullet b0) ()
--   ...
```

### 7) Flappy (simple physics + threshold)

```haskell
data Bird = Bird { y :: Double, vy :: Double } deriving (Eq, Show)

flap :: Bird -> Bird
flap b = b { vy = 5 }

gravity :: Double -> Bird -> Bird
gravity dt (Bird y0 vy0) =
  let vy1 = vy0 - 9.8 * dt
      y1 = y0 + vy1 * dt
  in Bird y1 vy1
```

### 8) Minesweeper (board reveal)

```haskell
type Cell = (Int, Int)
type Mine = Cell
type Board = ([Mine], [Cell])  -- mines, revealed

neighbors :: Cell -> [Cell]
neighbors (x, y) =
  [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0) ]

reveal :: Cell -> Board -> Board
reveal c (mines, rev) =
  if c `elem` rev then (mines, rev) else (mines, c : rev)
```

### 9) Sokoban (grid push)

```haskell
type Cell = (Int, Int)
data Soko = Soko { player :: Cell, boxes :: [Cell] } deriving (Eq, Show)

move :: (Int, Int) -> Cell -> Cell
move (dx, dy) (x, y) = (x + dx, y + dy)

push :: (Int, Int) -> Soko -> Soko
push d s =
  let p' = move d (player s)
      boxes' =
        if p' `elem` boxes s
          then move d p' : Prelude.filter (/= p') (boxes s)
          else boxes s
  in s { player = p', boxes = boxes' }
```

### 10) Platformer (jump + gravity)

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Input as I

data Pos = Pos Double Double deriving (Eq, Show)
data Vel = Vel Double Double deriving (Eq, Show)
data Player = Player deriving (Eq, Show)
data OnGround = OnGround deriving (Eq, Show)

data MoveQ = MoveQ { pos :: Pos, vel :: Vel }
  deriving (Generic)

instance E.Queryable C MoveQ

data JumpQ = JumpQ { vel :: Vel, ground :: E.Has OnGround }
  deriving (Generic)

instance E.Queryable C JumpQ

data Gravity
data Move
data Jump

gravityProg :: Program I.Input
gravityProg = S.program (S.handle 0) $ do
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.comp @Vel) $ \(Vel vx vy) ->
      S.set (Vel vx (vy - 9.8 * dt))
  pure ()

moveProg :: Program I.Input
moveProg = S.program (S.handle 1) $ do
  dt <- S.dt
  _ <- S.batch $ do
    S.each (E.query @MoveQ) $ \(MoveQ (Pos x y) (Vel vx vy)) ->
      S.set (Pos (x + vx * dt) (y + vy * dt))
  pure ()

jumpProg :: Program I.Input
jumpProg = S.program (S.handle 2) $ do
  _ <- S.await (I.justPressed (I.Button "jump"))
  _ <- S.batch $ do
    S.each (E.query @JumpQ) $ \(JumpQ (Vel vx _) _) ->
      S.set (Vel vx 12) <> S.del @OnGround
  pure ()

platGraph :: Graph I.Input
platGraph = S.graph gravityProg moveProg jumpProg
```

### 11) Vampire Survivors (waves + enemy pool)

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Msg = SpawnEnemy | Reward deriving (Eq, Show)
data Enemy = Enemy deriving (Eq, Show)
data HP = HP Int deriving (Eq, Show)
data Pos = Pos Double Double deriving (Eq, Show)
data SpawnTick

spawnTickProg :: Program Msg
spawnTickProg = S.program (S.handle 0) $ do
  evs <- S.step @SpawnTick (F.every 0.5) ()
  S.send (map (const SpawnEnemy) evs)

data PoolQ = PoolQ { free :: E.Not Enemy }
  deriving (Generic)

instance E.Queryable C PoolQ

spawnProg :: Program Msg
spawnProg = S.program (S.handle 1) $ do
  spawns <- S.awaitEvent (== SpawnEnemy)
  pool <- S.batch $ do
    S.collect (E.query @PoolQ)
  let targets = take (length spawns) pool
      p = foldMap (\(e, _) ->
            S.setAt e Enemy <> S.setAt e (HP 5) <> S.setAt e (Pos 0 0)
          ) targets
  S.world p
```

### 12) RPG (quest state + rewards)

```haskell
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S

data Msg = TalkNPC | BossDown | GiveReward deriving (Eq, Show)
data Quest = NotStarted | InProgress | Complete deriving (Eq, Show)

data QuestStart
data QuestFinish

questStartProg :: Program Msg
questStartProg = S.program (S.handle 0) $ do
  _ <- S.awaitEvent (== TalkNPC)
  _ <- S.batch $ do
    S.each (E.comp @Quest) $ \q ->
      case q of
        NotStarted -> S.set InProgress
        _ -> mempty

questFinishProg :: Program Msg
questFinishProg = S.program (S.handle 1) $ do
  _ <- S.awaitEvent (== BossDown)
  _ <- S.batch $ do
    S.each (E.comp @Quest) $ \q ->
      case q of
        InProgress -> S.set Complete
        _ -> mempty
  S.send [GiveReward]
```

---

## Notes

- Everything is pure. Effects are modeled as data flowing out of steps.
- `Events` are just lists per tick; use `pure []`, `(<>)`, and `filter`.
- For name clarity, prefer qualified imports as shown at the top.

---

## Pain points (current)

- No built-in IO/async runtime: `Job`/`Events` are data only; you need an external executor.
- Program graph runs sequentially; parallelism is not automatic.
- Patch conflicts are resolved by list order; no merge policy beyond `Semigroup` order.
- Waiting programs are re-run within a frame; misbehaving programs can loop forever.
- `QueryableSum` skips signature pruning; sum queries scan all entities.
- Queries are applicative; no dependent queries without giving up pruning.
- No spatial index; collision queries are naive unless you build your own structure.
- `Events` are plain lists: no timestamps, priorities, or guaranteed ordering across programs.
- Long-running `Step`s require manual state hygiene; no built-in garbage collection for events.
- Coroutine programs aren’t serializable as data; mid-flight saves require input logging/replay.
- Relative timers are event-driven; there’s no first-class clock scaling/pausing per program.
- No prefab/archetype registry; bundles are just ad‑hoc values without tooling.
- `progress` clamps; `range` is windowed—users need to learn the distinction.
