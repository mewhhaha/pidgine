# redatared

Small, pure data layer for game engines with a minimal FRP core and a tiny ECS.
The API is intentionally simple and favors short, single‑word names.

Suggested imports (to avoid name clashes with Prelude):

```haskell
import qualified Engine.Data.FRP as F
import qualified Engine.Data.ECS as E
import qualified Engine.Data.System as S
import qualified Engine.Data.Input as I
import qualified Engine.Data.Transform as T
```

Note: Some examples use `do` with Applicative only; enable `ApplicativeDo`.

## FRP: Practical examples

These `Step`s can run standalone via `F.run` or inside systems via `S.step`.

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

### 2d) Step inside System (composition)

```haskell
-- Run a Step inside a System (state is kept per-system).
data Pos = Pos Double
data WiggleTween

wiggleSys :: S.System msg
wiggleSys = S.system @WiggleTween $ do
  let tweenX = F.tween (F.Span 0 1) easeInOut (lerp (-2) 2)
  x <- S.step @WiggleTween (F.sample tweenX) ()
  S.each (E.comp @Pos) (\_ -> Pos x)
```

Note: `S.step @Key` stores Step state under a type key. Use a unique key per independent Step.
`S.stepE @Key` stores per‑entity Step state; `S.eachM @Key` stores per‑entity coroutine state.
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

waitJump :: S.System I.Input
waitJump = S.system @Jump $ do
  _ <- S.await (I.justPressed (I.Button "jump"))
  pure ()

waitDash :: S.System I.Input
waitDash = S.system @Dash $ do
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

### 1) Applicative steps (parallel systems)

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
q :: E.Query (Pos, Maybe Vel)
q = do
  p <- E.comp
  v <- E.opt
  pure (p, v)
```

### 3) Patches as Monoid (merge systems)

```haskell
data Game
data Physics
data AI

physics :: F.Step () S.Patch
physics = pure (S.patch id)

ai :: F.Step () S.Patch
ai = pure (S.patch id)

gameSys :: S.System msg
gameSys = S.system @Game $ do
  p <- S.step @Physics physics ()
  a <- S.step @AI ai ()
  S.edit (p <> a)
```

---

## ECS: Practical examples

These examples use direct `E.spawn` calls. It keeps world building explicit and easy to reason about.

### 1) Spawn and access components

```haskell
{-# LANGUAGE TypeApplications #-}

data Pos = Pos Double Double deriving (Show, Eq)
data Vel = Vel Double Double deriving (Show, Eq)
data Player = Player deriving (Show, Eq)
data Enemy = Enemy deriving (Show, Eq)

(e, w1) = E.spawn (Pos 0 0, Vel 1 0) E.empty

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
posQ :: E.Query Pos
posQ = E.comp

E.runq posQ w2
-- [(Entity 0, Pos 10 5)]
```

### 3) Join two components

```haskell
pairQ :: E.Query (Pos, Vel)
pairQ = do
  p <- E.comp
  v <- E.comp
  pure (p, v)

E.runq pairQ w2
-- [(Entity 0, (Pos 10 5, Vel 1 0))]
```

### 4) Optional component

```haskell
optQ :: E.Query (Pos, Maybe Vel)
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
  } deriving (Generic, E.Queryable)

data PlayerMaybe = PlayerMaybe
  { player :: E.Has Player
  , position :: Maybe Position
  } deriving (Generic, E.Queryable)

data NotEnemy = NotEnemy
  { enemy :: E.Not Enemy
  , position :: Position
  } deriving (Generic, E.Queryable)

players = E.runq (E.query @PlayerQuery) w
```

Queryable auto-derives for product types. For sum types, use `QueryableSum`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

data Who = P Player | E Enemy
  deriving (Generic, E.QueryableSum)

whoQ = E.querySum @Who
```

Sum queries use `<|>` and therefore skip `need` pruning.

### 6) Filter and map queries

```haskell

fastQ :: E.Query Vel
fastQ = E.filterQ (\(Vel x y) -> x*x + y*y > 1) E.comp

namesQ :: E.Query String
namesQ = do
  p <- E.comp
  pure (case p of Pos x y -> show (x, y))
```

### 7) Fold without building lists

```haskell
count :: Int
count = E.foldq (E.comp :: E.Query Pos) (\_ _ n -> n + 1) 0 w2
```

### 8) Update a world in place

```haskell
move :: Double -> E.World -> E.World
move dt w =
  E.foldq (do p <- (E.comp :: E.Query Pos); v <- (E.comp :: E.Query Vel); pure (p, v))
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

## Systems: FRP + ECS together (async list)

Note: system tags use `TypeApplications` (e.g. `system @Move` or `system @(Move, Render)`).
`System` is an alias for `SystemV msg ()` (a system returning `()`); if you want
to `await` a value, use `SystemV msg a`.

Systems emit `Patch` values (using `S.set`, `S.update`, `S.del`, `S.put`, `S.each`, etc). Each system has a
type tag id (via `TypeApplications`, e.g. `system @Move`). Tags are labels used by `await`:
if multiple systems share a tag, `await` will unblock when any of them runs. Tuple tags
register multiple labels at once, so `system @(Move, Render)` can be used as an `await`
handle for “either Move or Render”. If a tag is produced by multiple systems, the
latest in‑frame value wins.
The runtime
re-runs only systems that return `wait` (directly or via `await` gating) until no
new messages appear, so `await` can be satisfied within the same frame. The returned outbox is the
aggregate of all messages emitted this frame.
When an `await` is not ready, the rest of that system does not run for the tick.
Systems are coroutines: they resume at the suspended `await` on later frames,
and restart from the top once they return.

For “gathered” systems, use `system`: it batches `each`/`edit`/`send` operations and
applies them once per tick, while `await` provides gating without manual `wait`.
Use `sysP`/`sysE` only for low‑level patch or stateful `Step` systems.

For per‑entity continuations, use `eachM @Key` with `stepE @Key` inside. This gives
each entity its own time/state machine without storing Steps in components.
`eachM` resumes the per‑entity program; if you need fresh data mid‑run, query inside
the program (or use `stepE` with the data as input).

Graph construction is variadic: `S.graph sys1 sys2 sys3` (no list needed).

Within a single `system`, all `each`/`collect` calls read the same world snapshot;
their patches are merged and applied once. If you need “see the result of a
previous update,” split it into another system and `await x`.

```haskell
data Move

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic, E.Queryable, S.Eachable)

moveSys :: S.System ()
moveSys = S.system @Move $ do
  dt <- S.dt
  S.each (E.query @MoveQ) (\q ->
    q { pos = let Pos x y = pos q
                      Vel vx vy = vel q
              in Pos (x + vx * dt) (y + vy * dt)
      })
  pure ()

g0 :: S.Graph ()
g0 = S.graph moveSys

runFrame :: F.DTime -> E.World -> S.Graph () -> (E.World, S.Graph ())
runFrame dt w g =
  let (w', _out, g') = S.run dt w [] g
  in (w', g')
```

Note: Each round can be parallelized by evaluating all runnable systems
against the same snapshot/inbox and then merging patches in list order.
The current `run` executes sequentially for simplicity.

Patch helpers also support update-in-place:

```haskell
data Score = Score Int

bumpScore :: E.Entity -> S.Patch
bumpScore e = S.update e (\(Score n) -> Score (n + 1))

resetScore :: E.Entity -> S.Patch
resetScore e = S.update e (const (Score 0)) -- or S.set e (Score 0)
```

---

## System patterns

### 1) Independent systems in one frame

```haskell
data Move
data AI
data Anim

moveSys :: S.System ()
moveSys = S.system @Move (pure ())

aiSys :: S.System ()
aiSys = S.system @AI (pure ())   -- placeholder

animSys :: S.System ()
animSys = S.system @Anim (pure ()) -- placeholder

g0 :: S.Graph ()
g0 = S.graph moveSys aiSys animSys
```

### 2) Deterministic patch order

```haskell
data Physics
data Collision
data Damage

physicsSys :: S.System ()
physicsSys = S.system @Physics (pure ())

collisionSys :: S.System ()
collisionSys = S.system @Collision (pure ())  -- resolve contacts

damageSys :: S.System ()
damageSys = S.system @Damage (pure ())

g0 :: S.Graph ()
g0 = S.graph physicsSys aiSys collisionSys damageSys animSys
```

### 3) Snapshot semantics

```haskell
-- All systems read the same snapshot; list order only affects patch application.
data WritePos
data ReadPos

writePos :: S.System ()
writePos = S.system @WritePos $ do
  S.each (E.comp :: E.Query Pos) (\(Pos x y) -> Pos (x + 1) y)
  pure ()

readPos :: S.System ()
readPos = S.system @ReadPos (pure ()) -- build a spatial index

g0 :: S.Graph ()
g0 = S.graph writePos readPos
```

### 4) Messaging between systems

```haskell
data Msg = Fire | SpawnBullet deriving (Eq, Show)
data FireSys
data SpawnSys

fireSys :: S.System Msg
fireSys = S.system @FireSys $ do
  _fires <- S.await (S.Event (== Fire))
  S.send [SpawnBullet]

spawnSys :: S.System Msg
spawnSys = S.system @SpawnSys $ do
  spawns <- S.await (S.Event (== SpawnBullet))
  let p = foldMap (const (S.set e Bullet)) spawns  -- assume e exists
  S.edit p
  pure ()

-- `run` already feeds outbox back into inbox within the frame (bounded).
-- The returned outbox is the aggregate of all messages emitted this frame.
```

### 4b) Movement waits for velocity update

This pattern lets you write systems without rigid ordering: a system can
use `await` to gate until another system has run in‑frame, and `run` will re‑run it in‑frame.

```haskell
data Vel = Vel Double Double
data Pos = Pos Double Double

data Speed
data Move

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic, E.Queryable, S.Eachable)

speedSys :: S.System ()
speedSys = S.system @Speed $ do
  S.each (E.query @MoveQ) (\q ->
    q { vel = let Vel vx vy = vel q in Vel (vx * 1.1) (vy * 1.1) }
    )
  pure ()

moveSys :: S.System ()
moveSys = S.system @Move $ do
  _ready <- S.await speedSys
  dt <- S.dt
  S.each (E.query @MoveQ) (\q ->
    q { pos = let Pos x y = pos q
                      Vel vx vy = vel q
                  in Pos (x + vx * dt) (y + vy * dt)
      }
    )
  pure ()

g0 :: S.Graph ()
g0 = S.graph moveSys speedSys  -- order doesn’t matter: moveSys waits
```

### 4c) Await a system value

`await` returns the value produced by the system (for its last in‑frame run).
Use `SystemV` for non‑`()` values; `graph` accepts any mix of return types.

```haskell
data Speed
data Move

speedSys :: S.SystemV () Double
speedSys = S.system @Speed $ pure 2.5

moveSys :: S.System ()
moveSys = S.system @Move $ do
  speed <- S.await speedSys
  dt <- S.dt
  S.each (E.comp :: E.Query Pos) (\(Pos x y) -> Pos (x + speed * dt) y)
  pure ()

g0 :: S.Graph ()
g0 = S.graph speedSys moveSys
```

Another common pattern is “compute collisions once, then consume them”:

```haskell
data Collide
data Damage

data Collision = Collision Entity Entity

collideSys :: S.SystemV () [Collision]
collideSys = S.system @Collide $ do
  pairs <- S.collect (E.query @PairQ)  -- PairQ omitted for brevity
  pure (detect pairs)                 -- detect :: [(Entity, PairQ)] -> [Collision]

damageSys :: S.System ()
damageSys = S.system @Damage $ do
  collisions <- S.await collideSys
  let p = foldMap applyDamage collisions
  S.edit p
  pure ()

g1 :: S.Graph ()
g1 = S.graph collideSys damageSys
```

### 4d) Collect after sync point

`await Update` waits until all systems have run once in the frame, then `collect` returns a snapshot.

```haskell
data EnemyQ = EnemyQ
  { enemy :: Enemy
  , posE :: Pos
  } deriving (Generic, E.Queryable)

data CountEnemies

countEnemies :: S.System ()
countEnemies = S.system @CountEnemies $ do
  _ <- S.await S.Update
  enemies <- S.collect (E.query @EnemyQ)
  let n = length enemies
  pure ()
```

### 4e) Await any of multiple tags

You can create an `await` handle with tuple tags. This lets you wait for
“either X or Y” without adding an extra system to the graph.

```haskell
data Move
data Render
data WaitAny

moveSys :: S.System ()
moveSys = S.system @Move (pure ())

renderSys :: S.System ()
renderSys = S.system @Render (pure ())

moveOrRender :: S.System ()
moveOrRender = S.system @(Move, Render) (pure ()) -- handle only (don’t add to graph)

waitAnySys :: S.System ()
waitAnySys = S.system @WaitAny $ do
  _ <- S.await moveOrRender
  pure ()
```

### 4f) Await a group of systems

Tag multiple systems with the same type and wait for the whole group:

```haskell
data Physics
data Move
data Collide
data Sync

moveSys :: S.System ()
moveSys = S.group @Physics $ S.system @Move $ do
  -- physics integration
  pure ()

collideSys :: S.System ()
collideSys = S.group @Physics $ S.system @Collide $ do
  -- collision resolution
  pure ()

syncSys :: S.System ()
syncSys = S.system @Sync $ do
  _ <- S.await (S.Group @Physics)
  -- all physics systems have run this frame
  pure ()
```

### 5) Task queue as data (async runtime)

```haskell
data Job = LoadTex String | PlaySound String

jobsSys :: S.System I.Input
jobsSys = S.system @Jobs $ do
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

-- System form (sample a tween with system time)
-- assume Pos component
-- tweenSys = S.system @Tween $ do
--   x <- S.sample (F.tween (F.Span 0 duration) id (lerp 0 target))
--   S.each (E.comp @Pos) (\_ -> Pos x)
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

-- System form:
-- spriteSys = S.system @Sprite $ do
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
collisions :: E.World -> [(E.Entity, E.Entity)]
collisions w =
  pairs
    & Prelude.filter (\((e1,p1,s1),(e2,p2,s2)) -> aabb p1 s1 p2 s2)
    & Prelude.map (\((e1,_,_),(e2,_,_)) -> (e1, e2))
  where
    (&) = flip ($)
    xs =
      E.foldq (do p <- (E.comp :: E.Query Pos); s <- (E.comp :: E.Query Size); pure (p, s))
        (\e (p, s) acc -> (e, p, s) : acc) [] w
    pairs = [(a,b) | (i,a) <- zip [0..] xs, (j,b) <- zip [0..] xs, i < j]
```

### 4) Parallelism (compose independent systems)

```haskell
physicsSys :: S.System msg
physicsSys = S.system @Physics (pure ())

aiSys :: S.System msg
aiSys = S.system @AI (pure ())

gameGraph :: S.Graph msg
gameGraph = S.graph physicsSys aiSys
```

### 5) Fixed timestep accumulator

```haskell
-- Convert variable dt to fixed 60Hz steps.
fixed60 :: F.Step a (F.Events ())
fixed60 = F.every (1 / 60)

-- System form:
-- fixedSys = S.system @Fixed $ do
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

System form:

```haskell
data EnemyAIKey
data EnemyLoop

data EnemyQ = EnemyQ { sense :: Sense }
  deriving (Generic, E.Queryable)

enemySys :: S.System msg
enemySys = S.system @EnemyLoop $ do
  S.eachM @EnemyLoop (E.query @EnemyQ) $ \e q -> do
    st <- S.stepE @EnemyAIKey e enemy (sense q)
    S.edit (S.set e st)
```

Tip: for per‑entity time, use `S.stepE @Key e F.time ()` rather than `S.time`.

---

## Mini‑games (practical sketches)

The examples below are intentionally small and practical. They show how you can
structure a game loop using `System` (Step‑style time), `World`, and `Query`.

### 1) Pong (ECS + FRP)

```haskell
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.System as S

data Pos = Pos Double Double deriving (Show, Eq)
data Vel = Vel Double Double deriving (Show, Eq)
data Paddle = Paddle deriving (Show, Eq)
data Ball = Ball deriving (Show, Eq)

initWorld :: E.World
initWorld =
  let (_, w1) = E.spawn (Paddle, Pos (-8) 0) E.empty
      (_, w2) = E.spawn (Paddle, Pos 8 0) w1
      (_, w3) = E.spawn (Ball, Pos 0 0, Vel 5 2) w2
  in w3

data Move
data Bounce

data MoveQ = MoveQ
  { pos :: Pos
  , vel :: Vel
  } deriving (Generic, E.Queryable, S.Eachable)

moveSys :: S.System ()
moveSys = S.system @Move $ do
  dt <- S.dt
  S.each (E.query @MoveQ) (\q ->
    q { pos = let Pos x y = pos q
                      Vel vx vy = vel q
              in Pos (x + vx * dt) (y + vy * dt)
      })
  pure ()

bounceSys :: S.System ()
bounceSys = S.system @Bounce $ do
  _ <- S.await moveSys
  S.each (E.query @MoveQ) (\q ->
    q { vel = let Pos _ y = pos q
                      Vel vx vy = vel q
                  in Vel vx (if y > 9 || y < (-9) then -vy else vy)
      })
  pure ()

pongGraph0 :: S.Graph ()
pongGraph0 = S.graph moveSys bounceSys

frame :: F.DTime -> E.World -> S.Graph () -> (E.World, S.Graph ())
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

-- System form:
-- shipSys = S.system @Ship $ do
--   ship <- S.step @Ship (stepShip s0) ()
--   ...
-- bulletSys = S.system @Bullet $ do
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
import qualified Engine.Data.System as S
import qualified Engine.Data.Input as I

data Pos = Pos Double Double deriving (Eq, Show)
data Vel = Vel Double Double deriving (Eq, Show)
data Player = Player deriving (Eq, Show)
data OnGround = OnGround deriving (Eq, Show)

data MoveQ = MoveQ { pos :: Pos, vel :: Vel }
  deriving (Generic, E.Queryable)

data JumpQ = JumpQ { vel :: Vel, ground :: E.Has OnGround }
  deriving (Generic, E.Queryable)

data Gravity
data Move
data Jump

gravitySys :: S.System I.Input
gravitySys = S.system @Gravity $ do
  dt <- S.dt
  S.each (E.comp @Vel) $ \(Vel vx vy) ->
    Vel vx (vy - 9.8 * dt)

moveSys :: S.System I.Input
moveSys = S.system @Move $ do
  dt <- S.dt
  S.eachP (E.query @MoveQ) $ \e (MoveQ (Pos x y) (Vel vx vy)) ->
    S.set e (Pos (x + vx * dt) (y + vy * dt))

jumpSys :: S.System I.Input
jumpSys = S.system @Jump $ do
  _ <- S.await (I.justPressed (I.Button "jump"))
  S.eachP (E.query @JumpQ) $ \e (JumpQ (Vel vx _) _) ->
    S.set e (Vel vx 12) <> S.del @OnGround e

platGraph :: S.Graph I.Input
platGraph = S.graph gravitySys moveSys jumpSys
```

### 11) Vampire Survivors (waves + enemy pool)

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.System as S

data Msg = SpawnEnemy | Reward deriving (Eq, Show)
data Enemy = Enemy deriving (Eq, Show)
data HP = HP Int deriving (Eq, Show)
data Pos = Pos Double Double deriving (Eq, Show)
data SpawnTick

spawnTickSys :: S.System Msg
spawnTickSys = S.system @SpawnTick $ do
  evs <- S.step @SpawnTick (F.every 0.5) ()
  S.send (map (const SpawnEnemy) evs)

data PoolQ = PoolQ { free :: E.Not Enemy }
  deriving (Generic, E.Queryable)

spawnSys :: S.System Msg
spawnSys = S.system @SpawnSys $ do
  spawns <- S.await (S.Event (== SpawnEnemy))
  pool <- S.collect (E.query @PoolQ)
  let targets = take (length spawns) pool
      p = foldMap (\(e, _) ->
            S.set e Enemy <> S.set e (HP 5) <> S.set e (Pos 0 0)
          ) targets
  S.edit p
```

### 12) RPG (quest state + rewards)

```haskell
{-# LANGUAGE TypeApplications #-}

import qualified Engine.Data.ECS as E
import qualified Engine.Data.System as S

data Msg = TalkNPC | BossDown | GiveReward deriving (Eq, Show)
data Quest = NotStarted | InProgress | Complete deriving (Eq, Show)

data QuestStart
data QuestFinish

questStartSys :: S.System Msg
questStartSys = S.system @QuestStart $ do
  _ <- S.await (S.Event (== TalkNPC))
  S.each (E.comp @Quest) $ \q ->
    case q of
      NotStarted -> InProgress
      _ -> q

questFinishSys :: S.System Msg
questFinishSys = S.system @QuestFinish $ do
  _ <- S.await (S.Event (== BossDown))
  S.each (E.comp @Quest) $ \q ->
    case q of
      InProgress -> Complete
      _ -> q
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
- Systems run sequentially; no automatic parallelism or dependency scheduling.
- Patch conflicts are resolved by list order; no merge policy beyond `Semigroup` order.
- Waiting systems are re-run within a frame; misbehaving systems can loop forever.
- `QueryableSum` skips `need` pruning; sum queries scan all entities.
- Queries are applicative; no dependent queries without giving up pruning.
- No spatial index; collision queries are naive unless you build your own structure.
- `Events` are plain lists: no timestamps, priorities, or guaranteed ordering across systems.
- Long-running `Step`s require manual state hygiene; no built-in garbage collection for events.
- Coroutine systems aren’t serializable as data; mid-flight saves require input logging/replay.
- Relative timers are event-driven; there’s no first-class clock scaling/pausing per system.
- No prefab/archetype registry; bundles are just ad‑hoc values without tooling.
- `progress` clamps; `range` is windowed—users need to learn the distinction.
