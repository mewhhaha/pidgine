# redatared

Small, pure data layer for game engines with a minimal FRP core and a tiny ECS.
The API is intentionally simple and favors short, single‑word names.

Suggested imports (to avoid name clashes with Prelude):

```haskell
import qualified Engine.Data.FRP as F
import qualified Engine.Data.ECS as E
import qualified Engine.Data.System as S
import qualified Engine.Data.Input as I
```

Note: Some examples use `do` with Applicative only; enable `ApplicativeDo`.

## FRP: Practical examples

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
jump = I.press (I.Btn "jump")

left :: F.Step I.Input Bool
left = I.held (I.Btn "left")

moveX :: F.Step I.Input Double
moveX = I.axis "x"

sample :: I.Input
sample = I.Input
  { I.down = [I.Btn "jump"]
  , I.up = []
  , I.holds = [I.Btn "left"]
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
physics :: F.Step I.Input S.Patch
physics = pure (S.patch id)

ai :: F.Step I.Input S.Patch
ai = pure (S.patch id)

game :: F.Step I.Input S.Patch
game = do
  p <- physics
  a <- ai
  pure (p <> a)

tick :: F.DTime -> I.Input -> E.World -> (E.World, F.Step I.Input S.Patch)
tick dt inp w =
  let (p, game') = F.stepS game dt inp
  in (S.apply p w, game')
```

---

## ECS: Practical examples

### 1) Spawn and access components

```haskell
{-# LANGUAGE TypeApplications #-}

data Pos = Pos Double Double deriving (Show, Eq)
data Vel = Vel Double Double deriving (Show, Eq)

(e, w1) = E.spawn [E.component (Pos 0 0), E.component (Vel 1 0)] E.empty

E.get e w1 :: Maybe Pos
-- Just (Pos 0 0)

w2 = E.set e (Pos 10 5) w1
E.get e w2 :: Maybe Pos
-- Just (Pos 10 5)

E.has @Vel e w2
-- True
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

Queryable auto-derives for product types; sums are supported via `<|>` but skip `need` pruning.

### 6) Filter and map queries

```haskell
import qualified Engine.Data.Filterable as W

fastQ :: E.Query Vel
fastQ = W.filter (\(Vel x y) -> x*x + y*y > 1) E.comp

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

---

## Systems: FRP + ECS together

```haskell
stepWorld :: Double -> E.World -> E.World
stepWorld dt w =
  E.foldq (do p <- (E.comp :: E.Query Pos); v <- (E.comp :: E.Query Vel); pure (p, v))
    (\e (Pos x y, Vel vx vy) acc ->
      E.set e (Pos (x + vx * dt) (y + vy * dt)) acc
    ) w w

sys :: S.WorldSys
sys = S.stepw stepWorld

runFrame :: F.DTime -> E.World -> E.World
runFrame dt w =
  let (w', _) = F.stepS sys dt w
  in w'
```

---

## Common scenarios

### 1) Animating any value (tween)

```haskell
-- Linear tween from 0 to target over duration seconds.
tween :: Double -> Double -> F.Step a Double
tween duration target = do
  t <- F.time
  let u = clamp (t / duration)
  pure (u * target)
  where
    clamp x = if x < 0 then 0 else if x > 1 then 1 else x

-- Example: animate from 0 to 10 over 2 seconds
-- F.run (tween 2 10) [(0.5, ()), (0.5, ()), (1.0, ())]
-- [2.5,5.0,10.0]
--
-- From/to tween:
tween2 :: Double -> Double -> Double -> F.Step a Double
tween2 duration from to = do
  t <- F.time
  let u = clamp (t / duration)
  pure (from + u * (to - from))
  where
    clamp x = if x < 0 then 0 else if x > 1 then 1 else x
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
physics :: F.Step I.Input S.Patch
physics = pure (S.patch id)

ai :: F.Step I.Input S.Patch
ai = pure (S.patch id)

game :: F.Step I.Input S.Patch
game = do
  p <- physics
  a <- ai
  pure (p <> a)
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
carry the next state. Time‑based transitions use `F.after`.

---

## Mini‑games (practical sketches)

The examples below are intentionally small and practical. They show how you can
structure a game loop using `Step`, `World`, and `Query`.

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
  let w0 = E.empty
      (p1, w1) = E.spawn [E.component Paddle, E.component (Pos (-8) 0)] w0
      (p2, w2) = E.spawn [E.component Paddle, E.component (Pos 8 0)] w1
      (_b, w3) = E.spawn [E.component Ball, E.component (Pos 0 0), E.component (Vel 5 2)] w2
  in w3

move :: Double -> E.World -> E.World
move dt w =
  E.foldq (do p <- (E.comp :: E.Query Pos); v <- (E.comp :: E.Query Vel); pure (p, v))
    (\e (Pos x y, Vel vx vy) acc ->
      E.set e (Pos (x + vx * dt) (y + vy * dt)) acc
    ) w w

bounceWalls :: E.World -> E.World
bounceWalls w =
  E.foldq (do p <- (E.comp :: E.Query Pos); v <- (E.comp :: E.Query Vel); pure (p, v))
    (\e (Pos x y, Vel vx vy) acc ->
      let vy' = if y > 9 || y < (-9) then -vy else vy
      in E.set e (Vel vx vy') acc
    ) w w

stepPong :: Double -> E.World -> E.World
stepPong dt = bounceWalls . move dt

pong :: S.WorldSys
pong = S.stepw stepPong

frame :: F.DTime -> E.World -> E.World
frame dt w =
  let (w', _) = F.stepS pong dt w
  in w'
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

advance :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
advance dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)

stepShip :: Double -> Ship -> Ship
stepShip dt (Ship p v) = Ship (advance dt p v) v

stepBullet :: Double -> Bullet -> Bullet
stepBullet dt (Bullet p v) = Bullet (advance dt p v) v
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

---

## Notes

- Everything is pure. Effects are modeled as data flowing out of steps.
- `Events` are just lists per tick; use `pure []`, `(<>)`, and `filter`.
- For name clarity, prefer qualified imports as shown at the top.
