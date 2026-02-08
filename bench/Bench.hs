{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Bits (bit, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Foldable as Foldable
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Pos = Pos {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Vel = Vel {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Acc = Acc {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Hp = Hp Int
  deriving (Eq, Show)

data C
  = CPos Pos
  | CVel Vel
  | CAcc Acc
  | CHp Hp
  deriving (Generic)

instance E.ComponentId C


type World = E.World C
type Program msg a = S.Program C msg a
type Graph msg = S.Graph C msg

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

reqPV :: E.Sig
reqPV =
  bit (E.componentBitOf @C @Pos)
    .|. bit (E.componentBitOf @C @Vel)

reqP :: E.Sig
reqP = bit (E.componentBitOf @C @Pos)

reqV :: E.Sig
reqV = bit (E.componentBitOf @C @Vel)

reqA :: E.Sig
reqA = bit (E.componentBitOf @C @Acc)

reqH :: E.Sig
reqH = bit (E.componentBitOf @C @Hp)

reqPVA :: E.Sig
reqPVA = reqPV .|. reqA

pongFieldHalfW :: Double
pongFieldHalfW = 50

pongFieldHalfH :: Double
pongFieldHalfH = 30

pongPaddleHalf :: Double
pongPaddleHalf = 8

pongPaddleSpeed :: Double
pongPaddleSpeed = 35

pongBallRadius :: Double
pongBallRadius = 1

pongPaddleX :: Double
pongPaddleX = pongFieldHalfW - 2

vampireFieldHalfW :: Double
vampireFieldHalfW = 120

vampireFieldHalfH :: Double
vampireFieldHalfH = 80

vampirePlayerSpeed :: Double
vampirePlayerSpeed = 30

vampireMobBaseSpeed :: Double
vampireMobBaseSpeed = 10

vampireDamageRadius :: Double
vampireDamageRadius = 4

forceEachm :: World -> Double
forceEachm w =
  Foldable.foldl'
    (\acc (E.EntityRow _ _ bag) ->
      let sumPos =
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> x + y
              Nothing -> 0
          sumVel =
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> vx + vy
              Nothing -> 0
      in acc + sumPos + sumVel
    )
    0
    (E.matchingRows reqPV 0 w)

forceEachm3 :: World -> Double
forceEachm3 w =
  Foldable.foldl'
    (\acc (E.EntityRow _ _ bag) ->
      let sumPos =
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> x + y
              Nothing -> 0
          sumVel =
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> vx + vy
              Nothing -> 0
          sumAcc =
            case E.bagGet @C @Acc bag of
              Just (Acc ax ay) -> ax + ay
              Nothing -> 0
      in acc + sumPos + sumVel + sumAcc
    )
    0
    (E.matchingRows reqPVA 0 w)

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

forceSplitMatched :: World -> Double
forceSplitMatched w =
  let sumPos =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> acc + x + y
              Nothing -> acc
          )
          0
          (E.matchingRows reqP 0 w)
      sumVel =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> acc + vx + vy
              Nothing -> acc
          )
          0
          (E.matchingRows reqV 0 w)
  in sumPos + sumVel

forceSplit4Matched :: World -> Double
forceSplit4Matched w =
  let sumPos =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> acc + x + y
              Nothing -> acc
          )
          0
          (E.matchingRows reqP 0 w)
      sumVel =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> acc + vx + vy
              Nothing -> acc
          )
          0
          (E.matchingRows reqV 0 w)
      sumAcc =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Acc bag of
              Just (Acc ax ay) -> acc + ax + ay
              Nothing -> acc
          )
          0
          (E.matchingRows reqA 0 w)
      sumHp =
        Foldable.foldl'
          (\acc (E.EntityRow _ _ bag) ->
            case E.bagGet @C @Hp bag of
              Just (Hp hp) -> acc + fromIntegral hp
              Nothing -> acc
          )
          0
          (E.matchingRows reqH 0 w)
  in sumPos + sumVel + sumAcc + sumHp

forceSplitFullScan :: World -> Double
forceSplitFullScan w =
  let sumPos =
        E.foldEntities
          (\_ _ bag acc ->
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> acc + x + y
              Nothing -> acc
          )
          0
          w
      sumVel =
        E.foldEntities
          (\_ _ bag acc ->
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> acc + vx + vy
              Nothing -> acc
          )
          0
          w
  in sumPos + sumVel

data MoveLoop
data MoveLoopSplitP
data MoveLoopSplitV
data MoveLoopSplitA
data MoveLoopSplitH
data PongBall
data PongPaddle
data VampirePlayer
data VampireMob

data Move3 = Move3
  { pos3 :: Pos
  , vel3 :: Vel
  , acc3 :: Acc
  } deriving (Generic)

instance E.Queryable C Move3

data BallInfo = BallInfo
  { ballPos :: Pos
  , ballVel :: Vel
  , ballAcc :: Acc
  , ballNoHp :: E.Not Hp
  } deriving (Generic)

instance E.Queryable C BallInfo

data PaddleInfo = PaddleInfo
  { paddlePos :: Pos
  , paddleVel :: Vel
  , paddleHp :: Hp
  , paddleNoAcc :: E.Not Acc
  } deriving (Generic)

instance E.Queryable C PaddleInfo

data PlayerInfo = PlayerInfo
  { playerPos :: Pos
  , playerVel :: Vel
  , playerHp :: Hp
  , playerNoAcc :: E.Not Acc
  } deriving (Generic)

instance E.Queryable C PlayerInfo

data NpcInfo = NpcInfo
  { npcPos :: Pos
  , npcVel :: Vel
  , npcAcc :: Acc
  , npcHp :: Hp
  } deriving (Generic)

instance E.Queryable C NpcInfo

buildWorld :: Int -> World
buildWorld n = snd (foldl' add (0 :: Int, E.emptyWorld) [1 .. n])
  where
    add (i, w) _ =
      let (_, w1) =
            E.spawn
              ( Pos (fromIntegral i) 0
              , Vel 1 1
              , Acc 0.1 0.1
              , Hp 100
              )
              w
      in (i + 1, w1)

buildWorldPong :: World
buildWorldPong =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Pos 0 0, Vel 35 12, Acc 0 0) w0
      (_, w2) = E.spawn (Pos (-pongPaddleX) 0, Vel 0 0, Hp (-1)) w1
      (_, w3) = E.spawn (Pos pongPaddleX 0, Vel 0 0, Hp 1) w2
  in w3

buildWorldVampire :: Int -> World
buildWorldVampire mobCount =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Pos 0 0, Vel vampirePlayerSpeed 12, Hp 100) w0
      addMob (i, w) _ =
        let x = fromIntegral (i `mod` 200) - 100
            y = fromIntegral (i `div` 200) - 25
            speed = fromIntegral ((i `mod` 7) + 4)
            hp = 5 + (i `mod` 9)
            (_, w1') = E.spawn (Pos x y, Vel 0 0, Acc speed 0, Hp hp) w
        in (i + 1, w1')
  in snd (foldl' addMob (0 :: Int, w1) [1 .. mobCount])

buildWorldPlayers :: Int -> Int -> World
buildWorldPlayers unrelatedCount playerCount =
  let addUnrelated w _ =
        let (_, w1) = E.spawn (Hp 100) w
        in w1
      addPlayer (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0, Vel 1 1) w
        in (i + 1, w1)
      w0 = foldl' addUnrelated E.emptyWorld [1 .. unrelatedCount]
  in snd (foldl' addPlayer (0 :: Int, w0) [1 .. playerCount])

buildWorldSplit :: Int -> Int -> World
buildWorldSplit posCount velCount =
  let addPos (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1) = E.spawn (Vel (fromIntegral i) 1) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, E.emptyWorld) [1 .. posCount]
  in snd (foldl' addVel (0 :: Int, w1) [1 .. velCount])

buildWorldSplit4 :: Int -> Int -> Int -> Int -> World
buildWorldSplit4 posCount velCount accCount hpCount =
  let addPos (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1) = E.spawn (Vel (fromIntegral i) 1) w
        in (i + 1, w1)
      addAcc (i, w) _ =
        let (_, w1) = E.spawn (Acc (fromIntegral i) 1) w
        in (i + 1, w1)
      addHp (i, w) _ =
        let (_, w1) = E.spawn (Hp (i + 100)) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, E.emptyWorld) [1 .. posCount]
      (_, w2) = foldl' addVel (0 :: Int, w1) [1 .. velCount]
      (_, w3) = foldl' addAcc (0 :: Int, w2) [1 .. accCount]
  in snd (foldl' addHp (0 :: Int, w3) [1 .. hpCount])

qBall :: E.Query C BallInfo
qBall = E.query @BallInfo @C

qPaddle :: E.Query C PaddleInfo
qPaddle = E.query @PaddleInfo @C

qPlayer :: E.Query C PlayerInfo
qPlayer = E.query @PlayerInfo @C

qMob :: E.Query C NpcInfo
qMob = E.query @NpcInfo @C

qPVQ :: E.Query C (Pos, Vel)
qPVQ = (,) <$> (E.comp :: E.Query C Pos) <*> (E.comp :: E.Query C Vel)

qPQ :: E.Query C Pos
qPQ = E.comp

qVQ :: E.Query C Vel
qVQ = E.comp

qAQ :: E.Query C Acc
qAQ = E.comp

qHQ :: E.Query C Hp
qHQ = E.comp

qMove3 :: E.Query C Move3
qMove3 = E.query @Move3 @C

advance :: Double -> Pos -> Vel -> (Pos, Vel)
advance dt (Pos x y) (Vel vx vy) =
  let ax = sin (x * 0.01) * 0.5
      ay = cos (y * 0.01) * 0.5
      vx' = vx + ax * dt
      vy' = vy + ay * dt
      x' = x + vx' * dt + sin vx'
      y' = y + vy' * dt + cos vy'
  in (Pos x' y', Vel vx' vy')

advanceVampirePlayer :: Double -> Pos -> Vel -> (Pos, Vel)
advanceVampirePlayer dt (Pos x y) (Vel vx vy) =
  let x1 = x + vx * dt
      y1 = y + vy * dt
      (vx', x2) =
        if x1 > vampireFieldHalfW
          then (-abs vx, vampireFieldHalfW)
          else if x1 < -vampireFieldHalfW
            then (abs vx, -vampireFieldHalfW)
            else (vx, x1)
      (vy', y2) =
        if y1 > vampireFieldHalfH
          then (-abs vy, vampireFieldHalfH)
          else if y1 < -vampireFieldHalfH
            then (abs vy, -vampireFieldHalfH)
            else (vy, y1)
  in (Pos x2 y2, Vel vx' vy')

advanceVampireMob :: Double -> Pos -> Vel -> Acc -> Pos -> Hp -> (Pos, Vel, Hp)
advanceVampireMob dt (Pos x y) _ (Acc ax _) (Pos px py) (Hp hp) =
  let dx = px - x
      dy = py - y
      dist2 = dx * dx + dy * dy + 1.0e-6
      invDist = 1 / sqrt dist2
      speed = vampireMobBaseSpeed + ax
      vx' = dx * invDist * speed
      vy' = dy * invDist * speed
      x' = x + vx' * dt
      y' = y + vy' * dt
      hp' =
        if dist2 < vampireDamageRadius * vampireDamageRadius
          then hp - 1
          else hp
  in (Pos x' y', Vel vx' vy', Hp hp')

movePatch :: (Pos, Vel) -> S.EntityPatch C
movePatch (Pos x y, Vel vx vy) =
  let p = Pos (x + vx) (y + vy)
      v = Vel vx vy
  in S.set p <> S.set v

moveSet2 :: (Pos, Vel) -> (Pos, Vel)
moveSet2 (Pos x y, Vel vx vy) =
  let p = Pos (x + vx) (y + vy)
      v = Vel vx vy
  in (p, v)

moveProgM :: Program String ()
moveProgM = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.each qPVQ $ \(p, v) ->
      let (p', v') = moveSet2 (p, v)
      in S.set p' <> S.set v'
  pure ()

moveProgMStateful :: Program String ()
moveProgMStateful = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachM @MoveLoop qPVQ $ \(Pos x y, Vel vx vy) -> do
      let p = Pos (x + vx) (y + vy)
      S.edit (S.set p <> S.set (Vel vx vy))
  pure ()

moveProgMPure :: Program String ()
moveProgMPure = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.each qPVQ movePatch
  pure ()

moveProgMJoin3 :: Program String ()
moveProgMJoin3 = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.each qMove3 $ \q ->
      let Pos x y = pos3 q
          Vel vx vy = vel3 q
          Acc ax ay = acc3 q
          vx' = vx + ax
          vy' = vy + ay
          p' = Pos (x + vx') (y + vy')
      in S.set p' <> S.set (Vel vx' vy')
  pure ()

moveProgMLogic :: Program String ()
moveProgMLogic = S.program (S.handle 0) $ do
  dt <- S.dt
  _ <- S.await $ computeS $
    S.eachM @MoveLoop qPVQ $ \(p, v) -> do
      let (p', v') = advance dt p v
      S.edit (S.set p' <> S.set v')
  pure ()

pongProg :: Program String ()
pongProg = S.program (S.handle 5) $ do
  dt <- S.dt
  paddles <- S.await $ computeS $ S.collect qPaddle
  balls <- S.await $ computeS $ S.collect qBall
  let (leftY, leftVy, rightY, rightVy) =
        foldl'
          (\(ly, lvy, ry, rvy) (_, PaddleInfo (Pos _ py) (Vel _ pvy) (Hp side) _) ->
            if side < 0
              then (py, pvy, ry, rvy)
              else (ly, lvy, py, pvy)
          )
          (0, 0, 0, 0)
          paddles
      ballY =
        case balls of
          (_, BallInfo (Pos _ y) _ _ _) : _ -> y
          _ -> 0
      maxDy = pongPaddleSpeed * dt
      minY = -pongFieldHalfH + pongPaddleHalf
      maxY = pongFieldHalfH - pongPaddleHalf
  _ <- S.await $ computeS $
    S.eachM @PongPaddle qPaddle $ \(PaddleInfo (Pos _ y) _ (Hp side) _) -> do
      let targetY = ballY
          dy = clamp (-maxDy) maxDy (targetY - y)
          y' = clamp minY maxY (y + dy)
          x' = if side < 0 then -pongPaddleX else pongPaddleX
          vy' = if dt > 0 then dy / dt else 0
      S.edit (S.set (Pos x' y') <> S.set (Vel 0 vy'))
  _ <- S.await $ computeS $
    S.eachM @PongBall qBall $ \(BallInfo (Pos x y) (Vel vx vy) _ _) -> do
      let x1 = x + vx * dt
          y1 = y + vy * dt
          (vyWall, yWall) =
            if y1 > pongFieldHalfH - pongBallRadius
              then (-abs vy, pongFieldHalfH - pongBallRadius)
              else if y1 < -pongFieldHalfH + pongBallRadius
                then (abs vy, -pongFieldHalfH + pongBallRadius)
                else (vy, y1)
          hitLeft = x1 <= (-pongPaddleX + pongBallRadius)
            && abs (yWall - leftY) <= pongPaddleHalf
          hitRight = x1 >= (pongPaddleX - pongBallRadius)
            && abs (yWall - rightY) <= pongPaddleHalf
          vxP =
            if hitLeft
              then abs vx
              else if hitRight
                then -abs vx
                else vx
          vyP =
            if hitLeft
              then vyWall + leftVy * 0.2
              else if hitRight
                then vyWall + rightVy * 0.2
                else vyWall
          xP =
            if hitLeft
              then -pongPaddleX + pongBallRadius
              else if hitRight
                then pongPaddleX - pongBallRadius
                else x1
          (vx2, x2) =
            if xP > pongFieldHalfW - pongBallRadius
              then (-abs vxP, pongFieldHalfW - pongBallRadius)
              else if xP < -pongFieldHalfW + pongBallRadius
                then (abs vxP, -pongFieldHalfW + pongBallRadius)
                else (vxP, xP)
      S.edit (S.set (Pos x2 yWall) <> S.set (Vel vx2 vyP))
  pure ()

vampireProg :: Program String ()
vampireProg = S.program (S.handle 7) $ do
  dt <- S.dt
  players <- S.await $ computeS $ S.collect qPlayer
  let (p0, v0) =
        case players of
          (_, PlayerInfo p v _ _) : _ -> (p, v)
          _ -> (Pos 0 0, Vel vampirePlayerSpeed 12)
      (p1, v1) = advanceVampirePlayer dt p0 v0
  _ <- S.await $ computeS $
    S.eachM @VampirePlayer qPlayer $ \_ -> do
      S.edit (S.set p1 <> S.set v1)
  _ <- S.await $ computeS $
    S.eachM @VampireMob qMob $ \(NpcInfo p v a hp) -> do
      let (p', v', hp') = advanceVampireMob dt p v a p1 hp
      S.edit (S.set p' <> S.set v' <> S.set hp')
  pure ()

-- Four eachM loops in one compute block: compiled to one fused kernel pass.
moveProgMSplit4 :: Program String ()
moveProgMSplit4 = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    (S.eachM @MoveLoopSplitP qPQ $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p))
    *>
    (S.eachM @MoveLoopSplitV qVQ $ \(Vel vx vy) -> do
      let v = Vel (vx + 1) vy
      S.edit (S.set v))
    *>
    (S.eachM @MoveLoopSplitA qAQ $ \(Acc ax ay) -> do
      let a = Acc (ax + 1) ay
      S.edit (S.set a))
    *>
    (S.eachM @MoveLoopSplitH qHQ $ \(Hp hp) -> do
      S.edit (S.set (Hp (hp + 1))))
  pure ()

moveProgMSplit4P :: Program String ()
moveProgMSplit4P = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachM @MoveLoopSplitP qPQ $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p)
  pure ()

moveProgMSplit4V :: Program String ()
moveProgMSplit4V = S.program (S.handle 1) $ do
  _ <- S.await $ computeS $
    S.eachM @MoveLoopSplitV qVQ $ \(Vel vx vy) -> do
      let v = Vel (vx + 1) vy
      S.edit (S.set v)
  pure ()

moveProgMSplit4A :: Program String ()
moveProgMSplit4A = S.program (S.handle 2) $ do
  _ <- S.await $ computeS $
    S.eachM @MoveLoopSplitA qAQ $ \(Acc ax ay) -> do
      let a = Acc (ax + 1) ay
      S.edit (S.set a)
  pure ()

moveProgMSplit4H :: Program String ()
moveProgMSplit4H = S.program (S.handle 3) $ do
  _ <- S.await $ computeS $
    S.eachM @MoveLoopSplitH qHQ $ \(Hp hp) -> do
      S.edit (S.set (Hp (hp + 1)))
  pure ()

graphEachM :: Graph String
graphEachM = S.graph @String moveProgM

graphEachMStateful :: Graph String
graphEachMStateful = S.graph @String moveProgMStateful

graphEachMJoin3 :: Graph String
graphEachMJoin3 = S.graph @String moveProgMJoin3

graphEachMLogic :: Graph String
graphEachMLogic = S.graph @String moveProgMLogic

graphEachMSplit4 :: Graph String
graphEachMSplit4 = S.graph @String moveProgMSplit4

graphEachMSplit4Programs :: Graph String
graphEachMSplit4Programs =
  S.graph @String moveProgMSplit4P moveProgMSplit4V moveProgMSplit4A moveProgMSplit4H

graphPong :: Graph String
graphPong = S.graph @String pongProg

graphVampire :: Graph String
graphVampire = S.graph @String vampireProg

runTickSplitFullScan :: World -> World
runTickSplitFullScan w0 =
  let stepPos _ _ bag =
        case E.bagGet @C @Pos bag of
          Just (Pos x y) -> E.bagSetDirect @C @Pos (Pos (x + 1) y) bag
          Nothing -> bag
      stepVel _ _ bag =
        case E.bagGet @C @Vel bag of
          Just (Vel vx vy) -> E.bagSetDirect @C @Vel (Vel (vx + 1) vy) bag
          Nothing -> bag
      w1 = E.mapEntities stepPos w0
  in E.mapEntities stepVel w1

runTickSplit4FullScan :: World -> World
runTickSplit4FullScan w0 =
  let stepPos _ _ bag =
        case E.bagGet @C @Pos bag of
          Just (Pos x y) -> E.bagSetDirect @C @Pos (Pos (x + 1) y) bag
          Nothing -> bag
      stepVel _ _ bag =
        case E.bagGet @C @Vel bag of
          Just (Vel vx vy) -> E.bagSetDirect @C @Vel (Vel (vx + 1) vy) bag
          Nothing -> bag
      stepAcc _ _ bag =
        case E.bagGet @C @Acc bag of
          Just (Acc ax ay) -> E.bagSetDirect @C @Acc (Acc (ax + 1) ay) bag
          Nothing -> bag
      stepHp _ _ bag =
        case E.bagGet @C @Hp bag of
          Just (Hp hp) -> E.bagSetDirect @C @Hp (Hp (hp + 1)) bag
          Nothing -> bag
      w1 = E.mapEntities stepPos w0
      w2 = E.mapEntities stepVel w1
      w3 = E.mapEntities stepAcc w2
  in E.mapEntities stepHp w3

graphEachMPure :: Graph String
graphEachMPure = S.graph @String moveProgMPure

main :: IO ()
main = defaultMain
  [ bgroup "ecs"
      [ bench "spawn-10k" $ nf (length . E.entities . buildWorld) 10000
      ]
  , bgroup "program"
      [ bgroup "10k"
          [ let w = buildWorld 10000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-stateful" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMStateful
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMLogic
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMJoin3
                in forceEachm3 w1
              ) w
          , let w = buildWorldSplit4 2500 2500 2500 2500
            in bench "eachm-split4" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSplit4
                in forceSplit4Matched w1
              ) w
          , let w = buildWorldSplit4 2500 2500 2500 2500
            in bench "eachm-split4-progs" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSplit4Programs
                in forceSplit4Matched w1
              ) w
          ]
      , bgroup "game"
          [ let w = buildWorldPong
            in bench "pong" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphPong
                in forceEachm w1
              ) w
          , let w = buildWorldVampire 10000
            in bench "vampire-10k" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphVampire
                in forceEachm w1
              ) w
          ]
      , bgroup "10k+1"
          [ let w = buildWorldPlayers 10000 1
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in forceEachm w1
              ) w
          , let w = buildWorldPlayers 10000 1
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in forceEachm w1
              ) w
          ]
      ]
  ]
