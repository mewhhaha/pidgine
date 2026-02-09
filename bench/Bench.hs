{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Bits (bit, (.|.))
import Data.List (foldl')
import qualified Data.Foldable as Foldable
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
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

type ProgramM msg a = S.ProgramM C msg a

type Graph msg = S.Graph C msg

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

reqPV :: E.Sig
reqPV =
  bit (E.componentBitOf @C @Pos)
    .|. bit (E.componentBitOf @C @Vel)

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

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

data BallInfo = BallInfo
  { ballPos :: Pos
  , ballVel :: Vel
  , ballAcc :: Acc
  , ballNoHp :: E.Not Hp
  } deriving (Generic)

data PaddleInfo = PaddleInfo
  { paddlePos :: Pos
  , paddleVel :: Vel
  , paddleHp :: Hp
  , paddleNoAcc :: E.Not Acc
  } deriving (Generic)

data PlayerInfo = PlayerInfo
  { playerPos :: Pos
  , playerVel :: Vel
  , playerHp :: Hp
  , playerNoAcc :: E.Not Acc
  } deriving (Generic)

data NpcInfo = NpcInfo
  { npcPos :: Pos
  , npcVel :: Vel
  , npcAcc :: Acc
  , npcHp :: Hp
  } deriving (Generic)

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

qBall :: E.Query C BallInfo
qBall = E.query @BallInfo @C

qPaddle :: E.Query C PaddleInfo
qPaddle = E.query @PaddleInfo @C

qPlayer :: E.Query C PlayerInfo
qPlayer = E.query @PlayerInfo @C

qMob :: E.Query C NpcInfo
qMob = E.query @NpcInfo @C

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

pongProg :: ProgramM String ()
pongProg = do
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
    S.eachM @PaddleInfo $ \(PaddleInfo (Pos _ y) _ (Hp side) _) -> do
      let targetY = ballY
          dy = clamp (-maxDy) maxDy (targetY - y)
          y' = clamp minY maxY (y + dy)
          x' = if side < 0 then -pongPaddleX else pongPaddleX
          vy' = if dt > 0 then dy / dt else 0
      S.edit (S.set (Pos x' y') <> S.set (Vel 0 vy'))
  _ <- S.await $ computeS $
    S.eachM @BallInfo $ \(BallInfo (Pos x y) (Vel vx vy) _ _) -> do
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

vampireProg :: ProgramM String ()
vampireProg = do
  dt <- S.dt
  players <- S.await $ computeS $ S.collect qPlayer
  let (p0, v0) =
        case players of
          (_, PlayerInfo p v _ _) : _ -> (p, v)
          _ -> (Pos 0 0, Vel vampirePlayerSpeed 12)
      (p1, v1) = advanceVampirePlayer dt p0 v0
  _ <- S.await $ computeS $
    S.eachM @PlayerInfo $ \_ -> do
      S.edit (S.set p1 <> S.set v1)
  _ <- S.await $ computeS $
    S.eachM @NpcInfo $ \(NpcInfo p v a hp) -> do
      let (p', v', hp') = advanceVampireMob dt p v a p1 hp
      S.edit (S.set p' <> S.set v' <> S.set hp')
  pure ()

graphPong :: Graph String
graphPong =
  S.graph $ do
    _ <- S.program pongProg
    pure ()

graphVampire :: Graph String
graphVampire =
  S.graph $ do
    _ <- S.program vampireProg
    pure ()

main :: IO ()
main = defaultMain
  [ bgroup "game"
      [ let w = buildWorldPong
        in bench "rooftop-duel" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphPong
            in forceEachm w1
          ) w
      , let w = buildWorldVampire 10000
        in bench "flock-10k" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphVampire
            in forceEachm w1
          ) w
      ]
  ]
