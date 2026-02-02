{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.List (foldl')
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.System as S

data Pos = Pos Double Double
  deriving (Eq, Show)

data Vel = Vel Double Double
  deriving (Eq, Show)

data Acc = Acc Double Double
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

instance E.Component C Pos where
  inj = CPos
  prj c = case c of
    CPos v -> Just v
    _ -> Nothing

instance E.Component C Vel where
  inj = CVel
  prj c = case c of
    CVel v -> Just v
    _ -> Nothing

instance E.Component C Acc where
  inj = CAcc
  prj c = case c of
    CAcc v -> Just v
    _ -> Nothing

instance E.Component C Hp where
  inj = CHp
  prj c = case c of
    CHp v -> Just v
    _ -> Nothing

instance S.Eachable C (Pos, Vel)

type World = E.World C
type System msg = S.System C msg
type Graph msg = S.Graph C msg

data MoveLoop

data Move3 = Move3
  { pos3 :: Pos
  , vel3 :: Vel
  , acc3 :: Acc
  } deriving (Generic)

instance E.Queryable C Move3
instance S.Eachable C Move3

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

qPV :: E.Query C (Pos, Vel)
qPV = (,) <$> (E.comp :: E.Query C Pos) <*> (E.comp :: E.Query C Vel)

qP :: E.Query C Pos
qP = E.comp

qVA :: E.Query C (Vel, Acc)
qVA = (,) <$> (E.comp :: E.Query C Vel) <*> (E.comp :: E.Query C Acc)

qMove3 :: E.Query C Move3
qMove3 = E.query @Move3

moveSys :: System String
moveSys = S.system (S.handle 0) $ do
  S.each qPV (\(Pos x y, Vel vx vy) -> (Pos (x + vx) (y + vy), Vel vx vy))
  pure ()

moveSysSmall :: System String
moveSysSmall = S.system (S.handle 0) $ do
  S.each qP (\(Pos x y) -> Pos (x + 1) y)
  pure ()

moveSysJoin3 :: System String
moveSysJoin3 = S.system (S.handle 0) $ do
  S.each qMove3 (\q ->
    let Pos x y = pos3 q
        Vel vx vy = vel3 q
        Acc ax ay = acc3 q
        vx' = vx + ax
        vy' = vy + ay
        p' = Pos (x + vx') (y + vy')
    in q { pos3 = p', vel3 = Vel vx' vy' }
    )
  pure ()

moveSysTwoEach :: System String
moveSysTwoEach = S.system (S.handle 0) $ do
  S.each qPV (\(Pos x y, Vel vx vy) -> (Pos (x + vx) (y + vy), Vel vx vy))
  S.eachP qVA (\(Vel vx vy, Acc ax ay) -> S.set (Vel (vx + ax) (vy + ay)))
  pure ()

advance :: Double -> Pos -> Vel -> (Pos, Vel)
advance dt (Pos x y) (Vel vx vy) =
  let ax = sin (x * 0.01) * 0.5
      ay = cos (y * 0.01) * 0.5
      vx' = vx + ax * dt
      vy' = vy + ay * dt
      x' = x + vx' * dt + sin vx'
      y' = y + vy' * dt + cos vy'
  in (Pos x' y', Vel vx' vy')

moveSysLogic :: System String
moveSysLogic = S.system (S.handle 0) $ do
  dt <- S.dt
  S.each qPV (\(p, v) ->
    let (p', v') = advance dt p v
    in (p', v')
    )
  pure ()

moveStep :: F.Step (Pos, Vel) (Pos, Vel)
moveStep = F.Step $ \dt (Pos x y, Vel vx vy) ->
  let p = Pos (x + vx * dt) (y + vy * dt)
      v = Vel vx vy
  in ((p, v), moveStep)

moveSysStep :: System String
moveSysStep = S.system (S.handle 1) $ do
  S.eachStep @MoveLoop qPV moveStep $ \_ (p, v) ->
    S.set p <> S.set v
  pure ()

moveSysM :: System String
moveSysM = S.system (S.handle 0) $ do
  S.eachM @MoveLoop qPV $ \(Pos x y, Vel vx vy) -> do
    let p = Pos (x + vx) (y + vy)
        v = Vel vx vy
    S.edit (S.set p <> S.set v)
  pure ()

moveSysMSmall :: System String
moveSysMSmall = S.system (S.handle 0) $ do
  S.eachM @MoveLoop qP $ \(Pos x y) -> do
    let p = Pos (x + 1) y
    S.edit (S.set p)
  pure ()

moveSysMJoin3 :: System String
moveSysMJoin3 = S.system (S.handle 0) $ do
  S.eachM @MoveLoop qMove3 $ \q -> do
    let Pos x y = pos3 q
        Vel vx vy = vel3 q
        Acc ax ay = acc3 q
        vx' = vx + ax
        vy' = vy + ay
        p' = Pos (x + vx') (y + vy')
    S.edit (S.set p' <> S.set (Vel vx' vy'))
  pure ()

moveSysMTwoEach :: System String
moveSysMTwoEach = S.system (S.handle 0) $ do
  S.eachM @MoveLoop qPV $ \(Pos x y, Vel vx vy) -> do
    let p = Pos (x + vx) (y + vy)
    S.edit (S.set p)
  S.eachM @MoveLoop qVA $ \(Vel vx vy, Acc ax ay) -> do
    let v = Vel (vx + ax) (vy + ay)
    S.edit (S.set v)
  pure ()

moveSysMLogic :: System String
moveSysMLogic = S.system (S.handle 0) $ do
  dt <- S.dt
  S.eachM @MoveLoop qPV $ \(p, v) -> do
    let (p', v') = advance dt p v
    S.edit (S.set p' <> S.set v')
  pure ()

graphEach :: Graph String
graphEach = S.graph @String moveSys

graphEachS :: Graph String
graphEachS = S.graph @String moveSysStep

graphEachM :: Graph String
graphEachM = S.graph @String moveSysM

graphEachSmall :: Graph String
graphEachSmall = S.graph @String moveSysSmall

graphEachJoin3 :: Graph String
graphEachJoin3 = S.graph @String moveSysJoin3

graphEachTwoEach :: Graph String
graphEachTwoEach = S.graph @String moveSysTwoEach

graphEachLogic :: Graph String
graphEachLogic = S.graph @String moveSysLogic

graphEachMSmall :: Graph String
graphEachMSmall = S.graph @String moveSysMSmall

graphEachMJoin3 :: Graph String
graphEachMJoin3 = S.graph @String moveSysMJoin3

graphEachMTwoEach :: Graph String
graphEachMTwoEach = S.graph @String moveSysMTwoEach

graphEachMLogic :: Graph String
graphEachMLogic = S.graph @String moveSysMLogic

main :: IO ()
main = defaultMain
  [ bgroup "ecs"
      [ let w = buildWorld 10000
        in bench "query-10k" $ nf (length . E.runq qPV) w
      , let w = buildWorld 10000
        in bench "query-small-10k" $ nf (length . E.runq qP) w
      , bench "spawn-10k" $ nf (length . E.entities . buildWorld) 10000
      ]
  , bgroup "system"
      [ bgroup "10k"
          [ let w = buildWorld 10000
            in bench "each" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-two" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachLogic
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-two" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMLogic
                in length (E.entities w1)
              ) w
          ]
      , bgroup "50k"
          [ let w = buildWorld 50000
            in bench "each" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "each-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "each-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "each-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachLogic
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "eachm-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "eachm-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "eachm-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMLogic
                in length (E.entities w1)
              ) w
          ]
      , bgroup "100k"
          [ let w = buildWorld 100000
            in bench "each" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 100000
            in bench "each-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 100000
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 100000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in length (E.entities w1)
              ) w
          , let w = buildWorld 100000
            in bench "eachm-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSmall
                in length (E.entities w1)
              ) w
          ]
      ]
  ]
