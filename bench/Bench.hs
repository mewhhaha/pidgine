{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.List (foldl')
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.System as S

data Pos = Pos Double Double
  deriving (Eq, Show)

data Vel = Vel Double Double
  deriving (Eq, Show)

data C
  = CPos Pos
  | CVel Vel

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

instance S.Eachable C (Pos, Vel)

type World = E.World C
type System msg = S.System C msg
type Graph msg = S.Graph C msg

data MoveLoop

buildWorld :: Int -> World
buildWorld n = snd (foldl' add (0 :: Int, E.emptyWorld) [1 .. n])
  where
    add (i, w) _ =
      let (_, w1) = E.spawn (Pos (fromIntegral i) 0, Vel 1 1) w
      in (i + 1, w1)

qPV :: E.Query C (Pos, Vel)
qPV = (,) <$> (E.comp :: E.Query C Pos) <*> (E.comp :: E.Query C Vel)

moveSys :: System String
moveSys = S.system (S.handle 0) $ do
  S.each qPV (\(Pos x y, Vel vx vy) -> (Pos (x + vx) (y + vy), Vel vx vy))
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

graphEach :: Graph String
graphEach = S.graph @String moveSys

graphEachS :: Graph String
graphEachS = S.graph @String moveSysStep

graphEachM :: Graph String
graphEachM = S.graph @String moveSysM

main :: IO ()
main = defaultMain
  [ bgroup "ecs"
      [ let w = buildWorld 10000
        in bench "query-10k" $ nf (length . E.runq qPV) w
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
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
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
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 50000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
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
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 100000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in length (E.entities w1)
              ) w
          ]
      ]
  ]
