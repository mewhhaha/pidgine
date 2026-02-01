{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.List (foldl')
import qualified Engine.Data.ECS as E
import qualified Engine.Data.System as S

data Pos = Pos Double Double
  deriving (Eq, Show)

data Vel = Vel Double Double
  deriving (Eq, Show)

data MoveEach
data MoveEachM
data MoveLoop

buildWorld :: Int -> E.World
buildWorld n = snd (foldl' add (0 :: Int, E.empty) [1 .. n])
  where
    add (i, w) _ =
      let (_, w1) = E.spawn (Pos (fromIntegral i) 0, Vel 1 1) w
      in (i + 1, w1)

qPV :: E.Query (Pos, Vel)
qPV = (,) <$> (E.comp :: E.Query Pos) <*> (E.comp :: E.Query Vel)

moveSys :: S.System String
moveSys = S.system @MoveEach $ do
  S.each qPV (\(Pos x y, Vel vx vy) -> (Pos (x + vx) (y + vy), Vel vx vy))
  pure ()

moveSysM :: S.System String
moveSysM = S.system @MoveEachM $ do
  S.eachM @MoveLoop qPV $ \e (Pos x y, Vel vx vy) -> do
    let p = Pos (x + vx) (y + vy)
        v = Vel vx vy
    S.edit (S.set e p <> S.set e v)
  pure ()

graphEach :: S.Graph String
graphEach = S.graph @String moveSys

graphEachM :: S.Graph String
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
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in length (E.entities w1)
              ) w
          ]
      ]
  ]
