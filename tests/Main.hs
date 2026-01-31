{-# LANGUAGE TypeApplications #-}

module Main where

import ECSProps
import FRPProps
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import System.Exit (exitFailure)
import Test.QuickCheck (isSuccess, quickCheckResult)

assert :: String -> Bool -> IO ()
assert name cond =
  if cond
    then pure ()
    else putStrLn ("FAIL: " ++ name) >> exitFailure

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1e-9

approxList :: [Double] -> [Double] -> Bool
approxList xs ys = length xs == length ys && and (zipWith approx xs ys)

main :: IO ()
main = do
  let cOut = F.run (pure (7 :: Int)) [(1, ()), (1, ())]
  assert "constant" (cOut == [7, 7])

  let tOut = F.run F.time [(1, ()), (0.5, ())]
  assert "time" (approxList [1.0, 1.5] tOut)

  let dOut = F.run (F.delay (0 :: Int)) [(1, 1), (1, 2), (1, 3)]
  assert "delay" (dOut == [0, 1, 2])

  let w0 = E.empty
      (e1, w1) = E.spawn [E.component (10 :: Int), E.component ("hi" :: String)] w0
  assert "get" (E.get e1 w1 == Just (10 :: Int))

  let w2 = E.set e1 True w1
  assert "has" (E.has @Bool e1 w2)

  let q = (E.comp :: E.Query Int)
      qOut = E.runq q w2
  assert "query" (qOut == [(e1, 10)])

  results <-
    sequence
      [ quickCheckResult prop_edge
      , quickCheckResult prop_once
      , quickCheckResult prop_app
      , quickCheckResult prop_spawn_get
      , quickCheckResult prop_set_get
      , quickCheckResult prop_query_superset
      , quickCheckResult prop_put_getr
      , quickCheckResult prop_query_app
      , quickCheckResult prop_query_alt
      , quickCheckResult prop_query_queryable
      ]

  if all isSuccess results
    then putStrLn "OK"
    else exitFailure
