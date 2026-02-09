{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import ECSProps
import FRPProps
import ProgramProps
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import Test.QuickCheck (isSuccess, quickCheckResult)

data C
  = CInt Int
  | CString String
  | CBool Bool
  deriving (Generic)

instance E.ComponentId C

type World = E.World C

assert :: String -> Bool -> IO ()
assert name cond =
  if cond
    then pure ()
    else putStrLn ("FAIL: " ++ name) >> exitFailure

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1e-9

approxList :: [Double] -> [Double] -> Bool
approxList xs ys = length xs == length ys && and (zipWith approx xs ys)

approxMaybe :: Maybe Double -> Maybe Double -> Bool
approxMaybe (Just a) (Just b) = approx a b
approxMaybe Nothing Nothing = True
approxMaybe _ _ = False

approxMaybeList :: [Maybe Double] -> [Maybe Double] -> Bool
approxMaybeList xs ys = length xs == length ys && and (zipWith approxMaybe xs ys)

main :: IO ()
main = do
  let cOut = F.run (pure (7 :: Int)) [(1, ()), (1, ())]
  assert "constant" (cOut == [7, 7])

  let tOut = F.run F.time [(1, ()), (0.5, ())]
  assert "time" (approxList [1.0, 1.5] tOut)

  let dOut = F.run (F.delay (0 :: Int)) [(1, 1), (1, 2), (1, 3)]
  assert "delay" (dOut == [0, 1, 2])

  let evOut = map length (F.run (F.every 1) [(0.4,()), (0.4,()), (0.4,()), (0.4,())])
  assert "every" (evOut == [0,0,1,0])

  let durOut = F.run (F.during (0.5, 1.0)) [(0.25,()), (0.25,()), (0.25,()), (0.25,())]
  assert "during" (durOut == [False, True, True, False])

  let rOut = F.run (F.range (1, 2)) [(0.5,()), (0.5,()), (0.5,())]
  assert "range" (approxMaybeList rOut [Nothing, Just 0.0, Just 0.5])

  let pOut = F.run (F.progress (0, 1)) [(0.5,()), (0.5,()), (0.5,())]
  assert "progress" (approxList pOut [0.5, 1.0, 1.0])

  let wOut = F.run (F.window (1, 2) (pure (9 :: Int))) [(0.5,()), (0.6,()), (0.6,())]
  assert "window" (wOut == [Nothing, Just 9, Just 9])

  let sOut = F.run (F.since (F.after 0.2)) [(0.1,()), (0.1,()), (0.1,())]
  assert "since" (approxList sOut [0.1, 0.0, 0.1])

  let w0 = E.emptyWorld
      (e1, w1) = E.spawn ((10 :: Int), ("hi" :: String)) (w0 :: World)
  assert "get" (E.get e1 w1 == Just (10 :: Int))

  let w2 = E.set e1 True w1
  assert "has" (E.has @Bool e1 w2)

  let q = (E.comp :: E.Query C Int)
      qOut = E.runq q w2
  assert "query" (qOut == [(e1, 10)])

  assert "program resume" program_resume_once
  assert "program await value" program_await_value
  assert "each per-entity" program_eachm_entity_state
  assert "eachM enemy ai state machine" program_eachm_enemy_state_machine
  assert "each tuple query" program_each_tuple_query
  assert "compute fused order" program_compute_fused_order

  results <-
    sequence
      [ quickCheckResult prop_edge
      , quickCheckResult prop_once
      , quickCheckResult prop_app
      , quickCheckResult prop_span_range
      , quickCheckResult prop_span_progress
      , quickCheckResult prop_tween_progress
      , quickCheckResult prop_spawn_get
      , quickCheckResult prop_set_get
      , quickCheckResult prop_query_superset
      , quickCheckResult prop_put_getr
      , quickCheckResult prop_query_app
      , quickCheckResult prop_query_alt
      , quickCheckResult prop_query_queryable
      , quickCheckResult prop_query_queryable_sum
      , quickCheckResult prop_relations
      , quickCheckResult prop_parent_child
      , quickCheckResult prop_transform_inverse
      , quickCheckResult prop_program_resume
      , quickCheckResult prop_program_await_value
      ]

  if all isSuccess results
    then putStrLn "OK"
    else exitFailure
