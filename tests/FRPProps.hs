module FRPProps
  ( prop_edge
  , prop_once
  , prop_app
  , prop_span_range
  , prop_span_progress
  , prop_tween_progress
  ) where

import qualified Engine.Data.FRP as F
import Test.QuickCheck

prop_edge :: [Int] -> Bool
prop_edge xs =
  let inputs = zip (repeat 1) xs
      outs = F.run F.edge inputs
      expected = edgeExpected xs
  in outs == expected

prop_once :: [[Int]] -> Bool
prop_once evs =
  let inputs = zip (repeat 1) evs
      outs = concat (F.run F.once inputs)
      expected = case concat evs of
        [] -> []
        (x : _) -> [x]
  in outs == expected

prop_app :: Fun Int Int -> Int -> [NonNegative Double] -> [Int] -> Bool
prop_app (Fun _ f) x dts xs =
  let inputs = zip (map getNonNegative dts) xs
      outs = F.run (pure f <*> pure x) inputs
  in all (== f x) outs

prop_span_range :: NonNegative Double -> Positive Double -> Double -> Bool
prop_span_range (NonNegative t0) (Positive dt) t =
  let t1 = t0 + dt
      sp = F.Span t0 t1
      r = F.rangeSpan sp t
  in if not (finite t)
      then True
      else case r of
        Nothing -> t < t0 || t >= t1
        Just u -> u >= 0 && u <= 1 && t >= t0 && t < t1

prop_span_progress :: NonNegative Double -> Positive Double -> Double -> Bool
prop_span_progress (NonNegative t0) (Positive dt) t =
  let t1 = t0 + dt
      sp = F.Span t0 t1
      u = F.progressSpan sp t
  in if not (finite t)
      then True
      else if t <= t0
        then u == 0
        else if t >= t1
          then u == 1
          else u > 0 && u < 1

prop_tween_progress :: NonNegative Double -> Positive Double -> Double -> Bool
prop_tween_progress (NonNegative t0) (Positive dt) t =
  let t1 = t0 + dt
      sp = F.Span t0 t1
      tw = F.tween sp id id
  in if not (finite t)
      then True
      else F.at tw t == F.progressSpan sp t

edgeExpected :: [Int] -> [[Int]]
edgeExpected [] = []
edgeExpected (x:xs) = [x] : go x xs
  where
    go _ [] = []
    go prev (y:ys) =
      (if y /= prev then [y] else []) : go y ys

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
