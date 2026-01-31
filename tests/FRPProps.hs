module FRPProps
  ( prop_edge
  , prop_once
  , prop_app
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

edgeExpected :: [Int] -> [[Int]]
edgeExpected [] = []
edgeExpected (x:xs) = [x] : go x xs
  where
    go _ [] = []
    go prev (y:ys) =
      (if y /= prev then [y] else []) : go y ys
