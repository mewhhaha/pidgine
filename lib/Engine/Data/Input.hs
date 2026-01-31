module Engine.Data.Input
  ( Button(..)
  , Input(..)
  , press
  , release
  , held
  , axis
  ) where

import Engine.Data.FRP (Events, Step(..))

data Button = Btn String
  deriving (Eq, Ord, Show)

data Input = Input
  { down :: [Button]
  , up :: [Button]
  , holds :: [Button]
  , axes :: [(String, Double)]
  }
  deriving (Eq, Show)

instance Semigroup Input where
  a <> b = Input
    { down = down a <> down b
    , up = up a <> up b
    , holds = holds a <> holds b
    , axes = mergeAxes (axes a) (axes b)
    }

instance Monoid Input where
  mempty = Input [] [] [] []

press :: Button -> Step Input (Events ())
press b = Step $ \_ i ->
  (if b `elem` down i then [()] else [], press b)

release :: Button -> Step Input (Events ())
release b = Step $ \_ i ->
  (if b `elem` up i then [()] else [], release b)

held :: Button -> Step Input Bool
held b = Step $ \_ i ->
  (b `elem` holds i, held b)

axis :: String -> Step Input Double
axis name = Step $ \_ i ->
  (lookupAxis name (axes i), axis name)

lookupAxis :: String -> [(String, Double)] -> Double
lookupAxis _ [] = 0
lookupAxis name ((k, v) : xs) =
  if name == k then v else lookupAxis name xs

mergeAxes :: [(String, Double)] -> [(String, Double)] -> [(String, Double)]
mergeAxes xs ys = foldl' add xs ys
  where
    add acc (k, v) =
      let (found, rest) = extract k acc
      in case found of
          Nothing -> (k, v) : acc
          Just v0 -> (k, v0 + v) : rest

extract :: String -> [(String, Double)] -> (Maybe Double, [(String, Double)])
extract _ [] = (Nothing, [])
extract key ((k, v) : xs)
  | key == k =
      let (_, rest) = extract key xs
      in (Just v, rest)
  | otherwise =
      let (found, rest) = extract key xs
      in (found, (k, v) : rest)
