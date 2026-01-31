module Engine.Data.Filterable
  ( Filterable(..)
  , filter
  ) where

import Prelude hiding (filter)

class Functor f => Filterable f where
  mapMaybe :: (a -> Maybe b) -> f a -> f b

filter :: Filterable f => (a -> Bool) -> f a -> f a
filter p = mapMaybe (\a -> if p a then Just a else Nothing)
