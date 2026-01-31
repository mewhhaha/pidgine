{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ECSProps
  ( prop_spawn_get
  , prop_set_get
  , prop_query_superset
  , prop_put_getr
  , prop_query_app
  , prop_query_alt
  , prop_query_queryable
  ) where

import Control.Applicative ((<|>))
import qualified Engine.Data.ECS as E
import GHC.Generics (Generic)

prop_spawn_get :: Int -> Bool
prop_spawn_get x =
  let (e, w) = E.spawn [E.component x] E.empty
  in E.get e w == Just x

prop_set_get :: Int -> Bool
prop_set_get x =
  let (e, w) = E.spawn [] E.empty
      w' = E.set e x w
  in E.get e w' == Just x

prop_query_superset :: Int -> Bool
prop_query_superset x =
  let (e, w) = E.spawn [E.component x, E.component True] E.empty
      results = E.runq (E.comp :: E.Query Int) w
  in (e, x) `elem` results

prop_put_getr :: Int -> Bool
prop_put_getr x =
  let w = E.put x E.empty
  in E.getr w == Just x

prop_query_app :: Int -> Bool
prop_query_app x =
  let (e, w) = E.spawn [E.component x, E.component True] E.empty
      q = (,) <$> (E.comp :: E.Query Int) <*> (E.comp :: E.Query Bool)
  in E.runq q w == [(e, (x, True))]

prop_query_alt :: Int -> Bool
prop_query_alt x =
  let (e, w) = E.spawn [E.component x] E.empty
      q = (E.comp :: E.Query Int) <|> fmap (const 0) (E.comp :: E.Query Bool)
  in E.runq q w == [(e, x)]

data QB = QB
  { qbInt :: Int
  , qbMaybe :: Maybe Bool
  } deriving (Eq, Show, Generic, E.Queryable)

prop_query_queryable :: Int -> Bool -> Bool
prop_query_queryable x b =
  let (e, w) = E.spawn [E.component x, E.component b] E.empty
  in E.runq (E.query @QB) w == [(e, QB x (Just b))]
