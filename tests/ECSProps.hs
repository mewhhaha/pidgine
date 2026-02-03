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
  , prop_query_queryable_sum
  , prop_relations
  , prop_parent_child
  , prop_transform_inverse
  ) where

import Control.Applicative ((<|>))
import qualified Engine.Data.ECS as E
import GHC.Generics (Generic)
import qualified Engine.Data.Transform as T

data C
  = CInt Int
  | CBool Bool
  | CLocal T.Local
  | CGlobal T.Global
  deriving (Generic)

instance E.ComponentId C

type World = E.World C

prop_spawn_get :: Int -> Bool
prop_spawn_get x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
  in E.get e w == Just x

prop_set_get :: Int -> Bool
prop_set_get x =
  let (e, w) = E.spawn () (E.emptyWorld :: World)
      w' = E.set e x w
  in E.get e w' == Just x

prop_query_superset :: Int -> Bool
prop_query_superset x =
  let (e, w) = E.spawn (x, True) (E.emptyWorld :: World)
      results = E.runq (E.comp :: E.Query C Int) w
  in (e, x) `elem` results

prop_put_getr :: Int -> Bool
prop_put_getr x =
  let w = E.put x (E.emptyWorld :: World)
  in E.getr w == Just x

prop_query_app :: Int -> Bool
prop_query_app x =
  let (e, w) = E.spawn (x, True) (E.emptyWorld :: World)
      q = (,) <$> (E.comp :: E.Query C Int) <*> (E.comp :: E.Query C Bool)
  in E.runq q w == [(e, (x, True))]

prop_query_alt :: Int -> Bool
prop_query_alt x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
      q = (E.comp :: E.Query C Int) <|> fmap (const 0) (E.comp :: E.Query C Bool)
  in E.runq q w == [(e, x)]

data QB = QB
  { qbInt :: Int
  , qbMaybe :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance E.Queryable C QB

prop_query_queryable :: Int -> Bool -> Bool
prop_query_queryable x b =
  let (e, w) = E.spawn (x, b) (E.emptyWorld :: World)
  in E.runq (E.query @QB) w == [(e, QB x (Just b))]

data QSum = QInt Int | QBool Bool
  deriving (Eq, Show, Generic)

instance E.QueryableSum C QSum

prop_query_queryable_sum :: Int -> Bool
prop_query_queryable_sum x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
  in E.runq (E.querySum @QSum) w == [(e, QInt x)]

data Owns

prop_relations :: Int -> Int -> Bool
prop_relations a b =
  let (e1, w1) = E.spawn a (E.emptyWorld :: World)
      (e2, w2) = E.spawn b w1
      w3 = E.relate @Owns e1 e2 w2
  in E.out @Owns e1 w3 == [e2] && E.inn @Owns e2 w3 == [e1]

prop_parent_child :: Bool
prop_parent_child =
  let (p, w1) = E.spawn (T.Local (T.translate (0,0,0))) (E.emptyWorld :: World)
      (c, w2) = E.spawn (T.Local (T.translate (2,0,0))) w1
      w3 = T.attach p c w2
      w4 = T.propagate w3
  in E.get @T.Global c w4 == Just (T.Global (T.translate (2,0,0)))

prop_transform_inverse :: Bool
prop_transform_inverse =
  let m = T.compose [T.translate (1,2,3), T.scale (2,2,2)]
  in case T.inverse m of
      Nothing -> False
      Just inv -> T.mul m inv == T.identity
