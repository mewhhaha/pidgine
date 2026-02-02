{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.ECS
  ( Entity(..)
  , World
  , Bag
  , emptyWorld
  , entities
  , foldEntities
  , mapEntities
  , setEntities
  , spawn
  , kill
  , Component(..)
  , Bundle(..)
  , Has(..)
  , Not(..)
  , has
  , get
  , set
  , del
  , Query(..)
  , comp
  , opt
  , hasQ
  , notQ
  , runq
  , foldq
  , filterQ
  , Queryable(..)
  , QueryableSum(..)
  , query
  , querySum
  , TypeId
  , typeIdOf
  , put
  , getr
  , relate
  , unrelate
  , out
  , inn
  ) where

import Control.Applicative (Alternative(..))
import Data.Bits (xor)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Exts (Any)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

newtype Entity = Entity
  { eid :: Int
  } deriving (Eq, Ord, Show)

type TypeId = Int

typeIdOf :: forall a. Typeable a => TypeId
typeIdOf =
  let Fingerprint w1 w2 = typeRepFingerprint (typeRep (Proxy @a))
  in fromIntegral (w1 `xor` w2)

type Bag c = [c]

type EntityRow c = (Int, Bag c)

data World c = World
  { nextIdW :: !Int
  , entitiesW :: ![EntityRow c]
  , resourcesW :: !(IntMap Any)
  , relOutW :: !(IntMap (IntMap IntSet))
  , relInW :: !(IntMap (IntMap IntSet))
  }

emptyWorld :: World c
emptyWorld =
  World
    { nextIdW = 0
    , entitiesW = []
    , resourcesW = IntMap.empty
    , relOutW = IntMap.empty
    , relInW = IntMap.empty
    }

entities :: World c -> [Entity]
entities = map (Entity . rowId) . entitiesW

foldEntities :: (Entity -> Bag c -> s -> s) -> s -> World c -> s
foldEntities f s0 w =
  foldl'
    (\acc (eid', bag) -> f (Entity eid') bag acc)
    s0
    (entitiesW w)

mapEntities :: (Entity -> Bag c -> (Bag c)) -> World c -> World c
mapEntities f w =
  w
    { entitiesW = map
        (\(eid', bag) ->
          let bag' = f (Entity eid') bag
          in (eid', bag')
        )
        (entitiesW w)
    }

setEntities :: [(Entity, Bag c)] -> World c -> World c
setEntities rows w =
  w { entitiesW = map (\(Entity eid', bag) -> (eid', bag)) rows }

rowId :: EntityRow c -> Int
rowId (eid', _) = eid'

lookupRow :: Int -> [EntityRow c] -> Maybe (Bag c)
lookupRow _ [] = Nothing
lookupRow eid' ((eid'', bag) : rest)
  | eid' == eid'' = Just bag
  | otherwise = lookupRow eid' rest

adjustRow :: Int -> (Bag c -> Bag c) -> [EntityRow c] -> [EntityRow c]
adjustRow _ _ [] = []
adjustRow eid' f ((eid'', bag) : rest)
  | eid' == eid'' =
      let bag' = f bag
      in (eid'', bag') : rest
  | otherwise = (eid'', bag) : adjustRow eid' f rest

deleteRow :: Int -> [EntityRow c] -> [EntityRow c]
deleteRow eid' = filter (\(eid'', _) -> eid'' /= eid')

class Component c a where
  inj :: a -> c
  prj :: c -> Maybe a

class Bundle c a where
  bundle :: a -> [c]

instance {-# OVERLAPPING #-} Bundle c () where
  bundle _ = []

instance {-# OVERLAPPABLE #-} Component c a => Bundle c a where
  bundle a = [inj a]

instance {-# OVERLAPPING #-} Bundle c a => Bundle c (Maybe a) where
  bundle = maybe [] bundle

instance (Bundle c a, Bundle c b) => Bundle c (a, b) where
  bundle (a, b) = bundle a <> bundle b

instance (Bundle c a, Bundle c b, Bundle c d) => Bundle c (a, b, d) where
  bundle (a, b, d) = bundle a <> bundle b <> bundle d

instance (Bundle c a, Bundle c b, Bundle c d, Bundle c e) => Bundle c (a, b, d, e) where
  bundle (a, b, d, e) = bundle a <> bundle b <> bundle d <> bundle e

instance (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f) => Bundle c (a, b, d, e, f) where
  bundle (a, b, d, e, f) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f

instance (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g) => Bundle c (a, b, d, e, f, g) where
  bundle (a, b, d, e, f, g) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g

instance (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g, Bundle c h) => Bundle c (a, b, d, e, f, g, h) where
  bundle (a, b, d, e, f, g, h) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g <> bundle h

instance (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g, Bundle c h, Bundle c i) => Bundle c (a, b, d, e, f, g, h, i) where
  bundle (a, b, d, e, f, g, h, i) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g <> bundle h <> bundle i

spawn :: Bundle c a => a -> World c -> (Entity, World c)
spawn a w =
  let e = Entity (nextIdW w)
      row = (eid e, bundle a)
      ents' = row : entitiesW w
      w' = w { nextIdW = nextIdW w + 1, entitiesW = ents' }
  in (e, w')

kill :: Entity -> World c -> World c
kill e w =
  let eid' = eid e
      ents' = deleteRow eid' (entitiesW w)
      out' = dropRelEntity eid' (relOutW w)
      in' = dropRelEntity eid' (relInW w)
  in w { entitiesW = ents', relOutW = out', relInW = in' }

dropRelEntity :: Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
dropRelEntity eid' =
  IntMap.mapMaybe (\m ->
    let m' = IntMap.map (IntSet.delete eid') (IntMap.delete eid' m)
        m'' = IntMap.filter (not . IntSet.null) m'
    in if IntMap.null m'' then Nothing else Just m'')

bagGet :: Component c a => Bag c -> Maybe a
bagGet = foldr (\c acc -> prj c <|> acc) Nothing

bagSet :: forall a c. Component c a => a -> Bag c -> Bag c
bagSet a = (inj a :) . filter (isNothing . prj @c @a)

bagDel :: forall a c. Component c a => Bag c -> Bag c
bagDel = filter (isNothing . prj @c @a)

get :: forall a c. Component c a => Entity -> World c -> Maybe a
get e w =
  case lookupRow (eid e) (entitiesW w) of
    Nothing -> Nothing
    Just bag -> bagGet bag

set :: forall a c. Component c a => Entity -> a -> World c -> World c
set e a w =
  w { entitiesW = adjustRow (eid e) (bagSet a) (entitiesW w) }

del :: forall a c. Component c a => Entity -> World c -> World c
del e w =
  w { entitiesW = adjustRow (eid e) (bagDel @a) (entitiesW w) }

has :: forall a c. Component c a => Entity -> World c -> Bool
has e w = isJust (get @a e w)

newtype Query c a = Query
  { runQuery :: Entity -> Bag c -> Maybe a
  }

instance Functor (Query c) where
  fmap f (Query q) = Query (\e bag -> fmap f (q e bag))

instance Applicative (Query c) where
  pure a = Query (\_ _ -> Just a)
  Query f <*> Query g = Query (\e bag -> f e bag <*> g e bag)

instance Monad (Query c) where
  Query q >>= f = Query (\e bag -> q e bag >>= \a -> runQuery (f a) e bag)

instance Alternative (Query c) where
  empty = Query (\_ _ -> Nothing)
  Query a <|> Query b = Query (\e bag -> a e bag <|> b e bag)

comp :: forall a c. Component c a => Query c a
comp = Query (\_ bag -> bagGet bag)

opt :: forall a c. Component c a => Query c (Maybe a)
opt = Query (\_ bag -> Just (bagGet bag))

data Has a = Has deriving (Eq, Show)
data Not a = Not deriving (Eq, Show)

hasQ :: forall a c. Component c a => Query c (Has a)
hasQ = Query (\_ bag -> if isJust (bagGet bag :: Maybe a) then Just Has else Nothing)

notQ :: forall a c. Component c a => Query c (Not a)
notQ = Query (\_ bag -> if isJust (bagGet bag :: Maybe a) then Nothing else Just Not)

runq :: Query c a -> World c -> [(Entity, a)]
runq q w =
  foldr
    (\(eid', bag) acc ->
      case runQuery q (Entity eid') bag of
        Nothing -> acc
        Just a -> (Entity eid', a) : acc
    ) [] (entitiesW w)

foldq :: Query c a -> (Entity -> a -> s -> s) -> s -> World c -> s
foldq q step s0 w =
  foldl'
    (\acc (eid', bag) ->
      case runQuery q (Entity eid') bag of
        Nothing -> acc
        Just a -> step (Entity eid') a acc
    ) s0 (entitiesW w)

filterQ :: (a -> Bool) -> Query c a -> Query c a
filterQ f (Query q) =
  Query (\e bag -> do
    a <- q e bag
    if f a then Just a else Nothing
  )

class QueryField c a where
  fieldQuery :: Query c a

instance {-# OVERLAPPABLE #-} Component c a => QueryField c a where
  fieldQuery = comp

instance {-# OVERLAPPING #-} Component c a => QueryField c (Maybe a) where
  fieldQuery = opt

instance {-# OVERLAPPING #-} Component c a => QueryField c (Has a) where
  fieldQuery = hasQ

instance {-# OVERLAPPING #-} Component c a => QueryField c (Not a) where
  fieldQuery = notQ

class Queryable c a where
  queryC :: Query c a
  default queryC :: (Generic a, GQueryable c (Rep a)) => Query c a
  queryC = to <$> gquery

class QueryableSum c a where
  querySumC :: Query c a
  default querySumC :: (Generic a, GQueryableSum c (Rep a)) => Query c a
  querySumC = to <$> gquerySum

query :: forall a c. Queryable c a => Query c a
query = queryC @c @a

querySum :: forall a c. QueryableSum c a => Query c a
querySum = querySumC @c @a

class GQueryable c f where
  gquery :: Query c (f p)

instance GQueryable c U1 where
  gquery = pure U1

instance GQueryable c f => GQueryable c (M1 i m f) where
  gquery = M1 <$> gquery

instance (GQueryable c a, GQueryable c b) => GQueryable c (a :*: b) where
  gquery = (:*:) <$> gquery <*> gquery

instance QueryField c a => GQueryable c (K1 i a) where
  gquery = K1 <$> fieldQuery

class GQueryableSum c f where
  gquerySum :: Query c (f p)

instance (GQueryableSum c a, GQueryableSum c b) => GQueryableSum c (a :+: b) where
  gquerySum = (L1 <$> gquerySum) <|> (R1 <$> gquerySum)

instance GQueryableSum c f => GQueryableSum c (M1 D m f) where
  gquerySum = M1 <$> gquerySum

instance GQueryableSum c f => GQueryableSum c (M1 S m f) where
  gquerySum = M1 <$> gquerySum

instance GQueryable c f => GQueryableSum c (M1 C m f) where
  gquerySum = M1 <$> gquery

put :: forall a c. Typeable a => a -> World c -> World c
put a w =
  w { resourcesW = IntMap.insert (typeIdOf @a) (unsafeCoerce a) (resourcesW w) }

getr :: forall a c. Typeable a => World c -> Maybe a
getr w = unsafeCoerce <$> IntMap.lookup (typeIdOf @a) (resourcesW w)

relate :: forall r c. Typeable r => Entity -> Entity -> World c -> World c
relate a b w =
  let rid = typeIdOf @r
      aId = eid a
      bId = eid b
      out' = insertRel rid aId bId (relOutW w)
      in' = insertRel rid bId aId (relInW w)
  in w { relOutW = out', relInW = in' }

unrelate :: forall r c. Typeable r => Entity -> Entity -> World c -> World c
unrelate a b w =
  let rid = typeIdOf @r
      aId = eid a
      bId = eid b
      out' = deleteRel rid aId bId (relOutW w)
      in' = deleteRel rid bId aId (relInW w)
  in w { relOutW = out', relInW = in' }

out :: forall r c. Typeable r => Entity -> World c -> [Entity]
out e w =
  let rid = typeIdOf @r
      rels = IntMap.findWithDefault IntMap.empty rid (relOutW w)
  in case IntMap.lookup (eid e) rels of
      Nothing -> []
      Just s -> map Entity (IntSet.toList s)

inn :: forall r c. Typeable r => Entity -> World c -> [Entity]
inn e w =
  let rid = typeIdOf @r
      rels = IntMap.findWithDefault IntMap.empty rid (relInW w)
  in case IntMap.lookup (eid e) rels of
      Nothing -> []
      Just s -> map Entity (IntSet.toList s)

insertRel :: TypeId -> Int -> Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
insertRel rid src dst table =
  let rels = IntMap.findWithDefault IntMap.empty rid table
      targets = IntMap.findWithDefault IntSet.empty src rels
      rels' = IntMap.insert src (IntSet.insert dst targets) rels
  in IntMap.insert rid rels' table

deleteRel :: TypeId -> Int -> Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
deleteRel rid src dst table =
  let rels = IntMap.findWithDefault IntMap.empty rid table
      targets = IntMap.findWithDefault IntSet.empty src rels
      targets' = IntSet.delete dst targets
      rels' =
        if IntSet.null targets'
          then IntMap.delete src rels
          else IntMap.insert src targets' rels
      table' =
        if IntMap.null rels'
          then IntMap.delete rid table
          else IntMap.insert rid rels' table
  in table'
