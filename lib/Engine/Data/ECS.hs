{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.ECS
  ( Entity
  , eid
  , Component
  , component
  , World
  , empty
  , spawn
  , kill
  , get
  , set
  , del
  , has
  , entities
  , put
  , getr
  , tag
  , untag
  , hastag
  , Query
  , comp
  , opt
  , runq
  , foldq
  , Queryable(..)
  , Has(..)
  , Not(..)
  , hasQ
  , notHasQ
  ) where

import Prelude

import Engine.Data.Filterable (Filterable(..))
import qualified Control.Applicative as A
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import GHC.Generics
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Entity = Entity { eid :: Int }
  deriving (Eq, Ord, Show)

data Component = forall c. Typeable c => Component c

component :: Typeable c => c -> Component
component = Component

data World = World
  { next :: Int
  , ents :: IntSet
  , comps :: Map TypeRep (IntMap Dynamic)
  , tags :: Map String IntSet
  , res :: Map TypeRep Dynamic
  }

empty :: World
empty = World
  { next = 0
  , ents = IntSet.empty
  , comps = Map.empty
  , tags = Map.empty
  , res = Map.empty
  }

spawn :: [Component] -> World -> (Entity, World)
spawn cs w =
  let ent = Entity (next w)
      w1 = w { next = next w + 1, ents = IntSet.insert (eid ent) (ents w) }
      comps' = foldl' (insertComp ent) (comps w1) cs
  in (ent, w1 { comps = comps' })

kill :: Entity -> World -> World
kill ent w =
  let eid' = eid ent
      comps' = Map.mapMaybe (dropEnt eid') (comps w)
      tags' = Map.mapMaybe (dropTag eid') (tags w)
  in w
      { ents = IntSet.delete eid' (ents w)
      , comps = comps'
      , tags = tags'
      }

get :: forall c. Typeable c => Entity -> World -> Maybe c
get ent w =
  case Map.lookup (typeRep (Proxy :: Proxy c)) (comps w) of
    Nothing -> Nothing
    Just im -> IntMap.lookup (eid ent) im >>= fromDynamic

set :: Typeable c => Entity -> c -> World -> World
set ent c w =
  if not (exists ent w)
    then w
    else w { comps = insertComp ent (comps w) (Component c) }

del :: forall c. Typeable c => Entity -> World -> World
del ent w =
  if not (exists ent w)
    then w
    else case Map.lookup rep (comps w) of
      Nothing -> w
      Just im ->
        let im' = IntMap.delete (eid ent) im
            comps' =
              if IntMap.null im'
                then Map.delete rep (comps w)
                else Map.insert rep im' (comps w)
        in w { comps = comps' }
  where
    rep = typeRep (Proxy :: Proxy c)

has :: forall (c :: Type). Typeable c => Entity -> World -> Bool
has ent w =
  case (get ent w :: Maybe c) of
    Just _ -> True
    Nothing -> False

entities :: World -> [Entity]
entities w = map Entity (IntSet.toList (ents w))

put :: Typeable r => r -> World -> World
put r w =
  w { res = Map.insert (typeOf r) (toDyn r) (res w) }

getr :: forall r. Typeable r => World -> Maybe r
getr w =
  Map.lookup (typeRep (Proxy :: Proxy r)) (res w) >>= fromDynamic

tag :: String -> Entity -> World -> World
tag t ent w =
  if not (exists ent w)
    then w
    else w { tags = Map.alter (insertTag (eid ent)) t (tags w) }

untag :: String -> Entity -> World -> World
untag t ent w =
  if not (exists ent w)
    then w
    else w { tags = Map.update (removeTag (eid ent)) t (tags w) }

hastag :: String -> Entity -> World -> Bool
hastag t ent w =
  if not (exists ent w)
    then False
    else case Map.lookup t (tags w) of
      Nothing -> False
      Just s -> IntSet.member (eid ent) s

data Query a = Query
  { need :: [TypeRep]
  , run :: Entity -> World -> Maybe a
  }

instance Functor Query where
  fmap f q = q { run = \e w -> fmap f (run q e w) }

instance Applicative Query where
  pure a = Query [] (\_ _ -> Just a)
  qf <*> qa = Query
    { need = need qf ++ need qa
    , run = \e w -> do
        f <- run qf e w
        a <- run qa e w
        pure (f a)
    }

instance A.Alternative Query where
  empty = Query [] (\_ _ -> Nothing)
  q1 <|> q2 = Query
    { need = []
    , run = \e w -> run q1 e w A.<|> run q2 e w
    }

comp :: forall c. Typeable c => Query c
comp = Query
  { need = [typeRep (Proxy :: Proxy c)]
  , run = \e w -> get e w
  }

opt :: forall c. Typeable c => Query (Maybe c)
opt = Query
  { need = []
  , run = \e w -> Just (get e w)
  }

runq :: Query a -> World -> [(Entity, a)]
runq q w =
  foldq q (\e a acc -> (e, a) : acc) [] w

foldq :: Query a -> (Entity -> a -> s -> s) -> s -> World -> s
foldq q step s0 w =
  IntSet.foldr go s0 (candidates q w)
  where
    go eid' acc =
      let ent = Entity eid'
      in case run q ent w of
          Just a -> step ent a acc
          Nothing -> acc

exists :: Entity -> World -> Bool
exists ent w = IntSet.member (eid ent) (ents w)

insertComp :: Entity -> Map TypeRep (IntMap Dynamic) -> Component -> Map TypeRep (IntMap Dynamic)
insertComp ent m (Component c) =
  Map.alter (insertAt (eid ent) (toDyn c)) (typeOf c) m

insertAt :: Int -> Dynamic -> Maybe (IntMap Dynamic) -> Maybe (IntMap Dynamic)
insertAt eid' d Nothing = Just (IntMap.singleton eid' d)
insertAt eid' d (Just im) = Just (IntMap.insert eid' d im)

dropEnt :: Int -> IntMap Dynamic -> Maybe (IntMap Dynamic)
dropEnt eid' im =
  let im' = IntMap.delete eid' im
  in if IntMap.null im' then Nothing else Just im'

insertTag :: Int -> Maybe IntSet -> Maybe IntSet
insertTag eid' Nothing = Just (IntSet.singleton eid')
insertTag eid' (Just s) = Just (IntSet.insert eid' s)

removeTag :: Int -> IntSet -> Maybe IntSet
removeTag eid' s =
  let s' = IntSet.delete eid' s
  in if IntSet.null s' then Nothing else Just s'

dropTag :: Int -> IntSet -> Maybe IntSet
dropTag eid' = removeTag eid'

candidates :: Query a -> World -> IntSet
candidates q w =
  case dedupe (need q) of
    [] -> ents w
    reps -> intersectAll (map (componentSet w) reps)

dedupe :: [TypeRep] -> [TypeRep]
dedupe = Set.toList . Set.fromList

componentSet :: World -> TypeRep -> IntSet
componentSet w rep =
  case Map.lookup rep (comps w) of
    Nothing -> IntSet.empty
    Just im -> IntMap.keysSet im

intersectAll :: [IntSet] -> IntSet
intersectAll [] = IntSet.empty
intersectAll (s:xs) = foldl' IntSet.intersection s xs
instance Filterable Query where
  mapMaybe f q = q { run = \e w -> run q e w >>= f }

newtype Has (a :: Type) = Has ()
  deriving (Eq, Show)

newtype Not (a :: Type) = Not ()
  deriving (Eq, Show)

hasQ :: forall a. Typeable a => Query (Has a)
hasQ = Has () <$ comp @a

notHasQ :: forall a. Typeable a => Query (Not a)
notHasQ =
  mapMaybe
    (\m -> case m of
      Nothing -> Just (Not ())
      Just _ -> Nothing
    )
    (opt @a)

class FieldQuery a where
  fieldQ :: Query a

instance {-# OVERLAPPABLE #-} Typeable a => FieldQuery a where
  fieldQ = comp

instance {-# OVERLAPPING #-} Typeable a => FieldQuery (Maybe a) where
  fieldQ = opt

instance {-# OVERLAPPING #-} Typeable a => FieldQuery (Has a) where
  fieldQ = hasQ

instance {-# OVERLAPPING #-} Typeable a => FieldQuery (Not a) where
  fieldQ = notHasQ

class GQuery f where
  gquery :: Query (f p)

instance GQuery f => GQuery (M1 i c f) where
  gquery = M1 <$> gquery

instance (GQuery a, GQuery b) => GQuery (a :*: b) where
  gquery = (:*:) <$> gquery <*> gquery

instance (GQuery a, GQuery b) => GQuery (a :+: b) where
  gquery = (L1 <$> gquery) A.<|> (R1 <$> gquery)

instance GQuery U1 where
  gquery = pure U1

instance FieldQuery a => GQuery (K1 i a) where
  gquery = K1 <$> fieldQ

class Queryable a where
  query :: Query a
  default query :: (Generic a, GQuery (Rep a)) => Query a
  query = to <$> gquery
