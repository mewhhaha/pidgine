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
  , Bundle(..)
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
  , QueryableSum(..)
  , Has(..)
  , Not(..)
  , hasQ
  , notHasQ
  , mapMaybeQ
  , filterQ
  , Out(..)
  , In(..)
  , relate
  , unrelate
  , out
  , inn
  , Build(..)
  , build
  , exec
  , spawnB
  ) where

import Prelude

import qualified Control.Applicative as A
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import GHC.Generics
import GHC.TypeLits (ErrorMessage(..), TypeError)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Entity = Entity { eid :: Int }
  deriving (Eq, Ord, Show)

data Component = forall c. Typeable c => Component c

component :: Typeable c => c -> Component
component = Component

class Bundle a where
  bundle :: a -> [Component]

instance {-# OVERLAPPABLE #-} Typeable a => Bundle a where
  bundle a = [component a]

instance Bundle Component where
  bundle c = [c]

instance {-# OVERLAPPING #-} Bundle () where
  bundle () = []

instance Bundle [Component] where
  bundle = id

instance {-# OVERLAPPING #-} (Bundle a, Bundle b) => Bundle (a, b) where
  bundle (a, b) = bundle a ++ bundle b

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c) => Bundle (a, b, c) where
  bundle (a, b, c) = bundle a ++ bundle b ++ bundle c

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c, Bundle d) => Bundle (a, b, c, d) where
  bundle (a, b, c, d) = bundle a ++ bundle b ++ bundle c ++ bundle d

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c, Bundle d, Bundle e)
  => Bundle (a, b, c, d, e) where
  bundle (a, b, c, d, e) = bundle a ++ bundle b ++ bundle c ++ bundle d ++ bundle e

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c, Bundle d, Bundle e, Bundle f)
  => Bundle (a, b, c, d, e, f) where
  bundle (a, b, c, d, e, f) =
    bundle a ++ bundle b ++ bundle c ++ bundle d ++ bundle e ++ bundle f

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c, Bundle d, Bundle e, Bundle f, Bundle g)
  => Bundle (a, b, c, d, e, f, g) where
  bundle (a, b, c, d, e, f, g) =
    bundle a ++ bundle b ++ bundle c ++ bundle d ++ bundle e ++ bundle f ++ bundle g

instance {-# OVERLAPPING #-} (Bundle a, Bundle b, Bundle c, Bundle d, Bundle e, Bundle f, Bundle g, Bundle h)
  => Bundle (a, b, c, d, e, f, g, h) where
  bundle (a, b, c, d, e, f, g, h) =
    bundle a ++ bundle b ++ bundle c ++ bundle d ++ bundle e ++ bundle f ++ bundle g ++ bundle h

newtype Build a = Build { runBuild :: World -> (a, World) }

instance Functor Build where
  fmap f (Build g) = Build $ \w ->
    let (a, w') = g w
    in (f a, w')

instance Applicative Build where
  pure a = Build $ \w -> (a, w)
  Build f <*> Build a = Build $ \w ->
    let (f', w1) = f w
        (a', w2) = a w1
    in (f' a', w2)

instance Monad Build where
  Build a >>= f = Build $ \w ->
    let (a', w1) = a w
        Build b = f a'
    in b w1

build :: Build a -> (a, World)
build b = runBuild b empty

exec :: Build a -> World
exec = snd . build

spawnB :: Bundle a => a -> Build Entity
spawnB cs = Build $ \w -> spawn cs w

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

spawn :: Bundle a => a -> World -> (Entity, World)
spawn cs w =
  let ent = Entity (next w)
      w1 = w { next = next w + 1, ents = IntSet.insert (eid ent) (ents w) }
      comps' = foldl' (insertComp ent) (comps w1) (bundle cs)
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
mapMaybeQ :: (a -> Maybe b) -> Query a -> Query b
mapMaybeQ f q = q { run = \e w -> run q e w >>= f }

filterQ :: (a -> Bool) -> Query a -> Query a
filterQ p = mapMaybeQ (\a -> if p a then Just a else Nothing)

newtype Has (a :: Type) = Has ()
  deriving (Eq, Show)

newtype Not (a :: Type) = Not ()
  deriving (Eq, Show)

hasQ :: forall a. Typeable a => Query (Has a)
hasQ = Has () <$ comp @a

notHasQ :: forall a. Typeable a => Query (Not a)
notHasQ =
  mapMaybeQ
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

newtype Out (r :: Type) = Out [Entity]
  deriving (Eq, Show)

newtype In (r :: Type) = In [Entity]
  deriving (Eq, Show)

out :: forall (r :: Type). Typeable r => Entity -> World -> [Entity]
out e w =
  case get @(Out r) e w of
    Nothing -> []
    Just (Out xs) -> xs

inn :: forall (r :: Type). Typeable r => Entity -> World -> [Entity]
inn e w =
  case get @(In r) e w of
    Nothing -> []
    Just (In xs) -> xs

relate :: forall (r :: Type). Typeable r => Entity -> Entity -> World -> World
relate a b w =
  let outs = out @r a w
      ins = inn @r b w
      w1 = set a (Out @r (b : Prelude.filter (/= b) outs)) w
      w2 = set b (In @r (a : Prelude.filter (/= a) ins)) w1
  in w2

unrelate :: forall (r :: Type). Typeable r => Entity -> Entity -> World -> World
unrelate a b w =
  let outs = Prelude.filter (/= b) (out @r a w)
      ins = Prelude.filter (/= a) (inn @r b w)
      w1 = if null outs then del @(Out r) a w else set a (Out @r outs) w
      w2 = if null ins then del @(In r) b w1 else set b (In @r ins) w1
  in w2

class GQuery f where
  gquery :: Query (f p)

instance GQuery f => GQuery (M1 i c f) where
  gquery = M1 <$> gquery

instance (GQuery a, GQuery b) => GQuery (a :*: b) where
  gquery = (:*:) <$> gquery <*> gquery

instance
  TypeError
    ( 'Text "Queryable does not support sum types."
    ':$$: 'Text "Use QueryableSum for sums (querySum)."
    )
  => GQuery (a :+: b) where
  gquery = error "unreachable"

instance GQuery U1 where
  gquery = pure U1

instance FieldQuery a => GQuery (K1 i a) where
  gquery = K1 <$> fieldQ

class Queryable a where
  query :: Query a
  default query :: (Generic a, GQuery (Rep a)) => Query a
  query = to <$> gquery

class GQuerySum f where
  gquerySum :: Query (f p)

instance GQuerySum f => GQuerySum (M1 i c f) where
  gquerySum = M1 <$> gquerySum

instance (GQuerySum a, GQuerySum b) => GQuerySum (a :*: b) where
  gquerySum = (:*:) <$> gquerySum <*> gquerySum

instance (GQuerySum a, GQuerySum b) => GQuerySum (a :+: b) where
  gquerySum = (L1 <$> gquerySum) A.<|> (R1 <$> gquerySum)

instance GQuerySum U1 where
  gquerySum = pure U1

instance FieldQuery a => GQuerySum (K1 i a) where
  gquerySum = K1 <$> fieldQ

class QueryableSum a where
  querySum :: Query a
  default querySum :: (Generic a, GQuerySum (Rep a)) => Query a
  querySum = to <$> gquerySum
