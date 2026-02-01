{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Engine.Data.System
  ( Patch(..)
  , patch
  , empty
  , SystemId
  , GroupId
  , System
  , SystemV
  , AnySystem
  , anySystem
  , SystemM
  , Eachable(..)
  , Group(..)
  , group
  , Events
  , Inbox
  , events
  , done
  , seen
  , allIds
  , selfId
  , each
  , eachP
  , edit
  , send
  , dt
  , Await(..)
  , Awaitable
  , collect
  , emit
  , emitMany
  , await
  , Tick(..)
  , tick
  , wait
  , set
  , update
  , del
  , put
  , tag
  , untag
  , relate
  , unrelate
  , Sys
  , Graph(..)
  , system
  , graphAny
  , graphList
  , sysP
  , sysM
  , sysE
  , sysS
  , graph
  , run
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category(..))
import Control.Monad (ap)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics
import Data.Proxy (Proxy(..))
import Engine.Data.ECS (Entity, World)
import Engine.Data.FRP (DTime, Events, Step(..))
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Input as I

newtype Patch = Patch
  { apply :: World -> World
  }

patch :: (World -> World) -> Patch
patch = Patch

empty :: Patch
empty = mempty

instance Semigroup Patch where
  Patch f <> Patch g = Patch (g . f)

instance Monoid Patch where
  mempty = Patch id

set :: Typeable c => Entity -> c -> Patch
set e c = Patch (E.set e c)

update :: forall c. Typeable c => Entity -> (c -> c) -> Patch
update e f = Patch $ \w ->
  case E.get @c e w of
    Nothing -> w
    Just v -> E.set e (f v) w

del :: forall c. Typeable c => Entity -> Patch
del e = Patch (E.del @c e)

put :: Typeable c => c -> Patch
put c = Patch (E.put c)

tag :: String -> Entity -> Patch
tag t e = Patch (E.tag t e)

untag :: String -> Entity -> Patch
untag t e = Patch (E.untag t e)

relate :: forall (r :: Type). Typeable r => Entity -> Entity -> Patch
relate a b = Patch (E.relate @r a b)

unrelate :: forall (r :: Type). Typeable r => Entity -> Entity -> Patch
unrelate a b = Patch (E.unrelate @r a b)

emit :: a -> Events a
emit x = [x]

emitMany :: Events a -> Events a
emitMany = id

type SystemId = TypeRep
type GroupId = TypeRep

type Done = Set.Set SystemId
type Seen = Set.Set SystemId

class Typeable tag => Tags tag where
  tagsOf :: Proxy tag -> Set.Set SystemId
  default tagsOf :: Typeable tag => Proxy tag -> Set.Set SystemId
  tagsOf _ = Set.singleton (typeRep (Proxy @tag))

instance {-# OVERLAPPABLE #-} Typeable tag => Tags tag

instance {-# OVERLAPPING #-} (Tags a, Tags b) => Tags (a, b) where
  tagsOf _ = tagsOf (Proxy @a) `Set.union` tagsOf (Proxy @b)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c) => Tags (a, b, c) where
  tagsOf _ =
    tagsOf (Proxy @a) `Set.union` tagsOf (Proxy @b) `Set.union` tagsOf (Proxy @c)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c, Tags d) => Tags (a, b, c, d) where
  tagsOf _ =
    tagsOf (Proxy @a)
      `Set.union` tagsOf (Proxy @b)
      `Set.union` tagsOf (Proxy @c)
      `Set.union` tagsOf (Proxy @d)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c, Tags d, Tags e) => Tags (a, b, c, d, e) where
  tagsOf _ =
    tagsOf (Proxy @a)
      `Set.union` tagsOf (Proxy @b)
      `Set.union` tagsOf (Proxy @c)
      `Set.union` tagsOf (Proxy @d)
      `Set.union` tagsOf (Proxy @e)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c, Tags d, Tags e, Tags f) => Tags (a, b, c, d, e, f) where
  tagsOf _ =
    tagsOf (Proxy @a)
      `Set.union` tagsOf (Proxy @b)
      `Set.union` tagsOf (Proxy @c)
      `Set.union` tagsOf (Proxy @d)
      `Set.union` tagsOf (Proxy @e)
      `Set.union` tagsOf (Proxy @f)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c, Tags d, Tags e, Tags f, Tags g) => Tags (a, b, c, d, e, f, g) where
  tagsOf _ =
    tagsOf (Proxy @a)
      `Set.union` tagsOf (Proxy @b)
      `Set.union` tagsOf (Proxy @c)
      `Set.union` tagsOf (Proxy @d)
      `Set.union` tagsOf (Proxy @e)
      `Set.union` tagsOf (Proxy @f)
      `Set.union` tagsOf (Proxy @g)

instance {-# OVERLAPPING #-} (Tags a, Tags b, Tags c, Tags d, Tags e, Tags f, Tags g, Tags h)
  => Tags (a, b, c, d, e, f, g, h) where
  tagsOf _ =
    tagsOf (Proxy @a)
      `Set.union` tagsOf (Proxy @b)
      `Set.union` tagsOf (Proxy @c)
      `Set.union` tagsOf (Proxy @d)
      `Set.union` tagsOf (Proxy @e)
      `Set.union` tagsOf (Proxy @f)
      `Set.union` tagsOf (Proxy @g)
      `Set.union` tagsOf (Proxy @h)

data SystemV (msg :: Type) (a :: Type) where
  Sys :: Typeable a =>
    { sysId :: SystemId
    , sysTags :: Set.Set SystemId
    , sysGroups :: Set.Set GroupId
    , sysStep :: Sys msg
    } -> SystemV msg a

type System msg = SystemV msg ()

data AnySystem msg = forall a. AnySystem (SystemV msg a)

anySystem :: SystemV msg a -> AnySystem msg
anySystem = AnySystem

data Inbox a = Inbox
  { eventsI :: Events a
  , doneI :: Done
  , seenI :: Seen
  , allI :: Set.Set SystemId
  , groupsI :: Map.Map GroupId (Set.Set SystemId)
  , valuesI :: Map.Map SystemId Dynamic
  , selfI :: SystemId
  }

events :: Inbox a -> Events a
events = eventsI

done :: Inbox a -> Done
done = doneI

seen :: Inbox a -> Seen
seen = seenI

allIds :: Inbox a -> Set.Set SystemId
allIds = allI

selfId :: Inbox a -> SystemId
selfId = selfI

data Await msg a where
  System :: SystemV msg a -> Await msg a
  Event :: (msg -> Bool) -> Await msg (Events msg)
  Update :: Await msg ()

data Group g = Group

group :: forall g msg a. Typeable g => SystemV msg a -> SystemV msg a
group s = s { sysGroups = Set.insert (typeRep (Proxy @g)) (sysGroups s) }

class Awaitable t msg where
  type AwaitResult t msg
  awaitGateA :: t -> Inbox msg -> (Maybe (AwaitResult t msg), Tick msg -> Tick msg)

instance Awaitable (Await msg a) msg where
  type AwaitResult (Await msg a) msg = a
  awaitGateA = awaitGate

instance Awaitable (SystemV msg a) msg where
  type AwaitResult (SystemV msg a) msg = a
  awaitGateA s = awaitGate (System s)

instance Typeable g => Awaitable (Group g) msg where
  type AwaitResult (Group g) msg = ()
  awaitGateA _ inbox =
    let gid = typeRep (Proxy @g)
        members = Map.findWithDefault Set.empty gid (groupsI inbox)
        others = Set.delete (selfI inbox) members
        synced = others `Set.isSubsetOf` seenI inbox
    in if synced then (Just (), id) else (Nothing, markWait)

instance Awaitable I.InputPred I.Input where
  type AwaitResult I.InputPred I.Input = Events I.Input
  awaitGateA predI = awaitGate (Event (I.matchInput predI))

collect :: E.Query a -> SystemM msg [(Entity, a)]
collect q = Free (Collect q Pure)

await :: Awaitable t msg => t -> SystemM msg (AwaitResult t msg)
await waitOn = Free (Await waitOn Pure)

markWait :: Tick msg -> Tick msg
markWait t = t { waitT = True }

awaitGate :: Await msg a -> Inbox msg -> (Maybe a, Tick msg -> Tick msg)
awaitGate waitOn inbox =
  case waitOn of
    Event p ->
      let hits = Prelude.filter p (eventsI inbox)
      in if null hits
            then (Nothing, markWait)
            else (Just hits, id)
    System s@(Sys{}) ->
      let tags = systemTags s
          ready = not (Set.null (tags `Set.intersection` doneI inbox))
          value = firstJust (Set.toList tags) (\tagId ->
            Map.lookup tagId (valuesI inbox) >>= fromDynamic
            )
      in if not ready
            then (Nothing, markWait)
            else case value of
              Just v -> (Just v, id)
              Nothing -> error "await: value missing for system tag (type mismatch?)"
    Update ->
      let others = Set.delete (selfI inbox) (allI inbox)
          synced = others `Set.isSubsetOf` seenI inbox
      in if synced
            then (Just (), id)
            else (Nothing, markWait)

data Tick a = Tick
  { patchT :: Patch
  , outT :: Events a
  , waitT :: Bool
  , valueT :: Maybe Dynamic
  }

tick :: Patch -> Events a -> Tick a
tick p out = Tick p out False Nothing

wait :: Tick a
wait = Tick mempty [] True Nothing

type Sys a = Step (World, Inbox a) (Tick a)

ensureValue :: Tick a -> Tick a
ensureValue t =
  if waitT t
    then t
    else case valueT t of
      Nothing -> t { valueT = Just (toDyn ()) }
      Just _ -> t

newtype Graph a = Graph [AnySystem a]

class GraphArgs msg r where
  graphWith :: [AnySystem msg] -> r

instance GraphArgs msg (Graph msg) where
  graphWith acc = Graph (reverse acc)

instance GraphArgs msg r => GraphArgs msg (SystemV msg a -> r) where
  graphWith acc s = graphWith (AnySystem s : acc)

sysP :: forall tag msg. Tags tag => (DTime -> World -> Patch) -> System msg
sysP f = sysE @tag (\d w _ -> tick (f d w) [])

sysE :: forall tag msg. Tags tag => (DTime -> World -> Inbox msg -> Tick msg) -> System msg
sysE f =
  let sid = typeRep (Proxy @tag)
      extras = Set.delete sid (tagsOf (Proxy @tag))
  in Sys sid extras Set.empty go
  where
    go = Step $ \d (w, inbox) ->
      let t = ensureValue (f d w inbox)
      in (t, go)

sysS :: forall tag msg. Tags tag => Step (World, Inbox msg) (Tick msg) -> System msg
sysS s =
  let sid = typeRep (Proxy @tag)
      extras = Set.delete sid (tagsOf (Proxy @tag))
  in Sys sid extras Set.empty (ensureStep s)
  where
    ensureStep step0 = Step $ \d wi ->
      let (t, step1) = stepS step0 d wi
      in (ensureValue t, ensureStep step1)

sysM :: forall tag msg a. (Tags tag, Return msg a, Typeable a) => SystemM msg a -> SystemV msg a
sysM m =
  let sid = typeRep (Proxy @tag)
      extras = Set.delete sid (tagsOf (Proxy @tag))
  in Sys sid extras Set.empty go
  where
    go = stepLoop m

    stepLoop prog = Step $ \d (w, inbox) ->
      let (t0, prog', ma) = runSystemM d w inbox prog
          out' = outT t0 <> maybe [] retEvents ma
          t1 = t0 { outT = out' }
          t2 = case ma of
            Nothing -> t1
            Just a -> t1 { valueT = Just (toDyn a) }
          nextProg = case ma of
            Nothing -> prog'
            Just _ -> m
      in (t2, stepLoop nextProg)

system :: forall tag msg a. (Tags tag, Return msg a, Typeable a) => SystemM msg a -> SystemV msg a
system = sysM @tag

graph :: forall msg r. GraphArgs msg r => r
graph = graphWith @msg []

graphList :: [SystemV msg a] -> Graph msg
graphList = Graph . map AnySystem

graphAny :: [AnySystem msg] -> Graph msg
graphAny = Graph

class Eachable a where
  eachPatch :: Entity -> a -> Patch
  default eachPatch :: (Generic a, GEach (Rep a)) => Entity -> a -> Patch
  eachPatch e a = gEach e (from a)

instance {-# OVERLAPPABLE #-} Typeable a => Eachable a where
  eachPatch e a = set e a

class GEach f where
  gEach :: Entity -> f p -> Patch

instance GEach U1 where
  gEach _ _ = mempty

instance GEach f => GEach (M1 i c f) where
  gEach e (M1 f) = gEach e f

instance (GEach a, GEach b) => GEach (a :*: b) where
  gEach e (a :*: b) = gEach e a <> gEach e b

instance EachField a => GEach (K1 i a) where
  gEach e (K1 a) = eachField e a

class EachField a where
  eachField :: Entity -> a -> Patch

instance {-# OVERLAPPABLE #-} Typeable a => EachField a where
  eachField e a = set e a

instance {-# OVERLAPPING #-} Typeable a => EachField (Maybe a) where
  eachField e ma = maybe mempty (set e) ma

instance {-# OVERLAPPING #-} EachField (E.Has a) where
  eachField _ _ = mempty

instance {-# OVERLAPPING #-} EachField (E.Not a) where
  eachField _ _ = mempty

eachW :: Eachable a => E.Query a -> (a -> a) -> World -> Patch
eachW q f w =
  E.foldq q (\e a acc -> acc <> eachPatch e (f a)) mempty w

eachPW :: E.Query a -> (Entity -> a -> Patch) -> World -> Patch
eachPW q f w =
  E.foldq q (\e a acc -> acc <> f e a) mempty w

data Op msg k where
  Each :: Eachable a => E.Query a -> (a -> a) -> k -> Op msg k
  EachP :: E.Query a -> (Entity -> a -> Patch) -> k -> Op msg k
  Edit :: Patch -> k -> Op msg k
  Send :: Events msg -> k -> Op msg k
  Dt :: (DTime -> k) -> Op msg k
  Collect :: E.Query a -> ([(Entity, a)] -> k) -> Op msg k
  Await :: Awaitable t msg => t -> (AwaitResult t msg -> k) -> Op msg k

mapOp :: (k -> k') -> Op msg k -> Op msg k'
mapOp f op =
  case op of
    Each q g k -> Each q g (f k)
    EachP q g k -> EachP q g (f k)
    Edit p k -> Edit p (f k)
    Send out k -> Send out (f k)
    Dt k -> Dt (f . k)
    Collect q k -> Collect q (f . k)
    Await t k -> Await t (f . k)

data SystemM msg a
  = Pure a
  | Free (Op msg (SystemM msg a))

instance Functor (SystemM msg) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free op) = Free (mapOp (fmap f) op)

instance Applicative (SystemM msg) where
  pure = Pure
  (<*>) = ap

instance Monad (SystemM msg) where
  Pure a >>= k = k a
  Free op >>= k = Free (mapOp (>>= k) op)

edit :: Patch -> SystemM msg ()
edit p = Free (Edit p (Pure ()))

send :: Events msg -> SystemM msg ()
send out = Free (Send out (Pure ()))

dt :: SystemM msg DTime
dt = Free (Dt Pure)

each :: Eachable a => E.Query a -> (a -> a) -> SystemM msg ()
each q f = Free (Each q f (Pure ()))

eachP :: E.Query a -> (Entity -> a -> Patch) -> SystemM msg ()
eachP q f = Free (EachP q f (Pure ()))

class Return msg a where
  retEvents :: a -> Events msg

instance {-# OVERLAPPABLE #-} Return msg a where
  retEvents _ = []

instance {-# OVERLAPPING #-} Return msg () where
  retEvents _ = []

instance {-# OVERLAPPING #-} Return msg msg where
  retEvents x = [x]

instance {-# OVERLAPPING #-} Return msg (Events msg) where
  retEvents = id

runSystemM :: DTime -> World -> Inbox msg -> SystemM msg a -> (Tick msg, SystemM msg a, Maybe a)
runSystemM d w inbox = go mempty []
  where
    go patchAcc out prog =
      case prog of
        Pure a -> (Tick patchAcc out False Nothing, Pure a, Just a)
        Free op ->
          case op of
            Edit p k -> go (patchAcc <> p) out k
            Send out' k -> go patchAcc (out <> out') k
            Dt k -> go patchAcc out (k d)
            Each q f k -> go (patchAcc <> eachW q f w) out k
            EachP q f k -> go (patchAcc <> eachPW q f w) out k
            Collect q k -> go patchAcc out (k (E.runq q w))
            Await t k ->
              let (ma, gate) = awaitGateA t inbox
                  baseTick = Tick patchAcc out False Nothing
              in case ma of
                Nothing ->
                  let t0 = gate baseTick
                      t1 = t0 { waitT = True }
                  in (t1, Free (Await t k), Nothing)
                Just a -> go patchAcc out (k a)

run :: DTime -> World -> Events a -> Graph a -> (World, Events a, Graph a)
run d w0 inbox g0 = go w0 (Graph systems0) (repeat True) Set.empty Set.empty Map.empty []
  where
    Graph systems0 = g0
    baseInbox = inbox
    allSet = Set.fromList (map systemId systems0)
    groupMap = buildGroups systems0

    go w (Graph systems) toRun doneSet seenSet values accOut =
      let available = baseInbox ++ accOut
          (w', roundOut, systems', nextRun, done1, seen1, values1, progressedDone, progressedSeen) =
            stepRound d w available doneSet seenSet values allSet groupMap systems toRun
          accOut' = accOut ++ roundOut
          progress = not (null roundOut) || progressedDone || progressedSeen
      in if not (or nextRun) || not progress
          then (w', accOut', Graph systems')
          else go w' (Graph systems') nextRun done1 seen1 values1 accOut'

buildGroups :: [AnySystem a] -> Map.Map GroupId (Set.Set SystemId)
buildGroups =
  foldl' (\m sys0 ->
    Set.foldl' (\m' gid ->
      Map.insertWith Set.union gid (Set.singleton (systemId sys0)) m'
    ) m (systemGroups sys0)
  ) Map.empty

stepRound :: DTime -> World -> Events a -> Done -> Seen -> Map.Map SystemId Dynamic -> Set.Set SystemId -> Map.Map GroupId (Set.Set SystemId) -> [AnySystem a] -> [Bool]
          -> (World, Events a, [AnySystem a], [Bool], Done, Seen, Map.Map SystemId Dynamic, Bool, Bool)
stepRound d w0 events0 done0 seen0 values0 allSet groupMap systems toRun =
  go w0 events0 done0 seen0 values0 [] [] [] False False (zip systems toRun)
  where
    go w _ doneSet seenSet valuesSet accOut accSys accRun progressedDone progressedSeen [] =
      (w, accOut, reverse accSys, reverse accRun, doneSet, seenSet, valuesSet, progressedDone, progressedSeen)
    go w inb doneSet seenSet valuesSet accOut accSys accRun progressedDone progressedSeen
      ((AnySystem (sys0 :: SystemV msg a), runNow) : rest) =
      case sys0 of
        Sys sid tags grps _ ->
          let step0 = sysStep sys0
          in if runNow
            then
              let inbox0 = Inbox inb doneSet seenSet allSet groupMap valuesSet sid
                  (t, s') = stepS step0 d (w, inbox0)
                  w' = apply (patchT t) w
                  out = outT t
                  inb' = inb ++ out
                  accOut' = accOut ++ out
                  accSys' = AnySystem (Sys @a sid tags grps s') : accSys
                  accRun' = waitT t : accRun
                  tagsAll = Set.insert sid tags
                  (doneSet', progressedDone') =
                    if waitT t
                      then (doneSet, progressedDone)
                      else
                        let doneNew = Set.union tagsAll doneSet
                            progressedNew = progressedDone || not (tagsAll `Set.isSubsetOf` doneSet)
                        in (doneNew, progressedNew)
                  seenSet' = Set.union tagsAll seenSet
                  progressedSeen' = progressedSeen || not (tagsAll `Set.isSubsetOf` seenSet)
                  valuesSet' =
                    if waitT t
                      then valuesSet
                      else case valueT t of
                        Nothing -> valuesSet
                        Just v ->
                          Set.foldl' (\m tagId -> Map.insert tagId v m) valuesSet tagsAll
              in go w' inb' doneSet' seenSet' valuesSet' accOut' accSys' accRun' progressedDone' progressedSeen' rest
            else
              go w inb doneSet seenSet valuesSet accOut (AnySystem (Sys @a sid tags grps step0) : accSys) (False : accRun) progressedDone progressedSeen rest

systemId :: AnySystem msg -> SystemId
systemId (AnySystem s) = sysId s

systemGroups :: AnySystem msg -> Set.Set GroupId
systemGroups (AnySystem s) = sysGroups s

systemTags :: SystemV msg a -> Set.Set SystemId
systemTags s = Set.insert (sysId s) (sysTags s)

firstJust :: [a] -> (a -> Maybe b) -> Maybe b
firstJust [] _ = Nothing
firstJust (x : xs) f =
  case f x of
    Just v -> Just v
    Nothing -> firstJust xs f
