{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.System
  ( Patch
  , EntityPatch
  , patch
  , empty
  , SystemId
  , Handle
  , handle
  , handleOf
  , GroupId
  , System
  , SystemV
  , AnySystem
  , anySystem
  , SystemM
  , EntityM
  , MonadSystem
  , Eachable(..)
  , Group(..)
  , group
  , GraphM
  , graphM
  , graphM_
  , systemM
  , sysPM
  , sysEM
  , sysSM
  , addSystem
  , newHandleM
  , Events
  , Inbox
  , events
  , done
  , seen
  , allIds
  , selfId
  , each
  , eachM
  , eachStep
  , eachP
  , edit
  , world
  , send
  , dt
  , time
  , sample
  , step
  , stepE
  , Await(..)
  , Awaitable
  , collect
  , emit
  , emitMany
  , await
  , awaitEvent
  , Tick(..)
  , tick
  , wait
  , set
  , update
  , del
  , setAt
  , updateAt
  , delAt
  , put
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
import Control.Applicative ((<|>))
import Control.Monad (ap)
import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Kind (Type)
import qualified Data.IntMap as IntMap
import Data.Array (Array, (!), listArray)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics
import Data.Proxy (Proxy(..))
import GHC.Conc (numCapabilities)
import Engine.Data.ECS (Entity, World)
import Engine.Data.FRP (DTime, Events, Step(..))
import qualified Engine.Data.FRP as F
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Input as I

newtype Patch c = Patch (World c -> World c)

type Machines = IntMap.IntMap Any

patch :: (World c -> World c) -> Patch c
patch = Patch

newtype EntityPatch c = EntityPatch
  { runEntityPatch :: E.Bag c -> E.Bag c
  }

empty :: Patch c
empty = mempty

instance Semigroup (Patch c) where
  Patch f <> Patch g = Patch (g . f)

instance Monoid (Patch c) where
  mempty = Patch id

instance Semigroup (EntityPatch c) where
  EntityPatch f <> EntityPatch g = EntityPatch (g . f)

instance Monoid (EntityPatch c) where
  mempty = EntityPatch id

apply :: Patch c -> World c -> World c
apply (Patch f) = f

bagGet :: forall a c. E.Component c a => E.Bag c -> Maybe a
bagGet = foldr (\c acc -> E.prj c <|> acc) Nothing

bagSet :: forall a c. E.Component c a => a -> E.Bag c -> E.Bag c
bagSet a = (E.inj a :) . filter (isNothing . E.prj @c @a)

bagDel :: forall a c. E.Component c a => E.Bag c -> E.Bag c
bagDel = filter (isNothing . E.prj @c @a)

set :: forall a c. E.Component c a => a -> EntityPatch c
set a = EntityPatch (bagSet a)

update :: forall a c. E.Component c a => (a -> a) -> EntityPatch c
update f = EntityPatch $ \bag ->
  case bagGet @a bag of
    Nothing -> bag
    Just v -> bagSet (f v) bag

del :: forall a c. E.Component c a => EntityPatch c
del = EntityPatch (bagDel @a)

setAt :: forall a c. E.Component c a => Entity -> a -> Patch c
setAt e a = patch (E.set e a)

updateAt :: forall a c. E.Component c a => Entity -> (a -> a) -> Patch c
updateAt e f = patch $ \w ->
  case E.get @a e w of
    Nothing -> w
    Just v -> E.set e (f v) w

delAt :: forall a c. E.Component c a => Entity -> Patch c
delAt e = patch (E.del @a e)

put :: Typeable a => a -> Patch c
put a = patch (E.put a)

relate :: forall (r :: Type) c. Typeable r => Entity -> Entity -> Patch c
relate a b = patch (E.relate @r a b)

unrelate :: forall (r :: Type) c. Typeable r => Entity -> Entity -> Patch c
unrelate a b = patch (E.unrelate @r a b)

emit :: a -> Events a
emit x = [x]

emitMany :: Events a -> Events a
emitMany = id

type SystemId = Int
type GroupId = TypeRep

newtype Handle (a :: Type) = Handle
  { handleId :: SystemId
  } deriving (Eq, Ord, Show)

type Done = IntSet
type Seen = IntSet

handle :: SystemId -> Handle a
handle = Handle

data Locals c msg = Locals
  { localsGlobal :: IntMap.IntMap Any
  , localsCurrent :: Maybe Entity
  , localsCurrentBag :: Maybe (E.Bag c)
  }

emptyLocals :: Locals c msg
emptyLocals = Locals IntMap.empty Nothing Nothing

data ParCfg = ParCfg
  { parChunk :: Int
  , parMin :: Int
  } deriving (Eq, Show)

defaultParCfg :: ParCfg
defaultParCfg = ParCfg 128 4096

data SystemV c (msg :: Type) (a :: Type) where
  Sys ::
    { sysHandle :: Handle a
    , sysGroups :: Set.Set GroupId
    , sysStep :: Sys c msg
    } -> SystemV c msg a

type System c msg = SystemV c msg ()

data AnySystem c msg = forall a. AnySystem (SystemV c msg a)

anySystem :: SystemV c msg a -> AnySystem c msg
anySystem = AnySystem

handleOf :: SystemV c msg a -> Handle a
handleOf = sysHandle

data Inbox a = Inbox
  { eventsI :: Events a
  , doneI :: Done
  , seenI :: Seen
  , allI :: IntSet
  , groupsI :: Map.Map GroupId IntSet
  , valuesI :: IntMap.IntMap Any
  , selfI :: SystemId
  }

events :: Inbox a -> Events a
events = eventsI

done :: Inbox a -> Done
done = doneI

seen :: Inbox a -> Seen
seen = seenI

allIds :: Inbox a -> IntSet
allIds = allI

selfId :: Inbox a -> SystemId
selfId = selfI

data Await c msg a where
  System :: Handle a -> Await c msg a
  Event :: (msg -> Bool) -> Await c msg (Events msg)
  Update :: Await c msg ()

data Group g = Group

group :: forall g c msg a. Typeable g => SystemV c msg a -> SystemV c msg a
group s = s { sysGroups = Set.insert (typeRep (Proxy @g)) (sysGroups s) }

class Awaitable c t msg where
  type AwaitResult t msg
  awaitGateA :: t -> Inbox msg -> (Maybe (AwaitResult t msg), Tick c msg -> Tick c msg)

instance Awaitable c (Await c msg a) msg where
  type AwaitResult (Await c msg a) msg = a
  awaitGateA = awaitGate

instance Awaitable c (Handle a) msg where
  type AwaitResult (Handle a) msg = a
  awaitGateA h = awaitGate (System h)

instance Typeable g => Awaitable c (Group g) msg where
  type AwaitResult (Group g) msg = ()
  awaitGateA _ inbox =
    let gid = typeRep (Proxy @g)
        members = Map.findWithDefault IntSet.empty gid (groupsI inbox)
        others = IntSet.delete (selfI inbox) members
        synced = others `IntSet.isSubsetOf` seenI inbox
    in if synced then (Just (), id) else (Nothing, markWait)

instance Awaitable c I.InputPred I.Input where
  type AwaitResult I.InputPred I.Input = Events I.Input
  awaitGateA predI = awaitGate (Event (I.matchInput predI))

collect :: E.Query c a -> SystemM c msg [(Entity, a)]
collect q = Free (Collect q Pure)

awaitEvent :: forall c msg m. MonadSystem c msg m => (msg -> Bool) -> m (Events msg)
awaitEvent p = await (Event @msg @c p)

markWait :: Tick c msg -> Tick c msg
markWait t = t { waitT = True }

awaitGate :: Await c msg a -> Inbox msg -> (Maybe a, Tick c msg -> Tick c msg)
awaitGate waitOn inbox =
  case waitOn of
    Event p ->
      let hits = Prelude.filter p (eventsI inbox)
      in if null hits
            then (Nothing, markWait)
            else (Just hits, id)
    System h ->
      let sid = handleId h
          ready = IntSet.member sid (doneI inbox)
          value = IntMap.lookup sid (valuesI inbox)
      in if not ready
            then (Nothing, markWait)
            else case value of
              Just v -> (Just (unsafeCoerce v), id)
              Nothing -> error "await: value missing for system handle (type mismatch?)"
    Update ->
      let others = IntSet.delete (selfI inbox) (allI inbox)
          synced = others `IntSet.isSubsetOf` seenI inbox
      in if synced
            then (Just (), id)
            else (Nothing, markWait)

data Tick c a = Tick
  { patchT :: Patch c
  , outT :: Events a
  , waitT :: Bool
  , valueT :: Maybe Any
  }

tick :: Patch c -> Events a -> Tick c a
tick p out = Tick p out False Nothing

wait :: Tick c a
wait = Tick mempty [] True Nothing

type Sys c a = Step (World c, Inbox a) (Tick c a)

ensureValue :: Tick c a -> Tick c a
ensureValue t =
  if waitT t
    then t
    else case valueT t of
      Nothing -> t { valueT = Just (unsafeCoerce ()) }
      Just _ -> t

newtype Graph c a = Graph [AnySystem c a]

class GraphArgs c msg r | r -> c msg where
  graphWith :: [AnySystem c msg] -> r

instance GraphArgs c msg (Graph c msg) where
  graphWith acc = Graph (reverse acc)

instance GraphArgs c msg r => GraphArgs c msg (SystemV c msg a -> r) where
  graphWith acc s = graphWith (AnySystem s : acc)

sysP :: Handle () -> (DTime -> World c -> Patch c) -> System c msg
sysP h f = sysE h (\d w _ -> tick (f d w) [])

sysE :: Handle () -> (DTime -> World c -> Inbox msg -> Tick c msg) -> System c msg
sysE h f = Sys h Set.empty go
  where
    go = Step $ \d (w, inbox) ->
      let t = ensureValue (f d w inbox)
      in (t, go)

sysS :: Handle () -> Step (World c, Inbox msg) (Tick c msg) -> System c msg
sysS h s = Sys h Set.empty (ensureStep s)
  where
    ensureStep step0 = Step $ \d wi ->
      let (t, step1) = stepS step0 d wi
      in (ensureValue t, ensureStep step1)

sysMWith :: Return msg a => Handle a -> SystemM c msg a -> SystemV c msg a
sysMWith h m = Sys h Set.empty go
  where
    go = stepLoop emptyLocals m

    stepLoop locals prog = Step $ \d (w, inbox) ->
      let localsBase =
            locals
              { localsCurrent = Nothing
              , localsCurrentBag = Nothing
              }
          (t0, locals', prog', ma) = runSystemMWith d w inbox localsBase prog
          localsNext =
            locals'
              { localsCurrent = Nothing
              , localsCurrentBag = Nothing
              }
          out' = outT t0 <> maybe [] retEvents ma
          t1 = t0 { outT = out' }
          t2 = case ma of
            Nothing -> t1
            Just a -> t1 { valueT = Just (unsafeCoerce a) }
          nextProg = case ma of
            Nothing -> prog'
            Just _ -> m
      in (t2, stepLoop localsNext nextProg)

sysM :: Return msg a => Handle a -> SystemM c msg a -> SystemV c msg a
sysM h = sysMWith h

system :: Return msg a => Handle a -> SystemM c msg a -> SystemV c msg a
system = sysM

graph :: forall msg c r. GraphArgs c msg r => r
graph = graphWith @c @msg []

graphList :: [SystemV c msg a] -> Graph c msg
graphList = Graph . map AnySystem

graphAny :: [AnySystem c msg] -> Graph c msg
graphAny = Graph

data GraphState c msg = GraphState
  { gsNext :: !Int
  , gsSystems :: ![AnySystem c msg]
  }

newtype GraphM c msg a = GraphM
  { runGraphM :: GraphState c msg -> (GraphState c msg, a)
  }

instance Functor (GraphM c msg) where
  fmap f (GraphM g) = GraphM $ \s ->
    let (s', a) = g s
    in (s', f a)

instance Applicative (GraphM c msg) where
  pure a = GraphM (\s -> (s, a))
  (<*>) = ap

instance Monad (GraphM c msg) where
  GraphM g >>= k = GraphM $ \s ->
    let (s', a) = g s
    in runGraphM (k a) s'

newHandleM :: GraphM c msg (Handle a)
newHandleM = GraphM $ \s ->
  let n = gsNext s
  in (s { gsNext = n + 1 }, Handle n)

addSystem :: SystemV c msg a -> GraphM c msg (Handle a)
addSystem sys = GraphM $ \s ->
  let s' = s { gsSystems = AnySystem sys : gsSystems s }
  in (s', handleOf sys)

systemM :: Return msg a => SystemM c msg a -> GraphM c msg (Handle a)
systemM m = do
  h <- newHandleM
  _ <- addSystem (system h m)
  pure h

sysPM :: (DTime -> World c -> Patch c) -> GraphM c msg (Handle ())
sysPM f = do
  h <- newHandleM
  _ <- addSystem (sysP h f)
  pure h

sysEM :: (DTime -> World c -> Inbox msg -> Tick c msg) -> GraphM c msg (Handle ())
sysEM f = do
  h <- newHandleM
  _ <- addSystem (sysE h f)
  pure h

sysSM :: Step (World c, Inbox msg) (Tick c msg) -> GraphM c msg (Handle ())
sysSM s = do
  h <- newHandleM
  _ <- addSystem (sysS h s)
  pure h

graphM :: GraphM c msg a -> (a, Graph c msg)
graphM m =
  let (s, a) = runGraphM m (GraphState 0 [])
  in (a, Graph (reverse (gsSystems s)))

graphM_ :: GraphM c msg () -> Graph c msg
graphM_ = snd . graphM

class Eachable c a where
  eachPatch :: a -> EntityPatch c
  default eachPatch :: (Generic a, GEach c (Rep a)) => a -> EntityPatch c
  eachPatch a = gEach (from a)

instance {-# OVERLAPPABLE #-} E.Component c a => Eachable c a where
  eachPatch = set

class GEach c f where
  gEach :: f p -> EntityPatch c

instance GEach c U1 where
  gEach _ = mempty

instance GEach c f => GEach c (M1 i m f) where
  gEach (M1 f) = gEach f

instance (GEach c a, GEach c b) => GEach c (a :*: b) where
  gEach (a :*: b) = gEach a <> gEach b

instance EachField c a => GEach c (K1 i a) where
  gEach (K1 a) = eachField a

class EachField c a where
  eachField :: a -> EntityPatch c

instance {-# OVERLAPPABLE #-} E.Component c a => EachField c a where
  eachField = set

instance {-# OVERLAPPING #-} E.Component c a => EachField c (Maybe a) where
  eachField ma = maybe mempty set ma

instance {-# OVERLAPPING #-} EachField c (E.Has a) where
  eachField _ = mempty

instance {-# OVERLAPPING #-} EachField c (E.Not a) where
  eachField _ = mempty


eachWWith :: Eachable c a => Maybe ParCfg -> E.Query c a -> (a -> a) -> World c -> Patch c
eachWWith parCfg q f w =
  case parCfg of
    Nothing ->
      let rowsRev =
            E.foldEntities
              (\e bag acc ->
                case E.runQuery q e bag of
                  Nothing -> (e, bag) : acc
                  Just a ->
                    let bag' = runEntityPatch (eachPatch (f a)) bag
                    in (e, bag') : acc
              ) [] w
      in patch (E.setEntities (reverse rowsRev))
    Just pcfg ->
      let (nRows, rowsAccRev) =
            E.foldEntities
              (\e bag (n, acc) -> (n + 1, (e, bag) : acc))
              (0 :: Int, [])
              w
          rows = reverse rowsAccRev
          applyRow (e, bag) =
            case E.runQuery q e bag of
              Nothing -> (e, bag)
              Just a ->
                let bag' = runEntityPatch (eachPatch (f a)) bag
                in (e, bag')
          seqRows = map applyRow rows
      in if nRows <= 0
          then patch (E.setEntities [])
          else if nRows < parMin pcfg
            then patch (E.setEntities seqRows)
            else
              let rowsArr = listArray (0, nRows - 1) rows
                  ranges = [(i, min (i + parChunk pcfg - 1) (nRows - 1)) | i <- [0, parChunk pcfg .. nRows - 1]]
                  evalChunk (lo, hi) =
                    let goChunk i acc
                          | i > hi = reverse acc
                          | otherwise =
                              let row = rowsArr ! i
                                  row' = applyRow row
                              in goChunk (i + 1) (row' : acc)
                    in goChunk lo []
                  chunkResults0 = map evalChunk ranges
                  chunkResults = withStrategy (parListChunk 1 rseq) chunkResults0
                  rowsFinal = concat chunkResults
              in patch (E.setEntities rowsFinal)

eachPWWith :: Maybe ParCfg -> E.Query c a -> (a -> EntityPatch c) -> World c -> Patch c
eachPWWith parCfg q f w =
  case parCfg of
    Nothing ->
      let rowsRev =
            E.foldEntities
              (\e bag acc ->
                case E.runQuery q e bag of
                  Nothing -> (e, bag) : acc
                  Just a ->
                    let bag' = runEntityPatch (f a) bag
                    in (e, bag') : acc
              ) [] w
      in patch (E.setEntities (reverse rowsRev))
    Just pcfg ->
      let (nRows, rowsAccRev) =
            E.foldEntities
              (\e bag (n, acc) -> (n + 1, (e, bag) : acc))
              (0 :: Int, [])
              w
          rows = reverse rowsAccRev
          applyRow (e, bag) =
            case E.runQuery q e bag of
              Nothing -> (e, bag)
              Just a ->
                let bag' = runEntityPatch (f a) bag
                in (e, bag')
          seqRows = map applyRow rows
      in if nRows <= 0
          then patch (E.setEntities [])
          else if nRows < parMin pcfg
            then patch (E.setEntities seqRows)
            else
              let rowsArr = listArray (0, nRows - 1) rows
                  ranges = [(i, min (i + parChunk pcfg - 1) (nRows - 1)) | i <- [0, parChunk pcfg .. nRows - 1]]
                  evalChunk (lo, hi) =
                    let goChunk i acc
                          | i > hi = reverse acc
                          | otherwise =
                              let row = rowsArr ! i
                                  row' = applyRow row
                              in goChunk (i + 1) (row' : acc)
                    in goChunk lo []
                  chunkResults0 = map evalChunk ranges
                  chunkResults = withStrategy (parListChunk 1 rseq) chunkResults0
                  rowsFinal = concat chunkResults
              in patch (E.setEntities rowsFinal)

data Op c msg k where
  Each :: Eachable c a => E.Query c a -> (a -> a) -> k -> Op c msg k
  EachP :: E.Query c a -> (a -> EntityPatch c) -> k -> Op c msg k
  EachM :: Typeable msg => E.TypeId -> E.Query c a -> (a -> EntityM c msg ()) -> k -> Op c msg k
  EachS :: (Typeable a, Typeable b) => E.TypeId -> E.Query c a -> F.Step a b -> (a -> b -> EntityPatch c) -> k -> Op c msg k
  StepOp :: (Typeable a, Typeable b) => E.TypeId -> F.Step a b -> a -> (b -> k) -> Op c msg k
  EditW :: Patch c -> k -> Op c msg k
  EditE :: EntityPatch c -> k -> Op c msg k
  Send :: Events msg -> k -> Op c msg k
  Dt :: (DTime -> k) -> Op c msg k
  Local :: Typeable s => E.TypeId -> s -> (s -> (a, s)) -> (a -> k) -> Op c msg k
  LocalE :: Typeable s => E.TypeId -> Entity -> s -> (s -> (a, s)) -> (a -> k) -> Op c msg k
  Collect :: E.Query c a -> ([(Entity, a)] -> k) -> Op c msg k
  Await :: Awaitable c t msg => t -> (AwaitResult t msg -> k) -> Op c msg k

mapOp :: (k -> k') -> Op c msg k -> Op c msg k'
mapOp f op =
  case op of
    Each q g k -> Each q g (f k)
    EachP q g k -> EachP q g (f k)
    EachM key q g k -> EachM key q g (f k)
    EachS key q s g k -> EachS key q s g (f k)
    StepOp key s a k -> StepOp key s a (f . k)
    EditW p k -> EditW p (f k)
    EditE p k -> EditE p (f k)
    Send out k -> Send out (f k)
    Dt k -> Dt (f . k)
    Local key def g k -> Local key def g (f . k)
    LocalE key e def g k -> LocalE key e def g (f . k)
    Collect q k -> Collect q (f . k)
    Await t k -> Await t (f . k)

data SystemM c msg a
  = Pure a
  | Free (Op c msg (SystemM c msg a))

instance Functor (SystemM c msg) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free op) = Free (mapOp (fmap f) op)

instance Applicative (SystemM c msg) where
  pure = Pure
  (<*>) = ap

instance Monad (SystemM c msg) where
  Pure a >>= k = k a
  Free op >>= k = Free (mapOp (>>= k) op)

newtype Out msg = Out ([msg] -> [msg])

instance Semigroup (Out msg) where
  Out f <> Out g = Out (f . g)

instance Monoid (Out msg) where
  mempty = Out id

outFrom :: Events msg -> Out msg
outFrom xs = Out (xs ++)

outToList :: Out msg -> Events msg
outToList (Out f) = f []

data EntityCtx c msg = EntityCtx
  { ctxBag :: !(E.Bag c)
  , ctxMachines :: !Machines
  , ctxPatch :: !(EntityPatch c)
  , ctxOut :: !(Out msg)
  , ctxDt :: !DTime
  , ctxInbox :: !(Inbox msg)
  }

data StepRes c msg a
  = Done a
  | Wait (EntityM c msg a)

newtype EntityM c msg a = EntityM
  { unEntityM :: EntityCtx c msg -> (EntityCtx c msg, StepRes c msg a)
  }

instance Functor (EntityM c msg) where
  fmap f (EntityM g) = EntityM $ \ctx ->
    case g ctx of
      (ctx', Done a) -> (ctx', Done (f a))
      (ctx', Wait p) -> (ctx', Wait (fmap f p))

instance Applicative (EntityM c msg) where
  pure a = EntityM $ \ctx -> (ctx, Done a)
  (<*>) = ap

instance Monad (EntityM c msg) where
  EntityM g >>= k = EntityM $ \ctx ->
    case g ctx of
      (ctx', Done a) -> unEntityM (k a) ctx'
      (ctx', Wait p) -> (ctx', Wait (p >>= k))

class Monad m => MonadSystem c msg m | m -> c msg where
  edit :: EntityPatch c -> m ()
  send :: Events msg -> m ()
  dt :: m DTime
  await :: Awaitable c t msg => t -> m (AwaitResult t msg)
  stepId :: (Typeable a, Typeable b) => E.TypeId -> F.Step a b -> a -> m b

instance MonadSystem c msg (SystemM c msg) where
  edit p = Free (EditE p (Pure ()))
  send out = Free (Send out (Pure ()))
  dt = Free (Dt Pure)
  await waitOn = Free (Await waitOn Pure)
  stepId key s0 a = Free (StepOp key s0 a Pure)

instance MonadSystem c msg (EntityM c msg) where
  edit p = EntityM $ \ctx ->
    let patch' = ctxPatch ctx <> p
    in (ctx { ctxPatch = patch' }, Done ())
  send out = EntityM $ \ctx ->
    let out' = ctxOut ctx <> outFrom out
    in (ctx { ctxOut = out' }, Done ())
  dt = EntityM $ \ctx -> (ctx, Done (ctxDt ctx))
  await waitOn = EntityM $ \ctx ->
    case awaitGateA @c waitOn (ctxInbox ctx) of
      (Nothing, _) -> (ctx, Wait (await waitOn))
      (Just a, _) -> (ctx, Done a)
  stepId key s0 a = EntityM $ \ctx ->
    let machines0 = ctxMachines ctx
        s = case IntMap.lookup key machines0 of
          Just v -> unsafeCoerce v
          Nothing -> s0
        (b, s') = stepS s (ctxDt ctx) a
        machines' = IntMap.insert key (unsafeCoerce s') machines0
    in (ctx { ctxMachines = machines' }, Done b)

world :: Patch c -> SystemM c msg ()
world p = Free (EditW p (Pure ()))

localStateE :: forall key s c msg a. (Typeable key, Typeable s) => Entity -> s -> (s -> (a, s)) -> SystemM c msg a
localStateE e s0 f =
  let key = E.typeIdOf @key
  in Free (LocalE key e s0 f Pure)

data TimeKey
data Program (key :: Type)

time :: MonadSystem c msg m => m F.Time
time = step @TimeKey F.time ()

sample :: MonadSystem c msg m => F.Tween a -> m a
sample tw = do
  t <- time
  pure (F.at tw t)

step :: forall key a b c msg m. (MonadSystem c msg m, Typeable key, Typeable a, Typeable b) => F.Step a b -> a -> m b
step s0 a = stepId (E.typeIdOf @key) s0 a

stepE :: forall key a b c msg. (Typeable key, Typeable a, Typeable b) => Entity -> F.Step a b -> a -> SystemM c msg b
stepE e s0 a = do
  d <- dt
  localStateE @key e s0 (\s -> stepS s d a)

each :: Eachable c a => E.Query c a -> (a -> a) -> SystemM c msg ()
each q f = Free (Each q f (Pure ()))

eachM :: forall (key :: Type) c msg a. (Typeable key, Typeable msg) => E.Query c a -> (a -> EntityM c msg ()) -> SystemM c msg ()
eachM q f = Free (EachM (E.typeIdOf @(Program key)) q f (Pure ()))

eachStep :: forall (key :: Type) c a b msg. (Typeable key, Typeable a, Typeable b) => E.Query c a -> F.Step a b -> (a -> b -> EntityPatch c) -> SystemM c msg ()
eachStep q s f = Free (EachS (E.typeIdOf @key) q s f (Pure ()))

eachP :: E.Query c a -> (a -> EntityPatch c) -> SystemM c msg ()
eachP q f = Free (EachP q f (Pure ()))

class Return msg a where
  retEvents :: a -> Events msg

instance {-# OVERLAPPABLE #-} Return msg a where
  retEvents _ = []

instance {-# OVERLAPPING #-} Return msg msg where
  retEvents x = [x]

instance {-# OVERLAPPING #-} Return msg (Events msg) where
  retEvents = id

runEntityM :: forall c msg a. DTime -> Inbox msg -> E.Bag c -> Machines -> EntityM c msg a
  -> (E.Bag c, Machines, Out msg, Bool, EntityM c msg a, Maybe a)
runEntityM d inbox bag0 machines0 prog0 =
  let ctx0 =
        EntityCtx
          { ctxBag = bag0
          , ctxMachines = machines0
          , ctxPatch = mempty
          , ctxOut = mempty
          , ctxDt = d
          , ctxInbox = inbox
          }
      (ctx1, res) = unEntityM prog0 ctx0
      bag' = runEntityPatch (ctxPatch ctx1) (ctxBag ctx1)
      machines' = ctxMachines ctx1
      out' = ctxOut ctx1
  in case res of
    Done a -> (bag', machines', out', False, prog0, Just a)
    Wait prog' -> (bag', machines', out', True, prog', Nothing)

data ProgState c msg = ProgState
  !(Maybe (EntityM c msg ()))
  !Machines

data ChunkRes c msg = ChunkRes
  { crOut :: !(Out msg)
  , crWait :: !Bool
  , crRows :: ![(Entity, E.Bag c)]
  , crProg :: !(IntMap.IntMap Any)
  }

runSystemMWith :: DTime -> World c -> Inbox msg -> Locals c msg -> SystemM c msg a -> (Tick c msg, Locals c msg, SystemM c msg a, Maybe a)
runSystemMWith d w inbox locals0 = go mempty [] False locals0
  where
    parCfgMaybe =
      if numCapabilities > 1 then Just defaultParCfg else Nothing
    go patchAcc out waitAcc locals prog =
      case prog of
        Pure a -> (Tick patchAcc out waitAcc Nothing, locals, Pure a, Just a)
        Free op ->
          case op of
            EditW p k -> go (patchAcc <> p) out waitAcc locals k
            EditE p k ->
              case localsCurrent locals of
                Nothing -> go patchAcc out waitAcc locals k
                Just _ ->
                  let bag0 = case localsCurrentBag locals of
                        Just bag -> bag
                        Nothing -> []
                      bag' = runEntityPatch p bag0
                      locals' =
                        locals
                          { localsCurrentBag = Just bag' }
                  in go patchAcc out waitAcc locals' k
            Send out' k -> go patchAcc (out <> out') waitAcc locals k
            Dt k -> go patchAcc out waitAcc locals (k d)
            Local key def f k ->
              let s = case IntMap.lookup key (localsGlobal locals) of
                    Just v -> unsafeCoerce v
                    Nothing -> def
                  (a, s') = f s
                  globals' = IntMap.insert key (unsafeCoerce s') (localsGlobal locals)
                  locals' = locals { localsGlobal = globals' }
              in go patchAcc out waitAcc locals' (k a)
            LocalE key e def f k ->
              let eid' = E.eid e
                  store0 = case IntMap.lookup key (localsGlobal locals) of
                    Just v -> unsafeCoerce v
                    Nothing -> IntMap.empty
                  s = case IntMap.lookup eid' store0 of
                    Just v -> unsafeCoerce v
                    Nothing -> def
                  (a, s') = f s
                  store' = IntMap.insert eid' (unsafeCoerce s') store0
                  globals' = IntMap.insert key (unsafeCoerce store') (localsGlobal locals)
                  locals' = locals { localsGlobal = globals' }
              in go patchAcc out waitAcc locals' (k a)
            StepOp keyRep s0 a k ->
              case localsCurrent locals of
                Nothing ->
                  let s = case IntMap.lookup keyRep (localsGlobal locals) of
                        Just v -> unsafeCoerce v
                        Nothing -> s0
                      (b, s') = stepS s d a
                      globals' = IntMap.insert keyRep (unsafeCoerce s') (localsGlobal locals)
                      locals' = locals { localsGlobal = globals' }
                  in go patchAcc out waitAcc locals' (k b)
                Just e ->
                  let eid' = E.eid e
                      store0 = case IntMap.lookup keyRep (localsGlobal locals) of
                        Just v -> unsafeCoerce v
                        Nothing -> IntMap.empty
                      s = case IntMap.lookup eid' store0 of
                        Just v -> unsafeCoerce v
                        Nothing -> s0
                      (b, s') = stepS s d a
                      store' = IntMap.insert eid' (unsafeCoerce s') store0
                      globals' = IntMap.insert keyRep (unsafeCoerce store') (localsGlobal locals)
                      locals' = locals { localsGlobal = globals' }
                  in go patchAcc out waitAcc locals' (k b)
            Each q f k -> go (patchAcc <> eachWWith parCfgMaybe q f w) out waitAcc locals k
            EachP q f k -> go (patchAcc <> eachPWWith parCfgMaybe q f w) out waitAcc locals k
            EachM key (q :: E.Query c a) (f :: a -> EntityM c msg ()) k ->
              let progKey = key
                  outStart = outFrom out
                  progMap0 :: IntMap.IntMap Any
                  progMap0 =
                    case IntMap.lookup progKey (localsGlobal locals) of
                      Just v -> unsafeCoerce v
                      Nothing -> IntMap.empty
                  stepRowSeq e bag (outAcc, waitAcc', rowsAcc, progAcc) =
                    case E.runQuery q e bag of
                      Nothing -> (outAcc, waitAcc', (e, bag) : rowsAcc, progAcc)
                      Just a ->
                        let eid' = E.eid e
                            (prog0, machines0) = case IntMap.lookup eid' progMap0 of
                              Just progStored ->
                                let ProgState mProgStored machinesStored = unsafeCoerce progStored
                                    prog0' = case mProgStored of
                                      Just p -> p
                                      Nothing -> f a
                                in (prog0', machinesStored)
                              Nothing -> (f a, IntMap.empty)
                            (bag', machines', out', waitE, prog', _) =
                              runEntityM d inbox bag machines0 prog0
                            progKeep = waitE || not (IntMap.null machines')
                            progState = ProgState (if waitE then Just prog' else Nothing) machines'
                            progAcc' =
                              if progKeep
                                then IntMap.insert eid' (unsafeCoerce progState) progAcc
                                else progAcc
                            outAcc' = outAcc <> out'
                            waitAcc'' = waitAcc' || waitE
                        in (outAcc', waitAcc'', (e, bag') : rowsAcc, progAcc')
                  seqResult =
                    E.foldEntities stepRowSeq (outStart, waitAcc, [], IntMap.empty) w
                  parResult pcfg =
                    let (nRows, rowsAccRev) =
                          E.foldEntities
                            (\e bag (n, acc) -> (n + 1, (e, bag) : acc))
                            (0 :: Int, [])
                            w
                        rows = reverse rowsAccRev
                        rowsArr =
                          if nRows == 0
                            then listArray (0, -1) []
                            else listArray (0, nRows - 1) rows
                        ranges =
                          if nRows <= 0
                            then []
                            else [(i, min (i + parChunk pcfg - 1) (nRows - 1)) | i <- [0, parChunk pcfg .. nRows - 1]]
                        evalChunk (lo, hi) =
                          let goChunk i (outAcc, waitAcc', rowsAcc, progAcc)
                                | i > hi = (outAcc, waitAcc', rowsAcc, progAcc)
                                | otherwise =
                                    let (e, bag) = rowsArr ! i
                                    in case E.runQuery q e bag of
                                        Nothing -> goChunk (i + 1) (outAcc, waitAcc', (e, bag) : rowsAcc, progAcc)
                                        Just a ->
                                          let eid' = E.eid e
                                              (prog0, machines0) = case IntMap.lookup eid' progMap0 of
                                                Just progStored ->
                                                  let ProgState mProgStored machinesStored = unsafeCoerce progStored
                                                      prog0' = case mProgStored of
                                                        Just p -> p
                                                        Nothing -> f a
                                                  in (prog0', machinesStored)
                                                Nothing -> (f a, IntMap.empty)
                                              (bag', machines', out', waitE, prog', _) =
                                                runEntityM d inbox bag machines0 prog0
                                              progKeep = waitE || not (IntMap.null machines')
                                              progState = ProgState (if waitE then Just prog' else Nothing) machines'
                                              progAcc' =
                                                if progKeep
                                                  then IntMap.insert eid' (unsafeCoerce progState) progAcc
                                                  else progAcc
                                          in goChunk (i + 1) (outAcc <> out', waitAcc' || waitE, (e, bag') : rowsAcc, progAcc')
                              (outC, waitC, rowsCRev, progC) =
                                goChunk lo (mempty, False, [], IntMap.empty)
                          in ChunkRes outC waitC (reverse rowsCRev) progC
                        chunkResults0 = map evalChunk ranges
                        chunkResults =
                          if nRows >= parMin pcfg
                            then withStrategy (parListChunk 1 rseq) chunkResults0
                            else chunkResults0
                        stepChunk (outAcc, waitAcc', rowsChunksAcc, progAcc) cr =
                          let outAcc' = outAcc <> crOut cr
                              waitAcc'' = waitAcc' || crWait cr
                              progAcc' = IntMap.union progAcc (crProg cr)
                          in (outAcc', waitAcc'', crRows cr : rowsChunksAcc, progAcc')
                        (outPar, waitPar, rowsChunksRev, progFinal) =
                          foldl' stepChunk (outStart, waitAcc, [], IntMap.empty) chunkResults
                        rowsFinal = concat (reverse rowsChunksRev)
                    in (outPar, waitPar, rowsFinal, progFinal)
                  (outFinal, waitFinal, rowsRev, progNext) =
                    case parCfgMaybe of
                      Nothing -> seqResult
                      Just pcfg -> parResult pcfg
                  patchRows = patch (E.setEntities (reverse rowsRev))
                  globals' =
                    if IntMap.null progNext
                      then IntMap.delete progKey (localsGlobal locals)
                      else IntMap.insert progKey (unsafeCoerce progNext) (localsGlobal locals)
                  locals' = locals { localsGlobal = globals' }
              in go (patchAcc <> patchRows) (outToList outFinal) waitFinal locals' k
            EachS key (q :: E.Query c a) (s0 :: F.Step a b) (f :: a -> b -> EntityPatch c) k ->
              let stepMap0 = case IntMap.lookup key (localsGlobal locals) of
                    Just v -> unsafeCoerce v
                    Nothing -> IntMap.empty
                  stepRow e bag (rowsAcc, stepAcc) =
                    case E.runQuery q e bag of
                      Nothing -> ((e, bag) : rowsAcc, stepAcc)
                      Just a ->
                        let eid' = E.eid e
                            s = case IntMap.lookup eid' stepMap0 of
                              Just v -> unsafeCoerce v
                              Nothing -> s0
                            (b, s') = stepS s d a
                            stepAcc' = IntMap.insert eid' (unsafeCoerce s') stepAcc
                            bag' = runEntityPatch (f a b) bag
                        in ((e, bag') : rowsAcc, stepAcc')
                  seqResult =
                    let (rowsRev, stepMapSeq) = E.foldEntities stepRow ([], IntMap.empty) w
                    in (reverse rowsRev, stepMapSeq)
                  parResult pcfg =
                    let (nRows, rowsAccRev) =
                          E.foldEntities
                            (\e bag (n, acc) -> (n + 1, (e, bag) : acc))
                            (0 :: Int, [])
                            w
                        rows = reverse rowsAccRev
                        rowsArr :: Array Int (Entity, E.Bag c)
                        rowsArr =
                          if nRows == 0
                            then listArray (0, -1) []
                            else listArray (0, nRows - 1) rows
                        ranges =
                          if nRows <= 0
                            then []
                            else [(i, min (i + parChunk pcfg - 1) (nRows - 1)) | i <- [0, parChunk pcfg .. nRows - 1]]
                        evalChunk (lo, hi) =
                          let goChunk i (rowsAcc, stepAcc)
                                | i > hi = (reverse rowsAcc, stepAcc)
                                | otherwise =
                                    let (e, bag) = rowsArr ! i
                                    in case E.runQuery q e bag of
                                        Nothing -> goChunk (i + 1) ((e, bag) : rowsAcc, stepAcc)
                                        Just a ->
                                          let eid' = E.eid e
                                              s = case IntMap.lookup eid' stepMap0 of
                                                Just v -> unsafeCoerce v
                                                Nothing -> s0
                                              (b, s') = stepS s d a
                                              stepAcc' = IntMap.insert eid' (unsafeCoerce s') stepAcc
                                              bag' = runEntityPatch (f a b) bag
                                          in goChunk (i + 1) ((e, bag') : rowsAcc, stepAcc')
                              (rowsCRev, stepC) = goChunk lo ([], IntMap.empty)
                          in (rowsCRev, stepC)
                        chunkResults0 = map evalChunk ranges
                        chunkResults =
                          if nRows >= parMin pcfg
                            then withStrategy (parListChunk 1 rseq) chunkResults0
                            else chunkResults0
                        stepChunk (rowsChunksAcc, stepAcc) (rowsC, stepC) =
                          (rowsC : rowsChunksAcc, IntMap.union stepAcc stepC)
                        (rowsChunksRev, stepMapPar) =
                          foldl' stepChunk ([], IntMap.empty) chunkResults
                        rowsFinalPar = concat (reverse rowsChunksRev)
                    in (rowsFinalPar, stepMapPar)
                  (rowsFinal, stepMapFinal) =
                    case parCfgMaybe of
                      Nothing -> seqResult
                      Just pcfg ->
                        let (rowsPar, stepPar) = parResult pcfg
                        in (rowsPar, stepPar)
                  globals' =
                    if IntMap.null stepMapFinal
                      then IntMap.delete key (localsGlobal locals)
                      else IntMap.insert key (unsafeCoerce stepMapFinal) (localsGlobal locals)
                  locals' = locals { localsGlobal = globals' }
                  p = patch (E.setEntities rowsFinal)
              in go (patchAcc <> p) out waitAcc locals' k
            Collect q k -> go patchAcc out waitAcc locals (k (E.runq q w))
            Await t k ->
              let (ma, gate) = awaitGateA t inbox
                  baseTick = Tick patchAcc out waitAcc Nothing
              in case ma of
                Nothing ->
                  let t0 = gate baseTick
                      t1 = t0 { waitT = True }
                  in (t1, locals, Free (Await t k), Nothing)
                Just a -> go patchAcc out waitAcc locals (k a)


run :: DTime -> World c -> Events a -> Graph c a -> (World c, Events a, Graph c a)
run d w0 inbox g0 = go w0 (Graph systems0) (repeat True) IntSet.empty IntSet.empty IntMap.empty []
  where
    Graph systems0 = g0
    baseInbox = inbox
    allSet = IntSet.fromList (map systemId systems0)
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

buildGroups :: [AnySystem c a] -> Map.Map GroupId IntSet
buildGroups =
  foldl' (\m sys0 ->
    Set.foldl' (\m' gid ->
      Map.insertWith IntSet.union gid (IntSet.singleton (systemId sys0)) m'
    ) m (systemGroups sys0)
  ) Map.empty

stepRound :: DTime -> World c -> Events a -> Done -> Seen -> IntMap.IntMap Any -> IntSet -> Map.Map GroupId IntSet -> [AnySystem c a] -> [Bool]
          -> (World c, Events a, [AnySystem c a], [Bool], Done, Seen, IntMap.IntMap Any, Bool, Bool)
stepRound d w0 events0 done0 seen0 values0 allSet groupMap systems toRun =
  go w0 events0 done0 seen0 values0 [] [] [] False False (zip systems toRun)
  where
    go w _ doneSet seenSet valuesSet accOut accSys accRun progressedDone progressedSeen [] =
      (w, accOut, reverse accSys, reverse accRun, doneSet, seenSet, valuesSet, progressedDone, progressedSeen)
    go w inb doneSet seenSet valuesSet accOut accSys accRun progressedDone progressedSeen
      ((AnySystem (sys0 :: SystemV c msg a), runNow) : rest) =
      case sys0 of
        Sys h grps _ ->
          let step0 = sysStep sys0
              sid = handleId h
          in if runNow
            then
              let inbox0 = Inbox inb doneSet seenSet allSet groupMap valuesSet sid
                  (t, s') = stepS step0 d (w, inbox0)
                  w' = apply (patchT t) w
                  out = outT t
                  inb' = inb ++ out
                  accOut' = accOut ++ out
                  accSys' = AnySystem (Sys @a h grps s') : accSys
                  accRun' = waitT t : accRun
                  (doneSet', progressedDone') =
                    if waitT t
                      then (doneSet, progressedDone)
                      else
                        let doneNew = IntSet.insert sid doneSet
                            progressedNew = progressedDone || not (IntSet.member sid doneSet)
                        in (doneNew, progressedNew)
                  seenSet' = IntSet.insert sid seenSet
                  progressedSeen' = progressedSeen || not (IntSet.member sid seenSet)
                  valuesSet' =
                    if waitT t
                      then valuesSet
                      else case valueT t of
                        Nothing -> valuesSet
                        Just v -> IntMap.insert sid v valuesSet
              in go w' inb' doneSet' seenSet' valuesSet' accOut' accSys' accRun' progressedDone' progressedSeen' rest
            else
              go w inb doneSet seenSet valuesSet accOut (AnySystem (Sys @a h grps step0) : accSys) (False : accRun) progressedDone progressedSeen rest

systemId :: AnySystem c msg -> SystemId
systemId (AnySystem s) = handleId (sysHandle s)

systemGroups :: AnySystem c msg -> Set.Set GroupId
systemGroups (AnySystem s) = sysGroups s
