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

module Engine.Data.Program
  ( Patch
  , EntityPatch
  , patch
  , emptyPatch
  , ProgramId
  , Handle
  , handle
  , handleOf
  , Program
  , AnyProgram
  , ProgramM
  , EntityM
  , MonadProgram
  , Batch
  , batch
  , each
  , eachP
  , eachM
  , eachMP
  , collect
  , GraphM
  , graphM
  , graphM_
  , programM
  , addProgram
  , newHandleM
  , Events
  , Inbox
  , events
  , done
  , seen
  , allIds
  , selfId
  , edit
  , world
  , send
  , dt
  , time
  , sample
  , step
  , drive
  , Await(..)
  , await
  , awaitProgram
  , awaitEvent
  , awaitInput
  , emit
  , emitMany
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
  , Graph(..)
  , program
  , graphAny
  , graphList
  , graph
  , run
  ) where

import Prelude

import Control.Monad (ap)
import Data.Bits (bit, complement, (.&.), (.|.))
import Data.Kind (Type)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Typeable (Typeable)
import Engine.Data.ECS (Entity, World, Sig)
import Engine.Data.FRP (DTime, Events, Step(..))
import qualified Engine.Data.FRP as F
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Input as I

newtype Patch c = Patch (World c -> World c)

type Machines = IntMap.IntMap Any

patch :: (World c -> World c) -> Patch c
patch = Patch

newtype EntityPatch c = EntityPatch
  { runEntityPatch :: Sig -> E.Bag c -> (Sig, E.Bag c)
  }

emptyPatch :: Patch c
emptyPatch = mempty

instance Semigroup (Patch c) where
  Patch f <> Patch g = Patch (g . f)

instance Monoid (Patch c) where
  mempty = Patch id

instance Semigroup (EntityPatch c) where
  EntityPatch f <> EntityPatch g =
    EntityPatch $ \sig bag ->
      let (sig', bag') = f sig bag
      in g sig' bag'

instance Monoid (EntityPatch c) where
  mempty = EntityPatch (\sig bag -> (sig, bag))

apply :: Patch c -> World c -> World c
apply (Patch f) = f

componentBitOfType :: forall c a. (E.Component c a, E.ComponentBit c a) => Int
componentBitOfType = E.componentBitOf @c @a

set :: forall a c. (E.Component c a, E.ComponentBit c a) => a -> EntityPatch c
set a =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch $ \sig bag ->
      let bag' = E.bagSet a bag
      in (sig .|. bitC, bag')

drive :: forall a c. (E.Component c a, E.ComponentBit c a) => F.Step () a -> EntityPatch c
drive s0 =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch $ \sig bag ->
      let bag' = E.bagSetStep @c @a s0 bag
      in (sig .|. bitC, bag')

update :: forall a c. (E.Component c a, E.ComponentBit c a) => (a -> a) -> EntityPatch c
update f =
  EntityPatch $ \sig bag ->
    case E.bagGet @c @a bag of
      Nothing -> (sig, bag)
      Just v ->
        let bag' = E.bagSet (f v) bag
        in (sig, bag')

del :: forall a c. (E.Component c a, E.ComponentBit c a) => EntityPatch c
del =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch $ \sig bag ->
      let bag' = E.bagDel @a bag
      in (sig .&. complement bitC, bag')

setAt :: forall a c. (E.Component c a, E.ComponentBit c a) => Entity -> a -> Patch c
setAt e a = patch (E.set e a)

updateAt :: forall a c. (E.Component c a, E.ComponentBit c a) => Entity -> (a -> a) -> Patch c
updateAt e f = patch $ \w ->
  case E.get @a e w of
    Nothing -> w
    Just v -> E.set e (f v) w

delAt :: forall a c. (E.Component c a, E.ComponentBit c a) => Entity -> Patch c
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

type ProgramId = Int
newtype Handle (a :: Type) = Handle
  { handleId :: ProgramId
  } deriving (Eq, Ord, Show)

type Done = IntSet
type Seen = IntSet

handle :: ProgramId -> Handle a
handle = Handle

data Locals c msg = Locals
  { localsGlobal :: IntMap.IntMap Any
  }

emptyLocals :: Locals c msg
emptyLocals = Locals IntMap.empty

data Program c (msg :: Type) (a :: Type) = ProgramDef
  { programHandle :: Handle a
  , programLocals :: Locals c msg
  , programProg :: ProgramM c msg a
  , programBase :: ProgramM c msg a
  }

data AnyProgram c msg = forall a. AnyProgram (Program c msg a)

handleOf :: Program c msg a -> Handle a
handleOf = programHandle

data Inbox a = Inbox
  { eventsI :: Events a
  , doneI :: Done
  , seenI :: Seen
  , allI :: IntSet
  , valuesI :: IntMap.IntMap Any
  , selfI :: ProgramId
  }

events :: Inbox a -> Events a
events = eventsI

done :: Inbox a -> Done
done = doneI

seen :: Inbox a -> Seen
seen = seenI

allIds :: Inbox a -> IntSet
allIds = allI

selfId :: Inbox a -> ProgramId
selfId = selfI

data Await c msg a where
  ProgramAwait :: Handle a -> Await c msg a
  Event :: (msg -> Bool) -> Await c msg (Events msg)
  Update :: Await c msg ()
  BatchWait :: Batch c msg a -> Await c msg a

awaitEvent :: forall c msg m. MonadProgram c msg m => (msg -> Bool) -> m (Events msg)
awaitEvent p = await (Event @msg @c p)

awaitProgram :: forall c msg m a. MonadProgram c msg m => Handle a -> m a
awaitProgram h = await (ProgramAwait h)

awaitInput :: MonadProgram c I.Input m => I.InputPred -> m (Events I.Input)
awaitInput p = await (Event (I.matchInput p))

awaitGate :: Await c msg a -> Inbox msg -> Maybe a
awaitGate waitOn inbox =
  case waitOn of
    Event p ->
      let hits = Prelude.filter p (eventsI inbox)
      in if null hits then Nothing else Just hits
    ProgramAwait h ->
      let sid = handleId h
          ready = IntSet.member sid (doneI inbox)
          value = IntMap.lookup sid (valuesI inbox)
      in if not ready
            then Nothing
            else case value of
              Just v -> Just (unsafeCoerce v)
              Nothing -> error "await: value missing for program handle (type mismatch?)"
    Update ->
      let others = IntSet.delete (selfI inbox) (allI inbox)
          synced = others `IntSet.isSubsetOf` seenI inbox
      in if synced then Just () else Nothing
    BatchWait _ -> Nothing

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

newtype Graph c a = Graph [AnyProgram c a]

class GraphArgs c msg r | r -> c msg where
  graphWith :: [AnyProgram c msg] -> r

instance GraphArgs c msg (Graph c msg) where
  graphWith acc = Graph (reverse acc)

instance GraphArgs c msg r => GraphArgs c msg (Program c msg a -> r) where
  graphWith acc s = graphWith (AnyProgram s : acc)

program :: Handle a -> ProgramM c msg a -> Program c msg a
program h prog = ProgramDef h emptyLocals prog prog

graph :: forall msg c r. GraphArgs c msg r => r
graph = graphWith @c @msg []

graphList :: [Program c msg a] -> Graph c msg
graphList = Graph . map AnyProgram

graphAny :: [AnyProgram c msg] -> Graph c msg
graphAny = Graph

data GraphState c msg = GraphState
  { gsNext :: !Int
  , gsPrograms :: ![AnyProgram c msg]
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

addProgram :: Program c msg a -> GraphM c msg (Handle a)
addProgram sys = GraphM $ \s ->
  let s' = s { gsPrograms = AnyProgram sys : gsPrograms s }
  in (s', handleOf sys)

programM :: ProgramM c msg a -> GraphM c msg (Handle a)
programM m = do
  h <- newHandleM
  _ <- addProgram (program h m)
  pure h

graphM :: GraphM c msg a -> (a, Graph c msg)
graphM m =
  let (s, a) = runGraphM m (GraphState 0 [])
  in (a, Graph (reverse (gsPrograms s)))

graphM_ :: GraphM c msg () -> Graph c msg
graphM_ = snd . graphM

data ProgramKey (key :: Type)

data BatchOut c msg = BatchOut
  { boLocals :: Locals c msg -> Locals c msg
  , boOut :: Out msg
  }

instance Semigroup (BatchOut c msg) where
  BatchOut f outF <> BatchOut g outG = BatchOut (g . f) (outF <> outG)

instance Monoid (BatchOut c msg) where
  mempty = BatchOut id mempty

newtype Batch c msg a = Batch
  { unBatch :: DTime -> Inbox msg -> Locals c msg -> Gather c msg (a, BatchOut c msg)
  }

instance Functor (Batch c msg) where
  fmap f (Batch g) =
    Batch $ \d inbox locals ->
      fmap (\(a, bout) -> (f a, bout)) (g d inbox locals)

instance Applicative (Batch c msg) where
  pure a = Batch $ \_ _ _ -> pure (a, mempty)
  Batch gf <*> Batch ga =
    Batch $ \d inbox locals ->
      let gF = gf d inbox locals
          gA = ga d inbox locals
      in (\(f, outF) (a, outA) -> (f a, outF <> outA)) <$> gF <*> gA

data Gather c msg a where
  Gather ::
    s ->
    (Entity -> E.Sig -> E.Bag c -> s -> (E.Sig, E.Bag c, s)) ->
    (s -> a) ->
    Gather c msg a

instance Functor (Gather c msg) where
  fmap f (Gather s step done) = Gather s step (f . done)

instance Applicative (Gather c msg) where
  pure a = Gather () (\_ sig bag s -> (sig, bag, s)) (const a)
  Gather sF stepF doneF <*> Gather sA stepA doneA =
    Gather (sF, sA) step done
    where
      step e sig bag (stF, stA) =
        let (sig1, bag1, stF') = stepF e sig bag stF
            (sig2, bag2, stA') = stepA e sig1 bag1 stA
        in (sig2, bag2, (stF', stA'))
      done (stF', stA') = doneF stF' (doneA stA')

each :: E.Query c a -> (a -> EntityPatch c) -> Batch c msg ()
each q f =
  Batch $ \_ _ _ ->
    let step e sig bag () =
          case E.runQuerySig q sig e bag of
            Nothing -> (sig, bag, ())
            Just a ->
              let (sig', bag') = runEntityPatch (f a) sig bag
              in (sig', bag', ())
        done () = ((), mempty)
    in Gather () step done

eachP :: E.Plan c a -> (a -> EntityPatch c) -> Batch c msg ()
eachP p f =
  Batch $ \_ _ _ ->
    let req = E.planReq p
        forb = E.planForbid p
        runP = E.planRun p
        step _ sig bag () =
          if (sig .&. req) == req && (sig .&. forb) == 0
            then
              let a = runP bag
                  (sig', bag') = runEntityPatch (f a) sig bag
              in (sig', bag', ())
            else (sig, bag, ())
        done () = ((), mempty)
    in Gather () step done

data EachAcc msg = EachAcc !(Out msg) !(IntMap.IntMap Any)

eachM :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Query c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
eachM q f =
  Batch $ \d inbox locals ->
    let progKey = E.typeIdOf @(ProgramKey key)
        progMap0 :: IntMap.IntMap Any
        progMap0 =
          case IntMap.lookup progKey (localsGlobal locals) of
            Just v -> unsafeCoerce v
            Nothing -> IntMap.empty
        hasProg = not (IntMap.null progMap0)
        step e sig bag (EachAcc outAcc progAcc) =
          case E.runQuerySig q sig e bag of
            Nothing -> (sig, bag, EachAcc outAcc progAcc)
            Just a ->
              let eid' = E.eid e
                  (prog0, machines0) =
                    if hasProg
                      then case IntMap.lookup eid' progMap0 of
                        Just v ->
                          let ProgState mProg machinesStored = unsafeCoerce v
                              prog0' = case mProg of
                                Just p0 -> p0
                                Nothing -> f a
                          in (prog0', machinesStored)
                        Nothing -> (f a, IntMap.empty)
                      else (f a, IntMap.empty)
                  (bag', sig', machines', out', waitE, prog', _) =
                    runEntityM d inbox sig bag machines0 prog0
                  progKeep = waitE || not (IntMap.null machines')
                  progState = ProgState (if waitE then Just prog' else Nothing) machines'
                  progAcc' =
                    if progKeep
                      then IntMap.insert eid' (unsafeCoerce progState) progAcc
                      else progAcc
                  outAcc' = outAcc <> out'
              in (sig', bag', EachAcc outAcc' progAcc')
        done (EachAcc outAcc progAcc) =
          let update locals0 =
                let globals0 = localsGlobal locals0
                    globals' =
                      if IntMap.null progAcc
                        then IntMap.delete progKey globals0
                        else IntMap.insert progKey (unsafeCoerce progAcc) globals0
                in locals0 { localsGlobal = globals' }
          in ((), BatchOut update outAcc)
    in Gather (EachAcc mempty IntMap.empty) step done

eachMP :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Plan c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
eachMP p f =
  Batch $ \d inbox locals ->
    let progKey = E.typeIdOf @(ProgramKey key)
        progMap0 :: IntMap.IntMap Any
        progMap0 =
          case IntMap.lookup progKey (localsGlobal locals) of
            Just v -> unsafeCoerce v
            Nothing -> IntMap.empty
        hasProg = not (IntMap.null progMap0)
        req = E.planReq p
        forb = E.planForbid p
        runP = E.planRun p
        step e sig bag (EachAcc outAcc progAcc) =
          if (sig .&. req) == req && (sig .&. forb) == 0
            then
              let a = runP bag
                  eid' = E.eid e
                  (prog0, machines0) =
                    if hasProg
                      then case IntMap.lookup eid' progMap0 of
                        Just v ->
                          let ProgState mProg machinesStored = unsafeCoerce v
                              prog0' = case mProg of
                                Just p0 -> p0
                                Nothing -> f a
                          in (prog0', machinesStored)
                        Nothing -> (f a, IntMap.empty)
                      else (f a, IntMap.empty)
                  (bag', sig', machines', out', waitE, prog', _) =
                    runEntityM d inbox sig bag machines0 prog0
                  progKeep = waitE || not (IntMap.null machines')
                  progState = ProgState (if waitE then Just prog' else Nothing) machines'
                  progAcc' =
                    if progKeep
                      then IntMap.insert eid' (unsafeCoerce progState) progAcc
                      else progAcc
                  outAcc' = outAcc <> out'
              in (sig', bag', EachAcc outAcc' progAcc')
            else (sig, bag, EachAcc outAcc progAcc)
        done (EachAcc outAcc progAcc) =
          let update locals0 =
                let globals0 = localsGlobal locals0
                    globals' =
                      if IntMap.null progAcc
                        then IntMap.delete progKey globals0
                        else IntMap.insert progKey (unsafeCoerce progAcc) globals0
                in locals0 { localsGlobal = globals' }
          in ((), BatchOut update outAcc)
    in Gather (EachAcc mempty IntMap.empty) step done

collect :: E.Query c a -> Batch c msg [(Entity, a)]
collect q =
  Batch $ \_ _ _ ->
    fmap (\a -> (a, mempty)) $
      Gather [] step reverse
  where
    step e sig bag acc =
      case E.runQuerySig q sig e bag of
        Nothing -> (sig, bag, acc)
        Just a -> (sig, bag, (e, a) : acc)


data ProgCtx c msg = ProgCtx
  { sysWorld :: !(World c)
  , sysLocals :: !(Locals c msg)
  , sysOut :: !(Out msg)
  , sysPatch :: !(Patch c)
  , sysDt :: !DTime
  , sysInbox :: !(Inbox msg)
  , sysStepped :: !Bool
  }

data ProgRes c msg a
  = ProgDone a
  | ProgWait (ProgramM c msg a)

newtype ProgramM c msg a = ProgramM
  { unProgramM :: ProgCtx c msg -> (ProgCtx c msg, ProgRes c msg a)
  }

instance Functor (ProgramM c msg) where
  fmap f (ProgramM g) = ProgramM $ \ctx ->
    case g ctx of
      (ctx', ProgDone a) -> (ctx', ProgDone (f a))
      (ctx', ProgWait p) -> (ctx', ProgWait (fmap f p))

instance Applicative (ProgramM c msg) where
  pure a = ProgramM $ \ctx -> (ctx, ProgDone a)
  (<*>) = ap

instance Monad (ProgramM c msg) where
  ProgramM g >>= k = ProgramM $ \ctx ->
    case g ctx of
      (ctx', ProgDone a) -> unProgramM (k a) ctx'
      (ctx', ProgWait p) -> (ctx', ProgWait (p >>= k))

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
  , ctxSig :: !E.Sig
  , ctxMachines :: !Machines
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

class Monad m => MonadProgram c msg m | m -> c msg where
  send :: Events msg -> m ()
  dt :: m DTime
  await :: Await c msg a -> m a
  stepId :: (Typeable a, Typeable b) => E.TypeId -> F.Step a b -> a -> m b

instance MonadProgram c msg (ProgramM c msg) where
  send out = ProgramM $ \ctx ->
    let out' = sysOut ctx <> outFrom out
    in (ctx { sysOut = out' }, ProgDone ())
  dt = ProgramM $ \ctx -> (ctx, ProgDone (sysDt ctx))
  await waitOn = ProgramM $ \ctx ->
    case waitOn of
      BatchWait b ->
        let w0 = sysWorld ctx
            stepped0 = sysStepped ctx
            d = sysDt ctx
            wStep = if stepped0 then w0 else E.stepWorld d w0
            stepped' = True
            g = unBatch b d (sysInbox ctx) (sysLocals ctx)
            ((a, bout), p) = runGather g wStep
            w' = apply p wStep
            locals' = boLocals bout (sysLocals ctx)
            out' = sysOut ctx <> boOut bout
            patch' = sysPatch ctx <> p
            ctx' =
              ctx
                { sysWorld = w'
                , sysLocals = locals'
                , sysOut = out'
                , sysPatch = patch'
                , sysStepped = stepped'
                }
        in (ctx', ProgDone a)
      _ ->
        case awaitGate waitOn (sysInbox ctx) of
          Nothing -> (ctx, ProgWait (await waitOn))
          Just a -> (ctx, ProgDone a)
  stepId key s0 a = ProgramM $ \ctx ->
    let locals0 = sysLocals ctx
        globals0 = localsGlobal locals0
        s = case IntMap.lookup key globals0 of
          Just v -> unsafeCoerce v
          Nothing -> s0
        (b, s') = stepS s (sysDt ctx) a
        globals' = IntMap.insert key (unsafeCoerce s') globals0
        locals' = locals0 { localsGlobal = globals' }
    in (ctx { sysLocals = locals' }, ProgDone b)

instance MonadProgram c msg (EntityM c msg) where
  send out = EntityM $ \ctx ->
    let out' = ctxOut ctx <> outFrom out
    in (ctx { ctxOut = out' }, Done ())
  dt = EntityM $ \ctx -> (ctx, Done (ctxDt ctx))
  await waitOn = EntityM $ \ctx ->
    case waitOn of
      BatchWait _ -> error "await: batch is only valid in ProgramM"
      _ ->
        case awaitGate waitOn (ctxInbox ctx) of
          Nothing -> (ctx, Wait (await waitOn))
          Just a -> (ctx, Done a)
  stepId key s0 a = EntityM $ \ctx ->
    let machines0 = ctxMachines ctx
        s = case IntMap.lookup key machines0 of
          Just v -> unsafeCoerce v
          Nothing -> s0
        (b, s') = stepS s (ctxDt ctx) a
        machines' = IntMap.insert key (unsafeCoerce s') machines0
    in (ctx { ctxMachines = machines' }, Done b)

edit :: EntityPatch c -> EntityM c msg ()
edit p = EntityM $ \ctx ->
  let (sig', bag') = runEntityPatch p (ctxSig ctx) (ctxBag ctx)
  in (ctx { ctxSig = sig', ctxBag = bag' }, Done ())

world :: Patch c -> ProgramM c msg ()
world p = ProgramM $ \ctx ->
  let w' = apply p (sysWorld ctx)
      patch' = sysPatch ctx <> p
  in (ctx { sysWorld = w', sysPatch = patch' }, ProgDone ())

localStateId :: forall s c msg a. Typeable s => E.TypeId -> s -> (s -> (a, s)) -> ProgramM c msg a
localStateId key s0 f = ProgramM $ \ctx ->
  let locals0 = sysLocals ctx
      globals0 = localsGlobal locals0
      s = case IntMap.lookup key globals0 of
        Just v -> unsafeCoerce v
        Nothing -> s0
      (a, s') = f s
      globals' = IntMap.insert key (unsafeCoerce s') globals0
      locals' = locals0 { localsGlobal = globals' }
  in (ctx { sysLocals = locals' }, ProgDone a)


data TimeKey

time :: MonadProgram c msg m => m F.Time
time = step @TimeKey F.time ()

sample :: MonadProgram c msg m => F.Tween a -> m a
sample tw = do
  t <- time
  pure (F.at tw t)

step :: forall key a b c msg m. (MonadProgram c msg m, Typeable key, Typeable a, Typeable b) => F.Step a b -> a -> m b
step s0 a = stepId (E.typeIdOf @key) s0 a

batch :: Batch c msg a -> Await c msg a
batch = BatchWait

runEntityM :: forall c msg a. DTime -> Inbox msg -> E.Sig -> E.Bag c -> Machines -> EntityM c msg a
  -> (E.Bag c, E.Sig, Machines, Out msg, Bool, EntityM c msg a, Maybe a)
runEntityM d inbox sig0 bag0 machines0 prog0 =
  let ctx0 =
        EntityCtx
          { ctxBag = bag0
          , ctxSig = sig0
          , ctxMachines = machines0
          , ctxOut = mempty
          , ctxDt = d
          , ctxInbox = inbox
          }
      (ctx1, res) = unEntityM prog0 ctx0
      sig' = ctxSig ctx1
      bag' = ctxBag ctx1
      machines' = ctxMachines ctx1
      out' = ctxOut ctx1
  in case res of
    Done a -> (bag', sig', machines', out', False, prog0, Just a)
    Wait prog' -> (bag', sig', machines', out', True, prog', Nothing)

data ProgState c msg = ProgState
  !(Maybe (EntityM c msg ()))
  !Machines

runGather :: Gather c msg a -> World c -> (a, Patch c)
runGather (Gather s0 step done) w =
  let stepRow e sig bag (build, st) =
        let (sig', bag', st') = step e sig bag st
        in (build . ((E.eid e, sig', bag') :), st')
      (build, st') = E.foldEntities stepRow (id, s0) w
      rows = build []
  in (done st', patch (E.setEntityRows rows))

runProgramM :: DTime -> World c -> Bool -> Inbox msg -> Locals c msg -> ProgramM c msg a
  -> (Tick c msg, Locals c msg, ProgramM c msg a, Maybe a, Bool)
runProgramM d w0 stepped0 inbox locals0 prog0 =
  let ctx0 =
        ProgCtx
          { sysWorld = w0
          , sysLocals = locals0
          , sysOut = mempty
          , sysPatch = mempty
          , sysDt = d
          , sysInbox = inbox
          , sysStepped = stepped0
          }
      (ctx1, res) = unProgramM prog0 ctx0
      waitFlag = case res of
        ProgWait _ -> True
        ProgDone _ -> False
      prog' = case res of
        ProgWait p -> p
        ProgDone _ -> prog0
      value = case res of
        ProgDone a -> Just a
        ProgWait _ -> Nothing
      t = Tick (sysPatch ctx1) (outToList (sysOut ctx1)) waitFlag (unsafeCoerce <$> value)
  in (t, sysLocals ctx1, prog', value, sysStepped ctx1)

run :: DTime -> World c -> Events a -> Graph c a -> (World c, Events a, Graph c a)
run d w0 inbox g0 =
  go w0 programs0 (repeat True) IntSet.empty IntSet.empty IntMap.empty [] False
  where
    Graph programs0 = g0
    allSet = IntSet.fromList (map programId programs0)

    go w programs toRun doneSet seenSet values accOut stepped =
      let available = inbox ++ accOut
          (w', roundOut, programs', nextRun, done1, seen1, values1, progressed, stepped') =
            stepRound d w available programs toRun doneSet seenSet values allSet stepped
          accOut' = accOut ++ roundOut
      in if not (or nextRun) || not progressed
          then (w', accOut', Graph programs')
          else go w' programs' nextRun done1 seen1 values1 accOut' stepped'

stepRound :: DTime
          -> World c
          -> Events a
          -> [AnyProgram c a]
          -> [Bool]
          -> Done
          -> Seen
          -> IntMap.IntMap Any
          -> IntSet
          -> Bool
          -> (World c, Events a, [AnyProgram c a], [Bool], Done, Seen, IntMap.IntMap Any, Bool, Bool)
stepRound d w0 events0 programs toRun done0 seen0 values0 allSet stepped0 =
  go events0 done0 seen0 values0 [] [] [] w0 False stepped0 (zip programs toRun)
  where
    go _ doneSet seenSet valuesSet accOut accProg accRun w progressed stepped [] =
      (w, accOut, reverse accProg, reverse accRun, doneSet, seenSet, valuesSet, progressed, stepped)
    go inb doneSet seenSet valuesSet accOut accProg accRun w progressed stepped
      ((AnyProgram (prog0 :: Program c msg a), runNow) : rest) =
      if runNow
        then
          let sid = handleId (programHandle prog0)
              inbox0 = Inbox inb doneSet seenSet allSet valuesSet sid
              (t, locals', prog', _, stepped') = runProgramM d w stepped inbox0 (programLocals prog0) (programProg prog0)
              out = outT t
              inb' = inb ++ out
              accOut' = accOut ++ out
              w' = apply (patchT t) w
              waitFlag = waitT t
              nextProg = if waitFlag then prog' else programBase prog0
              prog1 = prog0 { programLocals = locals', programProg = nextProg }
              accProg' = AnyProgram prog1 : accProg
              accRun' = waitFlag : accRun
              doneSet' = if waitFlag then doneSet else IntSet.insert sid doneSet
              progressedDone = not waitFlag && not (IntSet.member sid doneSet)
              seenSet' = IntSet.insert sid seenSet
              progressedSeen = not (IntSet.member sid seenSet)
              valuesSet' =
                if waitFlag
                  then valuesSet
                  else case valueT t of
                    Nothing -> valuesSet
                    Just v -> IntMap.insert sid v valuesSet
              progressed' = progressed || progressedDone || progressedSeen || not (null out)
          in go inb' doneSet' seenSet' valuesSet' accOut' accProg' accRun' w' progressed' stepped' rest
        else
          let accProg' = AnyProgram prog0 : accProg
          in go inb doneSet seenSet valuesSet accOut accProg' (False : accRun) w progressed stepped rest

programId :: AnyProgram c msg -> ProgramId
programId (AnyProgram s) = handleId (programHandle s)
