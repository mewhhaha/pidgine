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
  , compute
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
  , Await
  , Awaitable(..)
  , await
  , emit
  , emitMany
  , Tick(..)
  , tick
  , wait
  , set
  , update
  , del
  , at
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
import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Bits (bit, complement, (.&.), (.|.))
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
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

at :: Entity -> EntityPatch c -> Patch c
at e p = patch (E.mapEntities step)
  where
    step e' sig bag
      | e' == e =
          let (_, bag') = runEntityPatch p sig bag
          in bag'
      | otherwise = bag

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

class Awaitable c msg a b | a -> b where
  toAwait :: a -> Await c msg b

instance Awaitable c msg (Await c msg a) a where
  toAwait = id

instance Awaitable c msg (Handle a) a where
  toAwait = ProgramAwait

instance Awaitable c msg (Batch c msg a) a where
  toAwait = BatchWait

instance Awaitable c msg (msg -> Bool) (Events msg) where
  toAwait = Event

instance Awaitable c I.Input I.InputPred (Events I.Input) where
  toAwait (I.InputPred p) = Event p

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

data PendingBatch c msg where
  PendingBatch ::
    ProgramId ->
    Locals c msg ->
    Inbox msg ->
    Batch c msg a ->
    (a -> ProgramM c msg b) ->
    PendingBatch c msg

data BatchGroup c msg = BatchGroup
  { bgPid :: !ProgramId
  , bgLocals :: !(Locals c msg)
  , bgK :: !(V.Vector Any -> Any)
  , bgCont :: !(Any -> Any)
  , bgOpCount :: !Int
  }

data PendingStep c msg where
  PendingStep ::
    Int ->
    s ->
    (Entity -> E.Sig -> E.Bag c -> s -> (E.Sig, E.Bag c, s)) ->
    (s -> (Any, BatchOut c msg)) ->
    (s -> s -> s) ->
    ([E.EntityRow c] -> s -> (s, s)) ->
    PendingStep c msg

data StepStatic c msg = StepStatic
  { ssGroup :: !Int
  , ssStep :: !(Entity -> Sig -> E.Bag c -> Any -> (Sig, E.Bag c, Any))
  , ssDone :: !(Any -> (Any, BatchOut c msg))
  , ssMerge :: !(Any -> Any -> Any)
  , ssSplit :: !([E.EntityRow c] -> Any -> (Any, Any))
  }

data PendingState c msg = PendingState
  { psStatics :: !(V.Vector (StepStatic c msg))
  , psStates :: !(V.Vector Any)
  }

data ProgramUpdate c msg = ProgramUpdate
  { puLocals :: Locals c msg
  , puProg :: Any
  }

data Acc c msg = Acc
  { accRes :: !(Build Any)
  , accOut :: !(BatchOut c msg)
  , accCount :: !Int
  }

data RunRes c msg where
  Ran :: Program c msg a -> Inbox msg -> Tick c msg -> Locals c msg -> ProgStep c msg a -> RunRes c msg
  Skipped :: Program c msg a -> RunRes c msg

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

data BatchOp c msg = BatchOp
  { opGather :: Gather c msg (Any, BatchOut c msg)
  }

data BatchRun c msg a = BatchRun
  { brOps :: !(Build (BatchOp c msg))
  , brCount :: !Int
  , brK :: !(V.Vector Any -> Int -> (a, Int))
  }

data Batch c msg a = Batch
  { unBatch :: DTime -> Inbox msg -> Locals c msg -> BatchRun c msg a
  , batchReq :: E.Sig
  , batchPar :: Bool
  }

instance Functor (Batch c msg) where
  fmap f (Batch g req parOk) =
    Batch
      (\d inbox locals ->
        let BatchRun ops n k = g d inbox locals
        in BatchRun ops n (\xs i -> let (a, i') = k xs i in (f a, i'))
      )
      req
      parOk

instance Applicative (Batch c msg) where
  pure a = Batch (\_ _ _ -> BatchRun mempty 0 (\_ i -> (a, i))) 0 True
  Batch gf reqF parF <*> Batch ga reqA parA =
    Batch
      (\d inbox locals ->
        let BatchRun opsF nF kF = gf d inbox locals
            BatchRun opsA nA kA = ga d inbox locals
            k xs i0 =
              let (f, i1) = kF xs i0
                  (a, i2) = kA xs i1
              in (f a, i2)
        in BatchRun (opsF <> opsA) (nF + nA) k
      )
      (reqF .|. reqA)
      (parF && parA)

data Gather c msg a where
  Gather ::
    s ->
    (Entity -> E.Sig -> E.Bag c -> s -> (E.Sig, E.Bag c, s)) ->
    (s -> a) ->
    (s -> s -> s) ->
    ([E.EntityRow c] -> s -> (s, s)) ->
    Gather c msg a

instance Functor (Gather c msg) where
  fmap f (Gather s step done merge split) = Gather s step (f . done) merge split

instance Applicative (Gather c msg) where
  pure a =
    Gather () (\_ sig bag s -> (sig, bag, s)) (const a) (\_ _ -> ()) (\_ s -> (s, s))
  Gather sF stepF doneF mergeF splitF <*> Gather sA stepA doneA mergeA splitA =
    Gather (sF, sA) step done merge split
    where
      step e sig bag (stF, stA) =
        let (sig1, bag1, stF') = stepF e sig bag stF
            (sig2, bag2, stA') = stepA e sig1 bag1 stA
        in (sig2, bag2, (stF', stA'))
      done (stF', stA') = doneF stF' (doneA stA')
      merge (stF1, stA1) (stF2, stA2) =
        (mergeF stF1 stF2, mergeA stA1 stA2)
      split rows (stF, stA) =
        let (stF1, stF2) = splitF rows stF
            (stA1, stA2) = splitA rows stA
        in ((stF1, stA1), (stF2, stA2))

planQuery :: E.Plan c a -> E.Query c a
planQuery (E.Plan req forb runP) =
  E.Query (\_ bag -> Just (runP bag)) (E.QueryInfo req forb)

batchRun1 :: Gather c msg (a, BatchOut c msg) -> BatchRun c msg a
batchRun1 g =
  let g' = fmap (\(a, bout) -> (unsafeCoerce a, bout)) g
      k xs i =
        if i < V.length xs
          then (unsafeCoerce (V.unsafeIndex xs i), i + 1)
          else error "compute: missing batch result"
  in BatchRun (buildOne (BatchOp g')) 1 k

each :: E.Query c a -> (a -> EntityPatch c) -> Batch c msg ()
each q f =
  let req = E.requireQ (E.queryInfo q)
  in Batch (\_ _ _ -> batchRun1 (Gather () step done (\_ _ -> ()) (\_ s -> (s, s)))) req True
  where
    step e sig bag () =
      case E.runQuerySig q sig e bag of
        Nothing -> (sig, bag, ())
        Just a ->
          let (sig', bag') = runEntityPatch (f a) sig bag
          in (sig', bag', ())
    done () = ((), mempty)

eachP :: E.Plan c a -> (a -> EntityPatch c) -> Batch c msg ()
eachP p f = each (planQuery p) f

data EachAcc c msg = EachAcc
  { eaOut :: !(Out msg)
  , eaBuild :: !(Build (Int, ProgState c msg))
  , eaProg :: ![(Int, ProgState c msg)]
  }

emptyEachAcc :: [(Int, ProgState c msg)] -> EachAcc c msg
emptyEachAcc prog = EachAcc mempty mempty prog

mergeEachAcc :: EachAcc c msg -> EachAcc c msg -> EachAcc c msg
mergeEachAcc a b =
  EachAcc
    { eaOut = eaOut a <> eaOut b
    , eaBuild = eaBuild a <> eaBuild b
    , eaProg = []
    }

eachM :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Query c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
eachM q f =
  let req = E.requireQ (E.queryInfo q)
  in Batch (\d inbox locals ->
      let progList0 :: [(Int, ProgState c msg)]
          progList0 =
            case IntMap.lookup progKey (localsGlobal locals) of
              Just v -> unsafeCoerce v
              Nothing -> []
          step = stepEntity d inbox
      in batchRun1 (Gather (emptyEachAcc progList0) step done merge split)
    ) req True
  where
    progKey = E.typeIdOf @(ProgramKey key)
    stepEntity d inbox e sig bag acc =
      case E.runQuerySig q sig e bag of
        Nothing ->
          let acc' = acc { eaProg = dropRemoved (E.eid e) (eaProg acc) }
          in (sig, bag, acc')
        Just a ->
          let eid' = E.eid e
              (mState, progRest) = popProg eid' (eaProg acc)
              (prog0, machines0) =
                case mState of
                  Just (ProgState mProg machinesStored) ->
                    let prog0' = maybe (f a) id mProg
                    in (prog0', machinesStored)
                  Nothing -> (f a, IntMap.empty)
              (bag', sig', machines', out', waitE, prog', _) =
                runEntityM d inbox sig bag machines0 prog0
              progKeep = waitE || not (IntMap.null machines')
              progState = ProgState (if waitE then Just prog' else Nothing) machines'
              build' =
                if progKeep
                  then eaBuild acc <> buildOne (eid', progState)
                  else eaBuild acc
              outAcc' = eaOut acc <> out'
              acc' = acc { eaOut = outAcc', eaBuild = build', eaProg = progRest }
          in (sig', bag', acc')
    done acc =
      let update locals0 =
            let globals0 = localsGlobal locals0
                progList = buildToList (eaBuild acc)
                globals' =
                  if null progList
                    then IntMap.delete progKey globals0
                    else IntMap.insert progKey (unsafeCoerce progList) globals0
            in locals0 { localsGlobal = globals' }
      in ((), BatchOut update (eaOut acc))
    merge = mergeEachAcc
    split rows acc =
      let (chunkProg, restProg) = splitProg rows (eaProg acc)
      in (emptyEachAcc chunkProg, emptyEachAcc restProg)
    popProg eid' [] = (Nothing, [])
    popProg eid' all@(p@(pid, _) : ps)
      | pid == eid' = (Just (snd p), ps)
      | pid > eid' = popProg eid' ps
      | otherwise = (Nothing, all)
    dropRemoved eid' = snd . popProg eid'
    splitProg [] prog = ([], prog)
    splitProg (_ : rows) [] = ([], [])
    splitProg rows@((eidRow, _, _) : restRows) prog@((pid, st) : restProg)
      | pid == eidRow =
          let (chunk, rest) = splitProg restRows restProg
          in ((pid, st) : chunk, rest)
      | pid > eidRow = splitProg rows restProg
      | otherwise = splitProg restRows prog

eachMP :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Plan c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
eachMP p f = eachM @key (planQuery p) f

collect :: E.Query c a -> Batch c msg [(Entity, a)]
collect q =
  let req = E.requireQ (E.queryInfo q)
  in Batch (\_ _ _ -> batchRun1 (Gather mempty step done (<>) (\_ s -> (s, s)))) req True
  where
    step e sig bag acc =
      case E.runQuerySig q sig e bag of
        Nothing -> (sig, bag, acc)
        Just a -> (sig, bag, acc <> buildOne (e, a))
    done acc = (buildToList acc, mempty)


data ProgCtx c msg = ProgCtx
  { sysWorld :: !(World c)
  , sysLocals :: !(Locals c msg)
  , sysOut :: !(Out msg)
  , sysPatch :: !(Patch c)
  , sysDt :: !DTime
  , sysInbox :: !(Inbox msg)
  }

data ProgStep c msg a where
  ProgDone :: a -> ProgStep c msg a
  ProgAwait :: Await c msg b -> (b -> ProgramM c msg a) -> ProgStep c msg a

newtype ProgramM c msg a = ProgramM
  { unProgramM :: ProgCtx c msg -> (ProgCtx c msg, ProgStep c msg a)
  }

instance Functor (ProgramM c msg) where
  fmap f (ProgramM g) = ProgramM $ \ctx ->
    case g ctx of
      (ctx', ProgDone a) -> (ctx', ProgDone (f a))
      (ctx', ProgAwait w k) -> (ctx', ProgAwait w (fmap f . k))

instance Applicative (ProgramM c msg) where
  pure a = ProgramM $ \ctx -> (ctx, ProgDone a)
  (<*>) = ap

instance Monad (ProgramM c msg) where
  ProgramM g >>= k = ProgramM $ \ctx ->
    case g ctx of
      (ctx', ProgDone a) -> unProgramM (k a) ctx'
      (ctx', ProgAwait w cont) -> (ctx', ProgAwait w (\x -> cont x >>= k))

newtype Out msg = Out ([msg] -> [msg])

instance Semigroup (Out msg) where
  Out f <> Out g = Out (f . g)

instance Monoid (Out msg) where
  mempty = Out id

outFrom :: Events msg -> Out msg
outFrom xs = Out (xs ++)

outToList :: Out msg -> Events msg
outToList (Out f) = f []

newtype Build a = Build ([a] -> [a])

instance Semigroup (Build a) where
  Build f <> Build g = Build (f . g)

instance Monoid (Build a) where
  mempty = Build id

buildOne :: a -> Build a
buildOne a = Build (a :)

buildToList :: Build a -> [a]
buildToList (Build f) = f []

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
  awaitM :: Await c msg a -> m a
  stepId :: (Typeable a, Typeable b) => E.TypeId -> F.Step a b -> a -> m b

await :: (MonadProgram c msg m, Awaitable c msg a b) => a -> m b
await = awaitM . toAwait

instance MonadProgram c msg (ProgramM c msg) where
  send out = ProgramM $ \ctx ->
    let out' = sysOut ctx <> outFrom out
    in (ctx { sysOut = out' }, ProgDone ())
  dt = ProgramM $ \ctx -> (ctx, ProgDone (sysDt ctx))
  awaitM waitOn = ProgramM $ \ctx ->
    case waitOn of
      BatchWait _ ->
        (ctx, ProgAwait waitOn pure)
      _ ->
        case awaitGate waitOn (sysInbox ctx) of
          Nothing -> (ctx, ProgAwait waitOn pure)
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
  awaitM waitOn = EntityM $ \ctx ->
    case waitOn of
      BatchWait _ -> error "await: compute is only valid in ProgramM"
      _ ->
        case awaitGate waitOn (ctxInbox ctx) of
          Nothing -> (ctx, Wait (awaitM waitOn))
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

compute :: Batch c msg a -> Batch c msg a
compute = id

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

runProgramM :: DTime -> World c -> Inbox msg -> Locals c msg -> ProgramM c msg a
  -> (Tick c msg, Locals c msg, ProgStep c msg a)
runProgramM d w0 inbox locals0 prog0 =
  let ctx0 =
        ProgCtx
          { sysWorld = w0
          , sysLocals = locals0
          , sysOut = mempty
          , sysPatch = mempty
          , sysDt = d
          , sysInbox = inbox
          }
      (ctx1, res) = unProgramM prog0 ctx0
      waitFlag = case res of
        ProgAwait _ _ -> True
        ProgDone _ -> False
      value = case res of
        ProgDone a -> Just (unsafeCoerce a)
        ProgAwait _ _ -> Nothing
      t = Tick (sysPatch ctx1) (outToList (sysOut ctx1)) waitFlag value
  in (t, sysLocals ctx1, res)

run :: DTime -> World c -> Events a -> Graph c a -> (World c, Events a, Graph c a)
run d w0 inbox g0 =
  go w0 programs0 (repeat True) IntSet.empty IntSet.empty IntMap.empty mempty False
  where
    Graph programs0 = g0
    allSet = IntSet.fromList (map programId programs0)
    inboxOut = outFrom inbox

    go w programs toRun doneSet seenSet values accOut stepped =
      let available = outToList (inboxOut <> accOut)
          (w', roundOut, programs', nextRun, done1, seen1, values1, progressed, stepped') =
            stepRound d w available programs toRun doneSet seenSet values allSet stepped
          accOut' = accOut <> roundOut
      in if not (or nextRun) || not progressed
          then (w', outToList accOut', Graph programs')
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
          -> (World c, Out a, [AnyProgram c a], [Bool], Done, Seen, IntMap.IntMap Any, Bool, Bool)
stepRound d w0 events0 programs toRun done0 seen0 values0 allSet stepped0 =
  let (w1, out1, programs1, run1, done1, seen1, values1, progressed1, pending) =
        runProgramsPhase events0 done0 seen0 values0 w0 (zip programs toRun)
  in case pending of
      [] -> (w1, out1, programs1, run1, done1, seen1, values1, progressed1, stepped0)
      _ ->
        let (w2, out2, programs2, run2, progressed2, stepped1) =
              runBatchesPhase d w1 pending programs1 run1 stepped0
        in (w2, out1 <> out2, programs2, run2, done1, seen1, values1, progressed1 || progressed2, stepped1)
  where
    runProgramsPhase inb doneSet seenSet valuesSet w pairs =
      let results =
            withStrategy (parListChunk 1 rseq) $
              map (runOne inb doneSet seenSet valuesSet w) pairs
          (w', out', progs', runs', dSet', sSet', vSet', progressed', pending') =
            foldl' applyOne (w, mempty, [], [], doneSet, seenSet, valuesSet, False, []) results
      in (w', out', reverse progs', reverse runs', dSet', sSet', vSet', progressed', reverse pending')
      where
        runOne inbox0 dSet sSet vSet w0 (AnyProgram (prog0 :: Program c msg a), runNow) =
          if runNow
            then
              let sid = handleId (programHandle prog0)
                  inbox1 = Inbox inbox0 dSet sSet allSet vSet sid
                  (t, locals', res) = runProgramM d w0 inbox1 (programLocals prog0) (programProg prog0)
              in Ran prog0 inbox1 t locals' res
            else
              Skipped prog0

        applyOne (wAcc, accOut, accProg, accRun, dSet, sSet, vSet, progressed, pending) res =
          case res of
            Skipped prog0 ->
              (wAcc, accOut, AnyProgram prog0 : accProg, False : accRun, dSet, sSet, vSet, progressed, pending)
            Ran (prog0 :: Program c msg a) inbox0 t locals' resStep ->
              let sid = handleId (programHandle prog0)
                  out = outT t
                  accOut' = accOut <> outFrom out
                  w' = apply (patchT t) wAcc
                  sSet' = IntSet.insert sid sSet
                  progressedSeen = not (IntSet.member sid sSet)
              in case resStep of
                  ProgDone _ ->
                    let prog1 = prog0 { programLocals = locals', programProg = programBase prog0 }
                        accProg' = AnyProgram prog1 : accProg
                        accRun' = False : accRun
                        dSet' = IntSet.insert sid dSet
                        progressedDone = not (IntSet.member sid dSet)
                        vSet' = case valueT t of
                          Nothing -> vSet
                          Just v -> IntMap.insert sid v vSet
                        progressed' = progressed || progressedDone || progressedSeen || not (null out)
                    in (w', accOut', accProg', accRun', dSet', sSet', vSet', progressed', pending)
                  ProgAwait waitOn cont ->
                    let progWait = await waitOn >>= cont
                        prog1 = prog0 { programLocals = locals', programProg = progWait }
                        accProg' = AnyProgram prog1 : accProg
                        (pending', runFlag) =
                          case waitOn of
                            BatchWait b -> (PendingBatch sid locals' inbox0 b cont : pending, False)
                            _ -> (pending, True)
                        accRun' = runFlag : accRun
                        progressed' = progressed || progressedSeen || not (null out)
                    in (w', accOut', accProg', accRun', dSet, sSet', vSet, progressed', pending')

    runBatchesPhase d w pending programs runFlags stepped0 =
      let pendingSorted = pending
          wStep = if stepped0 then w else E.stepWorld d w
          stepped1 = True
          compiled = zipWith (compilePending d) [0..] pendingSorted
          steps = concatMap fst compiled
          groups = V.fromList (map snd compiled)
          parOk = all pendingPar pendingSorted
          rows = E.entityRows wStep
          state0 = stepsFromPending steps
          (w', state') =
            if parOk
              then runPendingStepsPar rows state0 wStep
              else runPendingSteps rows state0 wStep
          (updates, out) = finalizePendingState state' groups
          programs' = applyProgramUpdates programs updates
          runFlags' = updateRunFlags programs' runFlags (IntMap.keysSet updates)
          progressed = not (IntMap.null updates) || not (null out)
      in (w', outFrom out, programs', runFlags', progressed, stepped1)

    pendingPar (PendingBatch _ _ _ b _) = batchPar b

    compilePending d gid (PendingBatch pid locals inbox b cont) =
      let BatchRun ops n k = unBatch b d inbox locals
          kAny xs = unsafeCoerce (fst (k xs 0))
          contAny v = unsafeCoerce (cont (unsafeCoerce v))
          group =
            BatchGroup
              { bgPid = pid
              , bgLocals = locals
              , bgK = kAny
              , bgCont = contAny
              , bgOpCount = n
              }
          steps = map (opToStep gid) (buildToList ops)
      in (steps, group)

    opToStep gid (BatchOp g) =
      case g of
        Gather s step done merge split -> PendingStep gid s step done merge split

    runPendingSteps rows state0 wStep =
      let (rows', state1) = processRows rows state0
          w' = E.setEntityRows rows' wStep
      in (w', state1)

    runPendingStepsPar rows state0 wStep =
      let chunkSize = 1024
          rowChunks = chunkRows chunkSize rows
          stateChunks = splitStateChunks rowChunks state0
          chunkResults =
            withStrategy (parListChunk 1 rseq) $
              zipWith (\st rs -> forceChunk (processRows rs st)) stateChunks rowChunks
          rows' = concatMap fst chunkResults
          state' = mergeStateChunks state0 (map snd chunkResults)
          w' = E.setEntityRows rows' wStep
      in (w', state')

    forceChunk (rows', st) = length rows' `seq` (rows', st)

    chunkRows n xs =
      let rowsV = V.fromList xs
          total = V.length rowsV
          go i acc =
            if i >= total
              then reverse acc
              else
                let chunkLen = min n (total - i)
                    chunk = V.toList (V.slice i chunkLen rowsV)
                in go (i + chunkLen) (chunk : acc)
      in if n <= 0 || total == 0
          then []
          else go 0 []

    stepsFromPending steps =
      let statics = V.fromList (map mkStatic steps)
          states = V.fromList (map mkState steps)
      in PendingState statics states
      where
        mkStatic (PendingStep gid st step done merge split) =
          StepStatic
            { ssGroup = gid
            , ssStep = \e sig bag stAny ->
                let (sig', bag', st') = step e sig bag (unsafeCoerce stAny)
                in (sig', bag', unsafeCoerce st')
            , ssDone = \stAny ->
                let (a, bout) = done (unsafeCoerce stAny)
                in (unsafeCoerce a, bout)
            , ssMerge = \a b -> unsafeCoerce (merge (unsafeCoerce a) (unsafeCoerce b))
            , ssSplit = \rows stAny ->
                let (s1, s2) = split rows (unsafeCoerce stAny)
                in (unsafeCoerce s1, unsafeCoerce s2)
            }
        mkState (PendingStep _ st _ _ _ _) = unsafeCoerce st

    processRows rows (PendingState statics states0) =
      let stepRow (build, states) row =
            let (row', states') = processRow statics states row
            in (build . (row' :), states')
          (build, states') = foldl' stepRow (id, states0) rows
      in (build [], PendingState statics states')

    processRow :: V.Vector (StepStatic c msg) -> V.Vector Any -> E.EntityRow c
               -> (E.EntityRow c, V.Vector Any)
    processRow statics states (eid', sig0, bag0) =
      let e = E.Entity eid'
          len = V.length statics
          go i sig bag acc =
            if i >= len
              then (sig, bag, V.fromListN len (reverse acc))
              else
                let stc = V.unsafeIndex statics i
                    st = V.unsafeIndex states i
                    (sig', bag', st') = ssStep stc e sig bag st
                in sig' `seq` bag' `seq` go (i + 1) sig' bag' (st' : acc)
          (sig', bag', states') = go 0 sig0 bag0 []
      in ((eid', sig', bag'), states')

    splitStateChunks [] _ = []
    splitStateChunks (rows : rest) (PendingState statics states0) =
      let (chunkStates, restStates) = splitStates statics rows states0
      in PendingState statics chunkStates : splitStateChunks rest (PendingState statics restStates)

    splitStates statics rows states0 =
      let len = V.length statics
          both = V.generate len $ \i ->
            let stc = V.unsafeIndex statics i
                s = V.unsafeIndex states0 i
            in ssSplit stc rows s
      in V.unzip both

    mergeStateChunks base [] = base
    mergeStateChunks base (s:ss) =
      let statics = psStatics base
          states = foldl' (mergeStates statics) (psStates s) (map psStates ss)
      in PendingState statics states

    mergeStates statics xs ys =
      let len = V.length statics
      in V.generate len $ \i ->
          let stc = V.unsafeIndex statics i
              a = V.unsafeIndex xs i
              b = V.unsafeIndex ys i
          in ssMerge stc a b

    finalizePendingState (PendingState statics states0) groups =
      let len = V.length statics
          groupCount = V.length groups
          emptyAcc = Acc mempty mempty 0
          combine acc delta =
            Acc
              { accRes = accRes acc <> accRes delta
              , accOut = accOut acc <> accOut delta
              , accCount = accCount acc + accCount delta
              }
          go i accUpdates accOut =
            if i >= len
              then (accUpdates, outToList accOut)
              else
                let stc = V.unsafeIndex statics i
                    st = V.unsafeIndex states0 i
                    StepStatic gid _ done _ _ = stc
                    (aAny, bout) = done st
                    delta = Acc (buildOne aAny) bout 1
                    accOut' = accOut <> boOut bout
                in go (i + 1) ((gid, delta) : accUpdates) accOut'
          (accUpdatesRev, outList) = go 0 [] mempty
          accUpdates = reverse accUpdatesRev
          accVec =
            if groupCount == 0
              then V.empty
              else V.accum combine (V.replicate groupCount emptyAcc) accUpdates
          applyUpdate acc gid accVal =
            if accCount accVal == 0
              then acc
              else case groups V.!? gid of
                Nothing -> acc
                Just grp ->
                  let results = V.fromList (buildToList (accRes accVal))
                      aAny = bgK grp results
                      progAny = bgCont grp aAny
                      locals' = boLocals (accOut accVal) (bgLocals grp)
                  in IntMap.insert (bgPid grp) (ProgramUpdate locals' progAny) acc
          updates = V.ifoldl' applyUpdate IntMap.empty accVec
      in (updates, outList)

    applyProgramUpdates progs updates =
      map
        (\(AnyProgram (prog0 :: Program c msg a)) ->
          let pid = handleId (programHandle prog0)
          in case IntMap.lookup pid updates of
              Nothing -> AnyProgram prog0
              Just (ProgramUpdate locals progAny) ->
                let prog' = unsafeCoerce progAny :: ProgramM c msg a
                in AnyProgram prog0 { programLocals = locals, programProg = prog' }
        )
        progs

    updateRunFlags progs flags resumed =
      map
        (\(AnyProgram prog0, runFlag) ->
          if IntSet.member (handleId (programHandle prog0)) resumed
            then True
            else runFlag
        )
        (zip progs flags)

programId :: AnyProgram c msg -> ProgramId
programId (AnyProgram s) = handleId (programHandle s)
