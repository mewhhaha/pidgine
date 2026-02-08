{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.Program
  ( Patch
  , EntityPatch
  , DirectPatch
  , patch
  , emptyPatch
  , ProgramId
  , Handle
  , handle
  , handleOf
  , Program
  , ProgramSlot
  , ProgramM
  , EntityM
  , MonadProgram
  , Batch
  , compute
  , each
  , eachP
  , eachPDirect
  , eachSet2
  , eachPSet2
  , eachM
  , eachMP
  , eachMPEdit
  , eachMPDirect
  , eachMPure
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
  , editDirect
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
  , set2
  , set3
  , setDirect
  , set2Direct
  , set3Direct
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

import Control.Monad ((>=>), ap, forM_)
import Control.Monad.ST (runST)
import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Bifunctor (first)
import Data.Bits (bit, complement, setBit, testBit, (.&.), (.|.))
import Data.Kind (Type)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word64)
import GHC.Exts (Any)
import GHC.Conc (numCapabilities)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
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

newtype EntityPatch c = EntityPatch (Sig -> E.Bag c -> (Sig, E.Bag c))

type DirectPatch c = Sig -> E.Bag c -> (Sig, E.Bag c)

emptyPatch :: Patch c
emptyPatch = mempty

instance Semigroup (Patch c) where
  Patch f <> Patch g = Patch (g . f)

instance Monoid (Patch c) where
  mempty = Patch id

instance Semigroup (EntityPatch c) where
  EntityPatch f <> EntityPatch g =
    EntityPatch (\sig bag ->
      let (sig', bag') = f sig bag
      in g sig' bag')

instance Monoid (EntityPatch c) where
  mempty = EntityPatch (,)

apply :: Patch c -> World c -> World c
apply (Patch f) = f

componentBitOfType :: forall c a. (E.Component c a, E.ComponentBit c a) => Int
componentBitOfType = E.componentBitOf @c @a

runEntityPatch :: EntityPatch c -> Sig -> E.Bag c -> (Sig, E.Bag c)
{-# INLINE runEntityPatch #-}
runEntityPatch (EntityPatch f) = f

-- Unsafe: only use when the Bag is uniquely owned by the caller.
runEntityPatchUnsafe :: EntityPatch c -> Sig -> E.Bag c -> (Sig, E.Bag c)
{-# INLINE runEntityPatchUnsafe #-}
runEntityPatchUnsafe (EntityPatch f) = f

type PlanCache c a = IntMap.IntMap (E.Bag c -> a)

cachedPlanRunner ::
  (E.Bag c -> a) ->
  Maybe (E.Sig -> E.Bag c -> a) ->
  E.Sig ->
  PlanCache c a ->
  (E.Bag c -> a, PlanCache c a)
{-# INLINE cachedPlanRunner #-}
cachedPlanRunner runP runForSig sig cache =
  let key = fromIntegral sig
  in case IntMap.lookup key cache of
      Just runPSig -> (runPSig, cache)
      Nothing ->
        let runPSig =
              case runForSig of
                Just compileForSig -> compileForSig sig
                Nothing -> runP
        in (runPSig, IntMap.insert key runPSig cache)

set :: forall a c. (E.Component c a, E.ComponentBit c a) => a -> EntityPatch c
{-# INLINE set #-}
set a =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch (\sig bag -> (sig .|. bitC, E.bagSetDirect @c @a a bag))

set2 :: forall a b c.
  (E.Component c a, E.ComponentBit c a, E.Component c b, E.ComponentBit c b)
  => a -> b -> EntityPatch c
{-# INLINE set2 #-}
set2 a b =
  let bitA = bit (componentBitOfType @c @a)
      bitB = bit (componentBitOfType @c @b)
  in EntityPatch (\sig bag ->
        (sig .|. bitA .|. bitB, E.bagSet2Direct @c @a @b a b bag))

set3 :: forall a b d c.
  (E.Component c a, E.ComponentBit c a
  ,E.Component c b, E.ComponentBit c b
  ,E.Component c d, E.ComponentBit c d
  ) => a -> b -> d -> EntityPatch c
{-# INLINE set3 #-}
set3 a b d =
  let bitA = bit (componentBitOfType @c @a)
      bitB = bit (componentBitOfType @c @b)
      bitD = bit (componentBitOfType @c @d)
  in EntityPatch (\sig bag ->
        (sig .|. bitA .|. bitB .|. bitD, E.bagSet3Direct @c @a @b @d a b d bag))

setDirect :: forall a c. (E.Component c a, E.ComponentBit c a) => a -> DirectPatch c
{-# INLINE setDirect #-}
setDirect a sig bag =
  let bitC = bit (componentBitOfType @c @a)
  in (sig .|. bitC, E.bagSetDirect @c @a a bag)

set2Direct :: forall a b c.
  (E.Component c a, E.ComponentBit c a, E.Component c b, E.ComponentBit c b)
  => a -> b -> DirectPatch c
{-# INLINE set2Direct #-}
set2Direct a b sig bag =
  let bitA = bit (componentBitOfType @c @a)
      bitB = bit (componentBitOfType @c @b)
  in (sig .|. bitA .|. bitB, E.bagSet2Direct @c @a @b a b bag)

set3Direct :: forall a b d c.
  (E.Component c a, E.ComponentBit c a
  ,E.Component c b, E.ComponentBit c b
  ,E.Component c d, E.ComponentBit c d
  ) => a -> b -> d -> DirectPatch c
{-# INLINE set3Direct #-}
set3Direct a b d sig bag =
  let bitA = bit (componentBitOfType @c @a)
      bitB = bit (componentBitOfType @c @b)
      bitD = bit (componentBitOfType @c @d)
  in (sig .|. bitA .|. bitB .|. bitD, E.bagSet3Direct @c @a @b @d a b d bag)

drive :: forall a c. (E.Component c a, E.ComponentBit c a) => F.Step () a -> EntityPatch c
drive s0 =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch (\sig bag -> (sig .|. bitC, E.bagSetStepDirect @c @a s0 bag))

update :: forall a c. (E.Component c a, E.ComponentBit c a) => (a -> a) -> EntityPatch c
update f =
  EntityPatch (\sig bag -> (sig, E.bagUpdateDirect @c @a f bag))

del :: forall a c. (E.Component c a, E.ComponentBit c a) => EntityPatch c
del =
  let bitC = bit (componentBitOfType @c @a)
  in EntityPatch (\sig bag -> (sig .&. complement bitC, E.bagDelDirect @c @a bag))

at :: Entity -> EntityPatch c -> Patch c
at e p = patch (E.mapEntities updateEntity)
  where
    updateEntity e' sig bag
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

data ProgSet
  = ProgSetSmall !Word64
  | ProgSetBig !IntSet

type Done = ProgSet
type Seen = ProgSet

progSetEmpty :: IntSet -> ProgSet
progSetEmpty allSet =
  if IntSet.null allSet
    then ProgSetSmall 0
    else
      let maxId = IntSet.findMax allSet
      in if maxId < 64
          then ProgSetSmall 0
          else ProgSetBig IntSet.empty

progSetMember :: Int -> ProgSet -> Bool
progSetMember i progSet =
  case progSet of
    ProgSetSmall bits ->
      (i >= 0 && i < 64) && testBit bits i
    ProgSetBig s -> IntSet.member i s

progSetInsert :: Int -> ProgSet -> ProgSet
progSetInsert i progSet =
  case progSet of
    ProgSetSmall bits ->
      if i >= 0 && i < 64
        then ProgSetSmall (setBit bits i)
        else ProgSetBig (IntSet.insert i IntSet.empty)
    ProgSetBig s -> ProgSetBig (IntSet.insert i s)

progSetToIntSet :: ProgSet -> IntSet
progSetToIntSet progSet =
  case progSet of
    ProgSetBig s -> s
    ProgSetSmall bits ->
      let go i acc =
            if i >= 64
              then acc
              else
                let acc' = if testBit bits i then IntSet.insert i acc else acc
                in go (i + 1) acc'
      in go 0 IntSet.empty

intSetSubsetOfProgSet :: IntSet -> ProgSet -> Bool
intSetSubsetOfProgSet xs progSet =
  IntSet.foldl' (\ok i -> ok && progSetMember i progSet) True xs

data Values
  = ValuesMap !(IntMap.IntMap Any)
  | ValuesVec !(V.Vector (Maybe Any))

valuesEmpty :: IntSet -> Values
valuesEmpty allSet =
  if IntSet.null allSet
    then ValuesVec V.empty
    else
      let maxId = IntSet.findMax allSet
          size = maxId + 1
          dense = size <= 2048
      in if dense
          then ValuesVec (V.replicate size Nothing)
          else ValuesMap IntMap.empty

valuesLookup :: ProgramId -> Values -> Maybe Any
valuesLookup sid values =
  case values of
    ValuesMap m -> IntMap.lookup sid m
    ValuesVec vec ->
      if sid >= 0 && sid < V.length vec
        then V.unsafeIndex vec sid
        else Nothing

valuesInsert :: ProgramId -> Any -> Values -> Values
valuesInsert sid v values =
  case values of
    ValuesMap m -> ValuesMap (IntMap.insert sid v m)
    ValuesVec vec ->
      if sid >= 0 && sid < V.length vec
        then ValuesVec (vec V.// [(sid, Just v)])
        else
          let m = valuesVecToMap vec
          in ValuesMap (IntMap.insert sid v m)

valuesVecToMap :: V.Vector (Maybe Any) -> IntMap.IntMap Any
valuesVecToMap =
  V.ifoldl' (\acc i mv -> case mv of
                            Nothing -> acc
                            Just v -> IntMap.insert i v acc
             ) IntMap.empty

handle :: ProgramId -> Handle a
handle = Handle

newtype Locals c msg = Locals
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

data ProgramSlot c msg where
  ProgramSlot ::
    !(Handle a) ->
    !(Locals c msg) ->
    !(ProgramM c msg a) ->
    !(ProgramM c msg a) ->
    ProgramSlot c msg

handleOf :: Program c msg a -> Handle a
handleOf = programHandle

data Inbox a = Inbox
  { eventsI :: Events a
  , doneI :: Done
  , seenI :: Seen
  , allI :: IntSet
  , valuesI :: Values
  , selfI :: ProgramId
  }

events :: Inbox a -> Events a
events = eventsI

done :: Inbox a -> IntSet
done = progSetToIntSet . doneI

seen :: Inbox a -> IntSet
seen = progSetToIntSet . seenI

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
          ready = progSetMember sid (doneI inbox)
          value = valuesLookup sid (valuesI inbox)
      in if not ready
            then Nothing
            else case value of
              Just v -> Just (unsafeCoerce v)
              Nothing -> error "await: value missing for program handle (type mismatch?)"
    Update ->
      let others = IntSet.delete (selfI inbox) (allI inbox)
          synced = intSetSubsetOfProgSet others (seenI inbox)
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

type RowVec c = V.Vector (E.EntityRow c)

data BatchGroup c msg = BatchGroup
  { bgPid :: !ProgramId
  , bgLocals :: !(Locals c msg)
  , bgK :: !(V.Vector Any -> Any)
  , bgCont :: !(Any -> Any)
  }

data KernelStep c msg where
  KernelStepStateful ::
    ProgramId ->
    E.Sig ->
    E.Sig ->
    s ->
    (Entity -> E.Sig -> E.Bag c -> s -> (E.Sig, E.Bag c, s)) ->
    (s -> (Any, BatchOut c msg)) ->
    (s -> s -> s) ->
    (RowVec c -> s -> (s, s)) ->
    KernelStep c msg
  KernelStepStateless ::
    ProgramId ->
    E.Sig ->
    E.Sig ->
    (Entity -> E.Sig -> E.Bag c -> (E.Sig, E.Bag c)) ->
    (Any, BatchOut c msg) ->
    KernelStep c msg

data KernelStepST s c msg where
  KernelStepStatefulST ::
    ProgramId ->
    E.Sig ->
    E.Sig ->
    !(STRef s st) ->
    (Entity -> E.Sig -> E.Bag c -> st -> (E.Sig, E.Bag c, st)) ->
    (st -> (Any, BatchOut c msg)) ->
    (st -> st -> st) ->
    (RowVec c -> st -> (st, st)) ->
    KernelStepST s c msg
  KernelStepStatelessST ::
    ProgramId ->
    E.Sig ->
    E.Sig ->
    (Entity -> E.Sig -> E.Bag c -> (E.Sig, E.Bag c)) ->
    (Any, BatchOut c msg) ->
    KernelStepST s c msg

data PendingState c msg = PendingState
  !(V.Vector (KernelStep c msg))
  !Bool

data CompiledBatch c msg = CompiledBatch
  { cbSteps :: !(V.Vector (KernelStep c msg))
  , cbGroup :: !(BatchGroup c msg)
  }

data ProgramUpdate c msg = ProgramUpdate
  !(Locals c msg)
  !Any

data Acc c msg = Acc
  { accRes :: !(Build Any)
  , accOut :: !(BatchOut c msg)
  , accCount :: !Int
  }

data RunRes c msg where
  Ran ::
    !(Handle a) ->
    !(ProgramM c msg a) ->
    Inbox msg ->
    Tick c msg ->
    Locals c msg ->
    ProgStep c msg a ->
    RunRes c msg
  Skipped :: ProgramSlot c msg -> RunRes c msg

newtype Graph c a = Graph [ProgramSlot c a]

class GraphArgs c msg r | r -> c msg where
  graphWith :: [ProgramSlot c msg] -> r

instance GraphArgs c msg (Graph c msg) where
  graphWith acc = Graph (reverse acc)

instance GraphArgs c msg r => GraphArgs c msg (Program c msg a -> r) where
  graphWith acc s =
    graphWith (ProgramSlot (programHandle s) (programLocals s) (programProg s) (programBase s) : acc)

program :: Handle a -> ProgramM c msg a -> Program c msg a
program h prog = ProgramDef h emptyLocals prog prog

graph :: forall msg c r. GraphArgs c msg r => r
graph = graphWith @c @msg []

graphList :: [Program c msg a] -> Graph c msg
graphList =
  Graph . map (\s -> ProgramSlot (programHandle s) (programLocals s) (programProg s) (programBase s))

graphAny :: [ProgramSlot c msg] -> Graph c msg
graphAny = Graph

data GraphState c msg = GraphState
  { gsNext :: !Int
  , gsPrograms :: ![ProgramSlot c msg]
  }

newtype GraphM c msg a = GraphM
  { runGraphM :: GraphState c msg -> (GraphState c msg, a)
  }

instance Functor (GraphM c msg) where
  fmap f (GraphM g) = GraphM $ \s ->
    let (s', a) = g s
    in (s', f a)

instance Applicative (GraphM c msg) where
  pure a = GraphM (, a)
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
  let s' =
        s
          { gsPrograms =
              ProgramSlot
                (programHandle sys)
                (programLocals sys)
                (programProg sys)
                (programBase sys)
                : gsPrograms s
          }
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

data BatchOp c msg
  = BatchOp
      !E.Sig
      !E.Sig
      (Gather c msg (Any, BatchOut c msg))
      !(Maybe (E.Sig -> RowVec c -> Any -> (Any, Bool, RowVec c)))
  | BatchOpStateless
      !E.Sig
      !E.Sig
      !(Entity -> E.Sig -> E.Bag c -> (E.Sig, E.Bag c))
      !(Any, BatchOut c msg)

data BatchRun c msg a = BatchRun
  !(V.Vector (BatchOp c msg))
  !Int
  !(V.Vector Any -> Int -> (a, Int))

data Batch c msg a = Batch
  !(DTime -> Inbox msg -> Locals c msg -> BatchRun c msg a)
  !Bool

instance Functor (Batch c msg) where
  fmap f (Batch g parOk) =
    Batch
      (\d inbox locals ->
        let BatchRun ops n k = g d inbox locals
        in BatchRun ops n (\xs i -> let (a, i') = k xs i in (f a, i'))
      )
      parOk

instance Applicative (Batch c msg) where
  pure a = Batch (\_ _ _ -> BatchRun V.empty 0 (\_ i -> (a, i))) True
  Batch gf parF <*> Batch ga parA =
    Batch
      (\d inbox locals ->
        let BatchRun opsF nF kF = gf d inbox locals
            BatchRun opsA nA kA = ga d inbox locals
            k xs i0 =
              let (f, i1) = kF xs i0
                  (a, i2) = kA xs i1
              in (f a, i2)
        in BatchRun (opsF V.++ opsA) (nF + nA) k
      )
      (parF && parA)

data Gather c msg a where
  Gather ::
    s ->
    (Entity -> E.Sig -> E.Bag c -> s -> (E.Sig, E.Bag c, s)) ->
    (s -> a) ->
    (s -> s -> s) ->
    (RowVec c -> s -> (s, s)) ->
    Gather c msg a

instance Functor (Gather c msg) where
  fmap f (Gather s stepG doneG mergeG splitG) = Gather s stepG (f . doneG) mergeG splitG

instance Applicative (Gather c msg) where
  pure a =
    Gather () (\_ sig bag s -> (sig, bag, s)) (const a) (\_ _ -> ()) (\_ s -> (s, s))
  Gather sF stepF doneF mergeF splitF <*> Gather sA stepA doneA mergeA splitA =
    Gather (sF, sA) stepG doneG mergeG splitG
    where
      stepG e sig bag (stF, stA) =
        let (sig1, bag1, stF') = stepF e sig bag stF
            (sig2, bag2, stA') = stepA e sig1 bag1 stA
        in (sig2, bag2, (stF', stA'))
      doneG (stF', stA') = doneF stF' (doneA stA')
      mergeG (stF1, stA1) (stF2, stA2) =
        (mergeF stF1 stF2, mergeA stA1 stA2)
      splitG rows (stF, stA) =
        let (stF1, stF2) = splitF rows stF
            (stA1, stA2) = splitA rows stA
        in ((stF1, stA1), (stF2, stA2))

batchRun1With ::
  E.Sig ->
  E.Sig ->
  Gather c msg (a, BatchOut c msg) ->
  Maybe (E.Sig -> RowVec c -> Any -> (Any, Bool, RowVec c)) ->
  BatchRun c msg a
batchRun1With req forb g runArchetype =
  let g' = fmap (first unsafeCoerce) g
      k xs i =
        if i < V.length xs
          then (unsafeCoerce (V.unsafeIndex xs i), i + 1)
          else error "compute: missing batch result"
  in BatchRun (V.singleton (BatchOp req forb g' runArchetype)) 1 k

batchRun1 :: E.Sig -> E.Sig -> Gather c msg (a, BatchOut c msg) -> BatchRun c msg a
batchRun1 req forb g = batchRun1With req forb g Nothing

batchRun1Stateless ::
  E.Sig ->
  E.Sig ->
  (Entity -> E.Sig -> E.Bag c -> (E.Sig, E.Bag c)) ->
  (a, BatchOut c msg) ->
  BatchRun c msg a
batchRun1Stateless req forb stepFn (a, bout) =
  let k xs i =
        if i < V.length xs
          then (unsafeCoerce (V.unsafeIndex xs i), i + 1)
          else error "compute: missing batch result"
  in
    BatchRun
      (V.singleton (BatchOpStateless req forb stepFn (unsafeCoerce a, bout)))
      1
      k

each :: E.Query c a -> (a -> EntityPatch c) -> Batch c msg ()
each q f =
  let E.Query runQ info = q
      req = E.requireQ info
      forb = E.forbidQ info
      stepQ e sig bag () =
        case runQ e bag of
          Nothing -> (sig, bag, ())
          Just a ->
            let (sig', bag') = runEntityPatchUnsafe (f a) sig bag
            in (sig', bag', ())
      doneQ () = ((), mempty)
  in Batch (\_ _ _ -> batchRun1 req forb (Gather () stepQ doneQ (\_ _ -> ()) (\_ s -> (s, s)))) True

eachP :: E.Plan c a -> (a -> EntityPatch c) -> Batch c msg ()
eachP (E.Plan req forb runP runForSig) f =
  let stepP _ sig bag cache =
        let (runPSig, cache') = cachedPlanRunner runP runForSig sig cache
            a = runPSig bag
            (sig', bag') = runEntityPatchUnsafe (f a) sig bag
        in (sig', bag', cache')
      runArchetype sig rows stAny =
        let cache0 = unsafeCoerce stAny :: PlanCache c a
            (runPSig, cache1) = cachedPlanRunner runP runForSig sig cache0
            stepRow (!moved, !rowsRevAcc) (E.EntityRow eid' rowSig bag) =
              let a = runPSig bag
                  (sig', bag') = runEntityPatchUnsafe (f a) rowSig bag
                  movedNext = moved || sig' /= sig
                  rowsRev' = E.EntityRow eid' sig' bag' : rowsRevAcc
              in movedNext `seq` rowsRev' `seq` (movedNext, rowsRev')
            (movedAny, rowsRevFinal) = V.ifoldl' (\acc _ row -> stepRow acc row) (False, []) rows
            rows' = V.fromList (reverse rowsRevFinal)
        in (unsafeCoerce cache1, movedAny, rows')
      doneP _ = ((), mempty)
      mergeP = IntMap.union
      splitP _ cache = (cache, cache)
      gather = Gather IntMap.empty stepP doneP mergeP splitP
  in Batch (\_ _ _ -> batchRun1With req forb gather (Just runArchetype)) True

eachPDirect :: E.Plan c a -> (a -> DirectPatch c) -> Batch c msg ()
eachPDirect (E.Plan req forb runP runForSig) f =
  let stepP _ sig bag cache =
        let (runPSig, cache') = cachedPlanRunner runP runForSig sig cache
            a = runPSig bag
            (sig', bag') = f a sig bag
        in (sig', bag', cache')
      runArchetype sig rows stAny =
        let cache0 = unsafeCoerce stAny :: PlanCache c a
            (runPSig, cache1) = cachedPlanRunner runP runForSig sig cache0
            stepRow (!moved, !rowsRevAcc) (E.EntityRow eid' rowSig bag) =
              let a = runPSig bag
                  (sig', bag') = f a rowSig bag
                  movedNext = moved || sig' /= sig
                  rowsRev' = E.EntityRow eid' sig' bag' : rowsRevAcc
              in movedNext `seq` rowsRev' `seq` (movedNext, rowsRev')
            (movedAny, rowsRevFinal) = V.ifoldl' (\acc _ row -> stepRow acc row) (False, []) rows
            rows' = V.fromList (reverse rowsRevFinal)
        in (unsafeCoerce cache1, movedAny, rows')
      doneP _ = ((), mempty)
      mergeP = IntMap.union
      splitP _ cache = (cache, cache)
      gather = Gather IntMap.empty stepP doneP mergeP splitP
  in Batch (\_ _ _ -> batchRun1With req forb gather (Just runArchetype)) True

eachPSet2 :: forall c a b msg.
  (E.Component c a, E.ComponentBit c a, E.Component c b, E.ComponentBit c b) =>
  E.Plan c (a, b) ->
  ((a, b) -> (a, b)) ->
  Batch c msg ()
{-# INLINE eachPSet2 #-}
eachPSet2 (E.Plan req forb runP runForSig) f =
  let bitA = bit (componentBitOfType @c @a)
      bitB = bit (componentBitOfType @c @b)
      runPSigFor sig =
        case runForSig of
          Just compileForSig -> compileForSig sig
          Nothing -> runP
      stepP _ sig bag =
        let runPSig = runPSigFor sig
            (a0, b0) = runPSig bag
            (a', b') = f (a0, b0)
            sig' = sig .|. bitA .|. bitB
            runSet2Sig = E.bagSet2DirectForSig @c @a @b sig
            bag' = runSet2Sig a' b' bag
        in (sig', bag')
  in Batch (\_ _ _ -> batchRun1Stateless req forb stepP ((), mempty)) True

eachSet2 :: forall c a b msg.
  (E.Component c a, E.ComponentBit c a, E.Component c b, E.ComponentBit c b) =>
  ((a, b) -> (a, b)) ->
  Batch c msg ()
{-# INLINE eachSet2 #-}
eachSet2 f =
  eachPSet2 (E.plan @(a, b) @c) f


data EachAcc c msg = EachAcc
  { eaOut :: !(Out msg)
  , eaOld :: !(IntMap.IntMap (ProgState c msg))
  , eaNew :: ![(Int, ProgState c msg)]
  , eaDt :: !DTime
  , eaInbox :: !(Inbox msg)
  }

emptyEachAcc :: DTime -> Inbox msg -> IntMap.IntMap (ProgState c msg) -> EachAcc c msg
emptyEachAcc d inbox prog = EachAcc mempty prog [] d inbox

mergeEachAcc :: EachAcc c msg -> EachAcc c msg -> EachAcc c msg
mergeEachAcc a b =
  EachAcc
    { eaOut = eaOut a <> eaOut b
    , eaOld = IntMap.empty
    , eaNew = eaNew a <> eaNew b
    , eaDt = eaDt a
    , eaInbox = eaInbox a
    }

data EachAccPlan c msg a = EachAccPlan
  { eapOut :: !(Out msg)
  , eapOld :: !(IntMap.IntMap (ProgState c msg))
  , eapNew :: ![(Int, ProgState c msg)]
  , eapDt :: !DTime
  , eapInbox :: !(Inbox msg)
  , eapPlanCache :: !(PlanCache c a)
  }

emptyEachAccPlan :: (E.Bag c -> a) -> DTime -> Inbox msg -> IntMap.IntMap (ProgState c msg) -> EachAccPlan c msg a
emptyEachAccPlan _ d inbox prog = EachAccPlan mempty prog [] d inbox IntMap.empty

mergeEachAccPlan :: EachAccPlan c msg a -> EachAccPlan c msg a -> EachAccPlan c msg a
mergeEachAccPlan a b =
  EachAccPlan
    { eapOut = eapOut a <> eapOut b
    , eapOld = IntMap.empty
    , eapNew = eapNew a <> eapNew b
    , eapDt = eapDt a
    , eapInbox = eapInbox a
    , eapPlanCache = IntMap.union (eapPlanCache a) (eapPlanCache b)
    }

eachM :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Query c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
{-# INLINE eachM #-}
eachM q f =
  let E.Query runQ info = q
      req = E.requireQ info
      forb = E.forbidQ info
      runMatch e _ = runQ e
  in eachMWith @key req forb runMatch f

eachMWith :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Sig ->
  E.Sig ->
  (Entity -> E.Sig -> E.Bag c -> Maybe a) ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
{-# INLINE eachMWith #-}
eachMWith req forb runMatch f =
  Batch (\d inbox locals ->
      let progMap0 :: IntMap.IntMap (ProgState c msg)
          progMap0 =
            maybe IntMap.empty unsafeCoerce (IntMap.lookup progKey (localsGlobal locals))
          acc0 = emptyEachAcc d inbox progMap0
      in batchRun1 req forb (Gather acc0 stepEntity doneEntity mergeEntity splitEntity)
    ) True
  where
    progKey = E.typeIdOf @(ProgramKey key)
    stepEntity e sig bag acc =
      case runMatch e sig bag of
        Nothing ->
          (sig, bag, acc)
        Just a ->
          let d = eaDt acc
              inbox = eaInbox acc
              eid' = E.eid e
              mState = IntMap.lookup eid' (eaOld acc)
              (prog0, machines0) =
                case mState of
                  Just (ProgState mProg machinesStored) ->
                    let prog0' = fromMaybe (f a) mProg
                    in (prog0', machinesStored)
                  Nothing -> (f a, IntMap.empty)
              (bag', sig', machines', out', waitE, prog', _) =
                runEntityM d inbox sig bag machines0 prog0
              progKeep = waitE || not (IntMap.null machines')
              progState = ProgState (if waitE then Just prog' else Nothing) machines'
              !newMap' =
                if progKeep
                  then (eid', progState) : eaNew acc
                  else eaNew acc
              !outAcc' = eaOut acc <> out'
              acc' = acc { eaOut = outAcc', eaNew = newMap' }
          in (sig', bag', acc')
    doneEntity acc =
      let updateLocals locals0 =
              let globals0 = localsGlobal locals0
                  progMap =
                    case eaNew acc of
                      [] -> IntMap.empty
                      xs -> IntMap.fromList xs
                  globals' =
                    if IntMap.null progMap
                      then IntMap.delete progKey globals0
                      else IntMap.insert progKey (unsafeCoerce progMap) globals0
              in locals0 { localsGlobal = globals' }
      in ((), BatchOut updateLocals (eaOut acc))
    mergeEntity = mergeEachAcc
    splitEntity _ acc =
      let prog = eaOld acc
          d = eaDt acc
          inbox = eaInbox acc
      in (emptyEachAcc d inbox prog, emptyEachAcc d inbox prog)

eachMP :: forall (key :: Type) c msg a.
  (Typeable key, Typeable msg) =>
  E.Plan c a ->
  (a -> EntityM c msg ()) ->
  Batch c msg ()
{-# INLINE eachMP #-}
eachMP (E.Plan req forb runP runForSig) f =
  Batch batch0 True
  where
    progKey = E.typeIdOf @(ProgramKey key)
    stepEntityWithA :: Entity -> E.Sig -> E.Bag c -> a -> EachAccPlan c msg a -> (E.Sig, E.Bag c, EachAccPlan c msg a)
    stepEntityWithA e sig bag a acc =
      let d = eapDt acc
          inbox = eapInbox acc
          eid' = E.eid e
          mState = IntMap.lookup eid' (eapOld acc)
          (prog0, machines0) =
            case mState of
              Just (ProgState mProg machinesStored) ->
                let prog0' = fromMaybe (f a) mProg
                in (prog0', machinesStored)
              Nothing -> (f a, IntMap.empty)
          (bag', sig', machines', out', waitE, prog', _) =
            runEntityM d inbox sig bag machines0 prog0
          progKeep = waitE || not (IntMap.null machines')
          progState = ProgState (if waitE then Just prog' else Nothing) machines'
          !newMap' =
            if progKeep
              then (eid', progState) : eapNew acc
              else eapNew acc
          !outAcc' = eapOut acc <> out'
          acc' =
            acc
              { eapOut = outAcc'
              , eapNew = newMap'
              }
      in (sig', bag', acc')
    stepEntity e sig bag acc =
      let (runPSig, planCache') = cachedPlanRunner runP runForSig sig (eapPlanCache acc)
          a = runPSig bag
          (sig', bag', accNoCache) = stepEntityWithA e sig bag a acc
          acc' = accNoCache { eapPlanCache = planCache' }
      in (sig', bag', acc')
    runArchetype sig rows stAny =
      let acc0 = unsafeCoerce stAny :: EachAccPlan c msg a
          (runPSig, planCache') = cachedPlanRunner runP runForSig sig (eapPlanCache acc0)
          acc1 = acc0 { eapPlanCache = planCache' }
          stepRow (!acc, !moved, !rowsRevAcc) (E.EntityRow eid' rowSig bag) =
            let e = E.Entity eid'
                a = runPSig bag
                (sig', bag', acc') = stepEntityWithA e rowSig bag a acc
                moved' = moved || sig' /= sig
                rowsRev' = E.EntityRow eid' sig' bag' : rowsRevAcc
            in moved' `seq` rowsRev' `seq` acc' `seq` (acc', moved', rowsRev')
          (acc2, moved2, rowsRevFinal) = V.ifoldl' (\acc _ row -> stepRow acc row) (acc1, False, []) rows
          rows2 = V.fromList (reverse rowsRevFinal)
      in (unsafeCoerce acc2, moved2, rows2)
    doneAcc acc =
      let updateLocals locals0 =
            let globals0 = localsGlobal locals0
                progMap =
                  case eapNew acc of
                    [] -> IntMap.empty
                    xs -> IntMap.fromList xs
                globals' =
                  if IntMap.null progMap
                    then IntMap.delete progKey globals0
                    else IntMap.insert progKey (unsafeCoerce progMap) globals0
            in locals0 { localsGlobal = globals' }
      in ((), BatchOut updateLocals (eapOut acc))
    merge = mergeEachAccPlan
    split _ acc =
      let prog = eapOld acc
          d = eapDt acc
          inbox = eapInbox acc
          cache = eapPlanCache acc
          mk = (emptyEachAccPlan runP d inbox prog) { eapPlanCache = cache }
      in (mk, mk)
    batch0 d inbox locals =
      let progMap0 :: IntMap.IntMap (ProgState c msg)
          progMap0 =
            maybe IntMap.empty unsafeCoerce (IntMap.lookup progKey (localsGlobal locals))
          acc0 = emptyEachAccPlan runP d inbox progMap0
          gather = Gather acc0 stepEntity doneAcc merge split
      in batchRun1With req forb gather (Just runArchetype)

eachMPure :: E.Plan c a -> (a -> EntityPatch c) -> Batch c msg ()
{-# INLINE eachMPure #-}
eachMPure = eachP

eachMPDirect :: E.Plan c a -> (a -> DirectPatch c) -> Batch c msg ()
{-# INLINE eachMPDirect #-}
eachMPDirect = eachPDirect

eachMPEdit :: E.Plan c a -> (a -> EntityPatch c) -> Batch c msg ()
{-# INLINE eachMPEdit #-}
eachMPEdit = eachP

collect :: E.Query c a -> Batch c msg [(Entity, a)]
collect q =
  let E.Query runQ info = q
      req = E.requireQ info
      forb = E.forbidQ info
      stepQ e sig bag acc =
        case runQ e bag of
          Nothing -> (sig, bag, acc)
          Just a -> (sig, bag, acc <> buildOne (e, a))
      doneQ acc = (buildToList acc, mempty)
  in Batch (\_ _ _ -> batchRun1 req forb (Gather mempty stepQ doneQ (<>) (\_ s -> (s, s)))) True


data ProgCtx c msg = ProgCtx
  { sysWorld :: !(World c)
  , sysLocals :: !(Locals c msg)
  , sysOut :: !(Out msg)
  , sysPatch :: !(Patch c)
  , sysDt :: {-# UNPACK #-} !DTime
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
  pure a = ProgramM (, ProgDone a)
  (<*>) = ap

instance Monad (ProgramM c msg) where
  ProgramM g >>= k = ProgramM $ \ctx ->
    case g ctx of
      (ctx', ProgDone a) -> unProgramM (k a) ctx'
      (ctx', ProgAwait w cont) -> (ctx', ProgAwait w (cont >=> k))

newtype Out msg = Out ([msg] -> [msg])

instance Semigroup (Out msg) where
  Out f <> Out g = Out (f . g)

instance Monoid (Out msg) where
  mempty = Out id

outFrom :: Events msg -> Out msg
outFrom xs = Out (\ys -> foldr (:) ys xs)

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
  , ctxSig :: {-# UNPACK #-} !E.Sig
  , ctxMachines :: !Machines
  , ctxOut :: !(Out msg)
  , ctxDt :: {-# UNPACK #-} !DTime
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
  pure a = EntityM (, Done a)
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
        s = maybe s0 unsafeCoerce (IntMap.lookup key globals0)
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
        s = maybe s0 unsafeCoerce (IntMap.lookup key machines0)
        (b, s') = stepS s (ctxDt ctx) a
        machines' = IntMap.insert key (unsafeCoerce s') machines0
    in (ctx { ctxMachines = machines' }, Done b)

edit :: EntityPatch c -> EntityM c msg ()
{-# INLINE edit #-}
edit p = EntityM $ \ctx ->
  let (sig', bag') = runEntityPatchUnsafe p (ctxSig ctx) (ctxBag ctx)
  in (ctx { ctxSig = sig', ctxBag = bag' }, Done ())

editDirect :: DirectPatch c -> EntityM c msg ()
{-# INLINE editDirect #-}
editDirect f = EntityM $ \ctx ->
  let (sig', bag') = f (ctxSig ctx) (ctxBag ctx)
  in (ctx { ctxSig = sig', ctxBag = bag' }, Done ())

world :: Patch c -> ProgramM c msg ()
world p = ProgramM $ \ctx ->
  let w' = apply p (sysWorld ctx)
      patch' = sysPatch ctx <> p
  in (ctx { sysWorld = w', sysPatch = patch' }, ProgDone ())


data TimeKey

time :: MonadProgram c msg m => m F.Time
time = step @TimeKey F.time ()

sample :: MonadProgram c msg m => F.Tween a -> m a
sample tw = F.at tw <$> time

step :: forall key a b c msg m. (MonadProgram c msg m, Typeable key, Typeable a, Typeable b) => F.Step a b -> a -> m b
step = stepId (E.typeIdOf @key)

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
      !sig' = ctxSig ctx1
      !bag' = ctxBag ctx1
      !machines' = ctxMachines ctx1
      !out' = ctxOut ctx1
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
  let emptySet = progSetEmpty allSet
  in go w0 programs0 (repeat True) emptySet emptySet (valuesEmpty allSet) mempty False
  where
    Graph programs0 = g0
    allSet = foldl' (\acc p -> IntSet.insert (programId p) acc) IntSet.empty programs0
    inboxOut = outFrom inbox

    go w programs toRun doneSet seenSet values accOut stepped =
      let available = outToList (inboxOut <> accOut)
          (w', roundOut, programs', nextRun, done1, seen1, values1, progressed, stepped') =
            stepRound d w available programs toRun doneSet seenSet values allSet stepped
          !accOut' = accOut <> roundOut
      in if not (or nextRun) || not progressed
          then (w', outToList accOut', Graph programs')
          else go w' programs' nextRun done1 seen1 values1 accOut' stepped'

stepRound :: DTime
          -> World c
          -> Events a
          -> [ProgramSlot c a]
          -> [Bool]
          -> Done
          -> Seen
          -> Values
          -> IntSet
          -> Bool
          -> (World c, Out a, [ProgramSlot c a], [Bool], Done, Seen, Values, Bool, Bool)
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
      let stepRun acc pair =
            applyOne acc (runOne inb doneSet seenSet valuesSet w pair)
          (w', out', progs', runs', dSet', sSet', vSet', progressed', pending') =
            foldl' stepRun (w, mempty, [], [], doneSet, seenSet, valuesSet, False, []) pairs
      in (w', out', reverse progs', reverse runs', dSet', sSet', vSet', progressed', reverse pending')
      where
        runOne inbox0 dSet sSet vSet worldRun (slot0@(ProgramSlot (h :: Handle a) locals0 prog0 base0), runNow) =
          if runNow
            then
              let sid = handleId h
                  inbox1 = Inbox inbox0 dSet sSet allSet vSet sid
                  (t, locals', res) = runProgramM d worldRun inbox1 locals0 prog0
              in Ran h base0 inbox1 t locals' res
            else
              Skipped slot0

        applyOne (wAcc, accOut, accProg, accRun, dSet, sSet, vSet, progressed, pending) res =
          case res of
            Skipped slot0 ->
              (wAcc, accOut, slot0 : accProg, False : accRun, dSet, sSet, vSet, progressed, pending)
            Ran (h :: Handle a) base0 inbox0 t locals' resStep ->
              let sid = handleId h
                  out = outT t
                  !accOut' = accOut <> outFrom out
                  w' = apply (patchT t) wAcc
                  sSet' = progSetInsert sid sSet
                  progressedSeen = not (progSetMember sid sSet)
              in case resStep of
                  ProgDone _ ->
                    let slot1 = ProgramSlot h locals' base0 base0
                        accProg' = slot1 : accProg
                        accRun' = False : accRun
                        dSet' = progSetInsert sid dSet
                        progressedDone = not (progSetMember sid dSet)
                        vSet' = case valueT t of
                          Nothing -> vSet
                          Just v -> valuesInsert sid v vSet
                        progressed' = progressed || progressedDone || progressedSeen || not (null out)
                    in (w', accOut', accProg', accRun', dSet', sSet', vSet', progressed', pending)
                  ProgAwait waitOn cont ->
                    let progWait = await waitOn >>= cont
                        slot1 = ProgramSlot h locals' progWait base0
                        accProg' = slot1 : accProg
                        (pending', runFlag) =
                          case waitOn of
                            BatchWait b -> (PendingBatch sid locals' inbox0 b cont : pending, False)
                            _ -> (pending, True)
                        accRun' = runFlag : accRun
                        progressed' = progressed || progressedSeen || not (null out)
                    in (w', accOut', accProg', accRun', dSet, sSet', vSet, progressed', pending')

    runBatchesPhase dTime w pending programs0 runFlags steppedAlready =
      let pendingSorted = pending
          wStep =
            if steppedAlready
              then w
              else
                if E.worldHasSteps w
                  then E.stepWorld dTime w
                  else w
          stepped1 = True
          compiled = map (compilePending dTime) pendingSorted
          steps =
            V.concat $
              map cbSteps compiled
          groups =
            foldl' (\acc c ->
                      let grp = cbGroup c
                      in IntMap.insert (bgPid grp) grp acc
                   ) IntMap.empty compiled
          parOk = all pendingPar pendingSorted
          hasStateful = V.any (not . isStatelessStep) steps
          state0 = PendingState steps hasStateful
          (w', state') =
            if parOk
              then runPendingStepsPar state0 wStep
              else runPendingSteps state0 wStep
          (updates, out) = finalizePendingState state' groups
          programs' = applyProgramUpdates programs0 updates
          runFlags' = updateRunFlags programs' runFlags (IntMap.keysSet updates)
          progressed = not (IntMap.null updates) || not (null out)
      in (w', outFrom out, programs', runFlags', progressed, stepped1)

    pendingPar (PendingBatch _ _ _ (Batch _ parOk) _) = parOk

    compilePending dTime (PendingBatch pid locals inbox b cont) =
      let Batch runBatch _ = b
          BatchRun ops n k = runBatch dTime inbox locals
          kAny xs = unsafeCoerce (fst (k xs 0))
          contAny v = unsafeCoerce (cont (unsafeCoerce v))
          stepsList =
            V.foldr
              (\op acc -> opToStep pid op : acc)
              []
              ops
          steps = V.fromListN n stepsList
          group =
            BatchGroup
              { bgPid = pid
              , bgLocals = locals
              , bgK = kAny
              , bgCont = contAny
              }
      in CompiledBatch steps group

    opToStep pid op =
      case op of
        BatchOp req forb g _runArchetype ->
          case g of
            Gather s stepFn doneFn mergeFn splitFn ->
              KernelStepStateful pid req forb s stepFn doneFn mergeFn splitFn
        BatchOpStateless req forb stepFn doneConst ->
          KernelStepStateless pid req forb stepFn doneConst

    parMinRows :: Int
    parMinRows = 3072

    parRowsPerChunk :: Int
    parRowsPerChunk = 1024

    parWorkPerChunk :: Int
    parWorkPerChunk = 1200

    chooseParallelChunks rowCount stepCount =
      let caps = max 1 numCapabilities
          work = rowCount * max 1 stepCount
          byRows = max 1 (rowCount `quot` parRowsPerChunk)
          byWork = max 1 (work `quot` parWorkPerChunk)
          chunks = min caps (min byRows byWork)
      in
        if caps <= 1 || rowCount < parMinRows
          then 1
          else max 1 chunks

    splitRowChunks chunkCount rows =
      let rowCount = V.length rows
          mk i =
            let start = (i * rowCount) `quot` chunkCount
                end = ((i + 1) * rowCount) `quot` chunkCount
                len = end - start
            in if len <= 0 then Nothing else Just (V.slice start len rows)
      in foldr
          (\i acc ->
            case mk i of
              Nothing -> acc
              Just r -> r : acc
          )
          []
          [0 .. chunkCount - 1]

    splitStateAcross splitFn st chunks =
      case chunks of
        [] -> []
        [_] -> [st]
        rowsL : rest ->
          let (sL, sR) = splitFn rowsL st
          in sL : splitStateAcross splitFn sR rest

    splitStepsForChunks steps chunks =
      let chunkCount = length chunks
      in case chunkCount of
          0 -> []
          1 -> [steps]
          _ ->
            let perStep =
                  V.map
                    (\step ->
                      case step of
                        KernelStepStateful pid req forb st stepFn doneFn mergeFn splitFn ->
                          let states = splitStateAcross splitFn st chunks
                          in map (\s -> KernelStepStateful pid req forb s stepFn doneFn mergeFn splitFn) states
                        KernelStepStateless {} ->
                          replicate chunkCount step
                    )
                    steps
                chunkSteps i =
                  V.generate (V.length steps) (\j -> (V.unsafeIndex perStep j) !! i)
            in map chunkSteps [0 .. chunkCount - 1]

    mergeChunkSteps stepsByChunk =
      case stepsByChunk of
        [] -> V.empty
        [st] -> st
        st0 : rest ->
          foldl'
            (\acc stN ->
              V.imap
                (\i stepA ->
                  case stepA of
                    KernelStepStateful pid req forb st stepFn doneFn mergeFn splitFn ->
                      case V.unsafeIndex stN i of
                        KernelStepStateful _ _ _ stB _ _ _ _ ->
                          let stB' = unsafeCoerce stB
                          in KernelStepStateful pid req forb (mergeFn st stB') stepFn doneFn mergeFn splitFn
                        _ ->
                          error "mergeChunkSteps: mismatched step shape"
                    KernelStepStateless {} ->
                      stepA
                )
                acc
            )
            st0
            rest

    isStatelessStep step =
      case step of
        KernelStepStateless {} -> True
        KernelStepStateful {} -> False

    runKernelRowsStateless steps0 rows0 =
      let len = V.length steps0
          matchesSig req forb sig = (sig .&. req) == req && (sig .&. forb) == 0
          runRow (E.EntityRow eid' sig0 bag0) =
            let e = E.Entity eid'
                runStep !i !sig !bag =
                  if i >= len
                    then (sig, bag)
                    else
                      case V.unsafeIndex steps0 i of
                        KernelStepStateless _ req forb stepFn _ ->
                          if matchesSig req forb sig
                            then
                              let (sig', bag') = stepFn e sig bag
                              in runStep (i + 1) sig' bag'
                            else runStep (i + 1) sig bag
                        KernelStepStateful {} ->
                          error "runKernelRowsStateless: stateful step"
            in
              let (sig1, bag1) = runStep 0 sig0 bag0
              in E.EntityRow eid' sig1 bag1
      in (V.map runRow rows0, steps0)

    runKernelRowsStateful steps0 rows0 =
      let len = V.length steps0
          rowCount = V.length rows0
      in runST $ do
          stepsST <-
            V.generateM len $ \i ->
              case V.unsafeIndex steps0 i of
                KernelStepStateful pid req forb st stepFn doneFn mergeFn splitFn -> do
                  ref <- newSTRef st
                  pure (KernelStepStatefulST pid req forb ref stepFn doneFn mergeFn splitFn)
                KernelStepStateless pid req forb stepFn doneConst ->
                  pure (KernelStepStatelessST pid req forb stepFn doneConst)
          rowsM <- MV.new rowCount
          let matchesSig req forb sig = (sig .&. req) == req && (sig .&. forb) == 0
              runRow (E.EntityRow eid' sig0 bag0) =
                let e = E.Entity eid'
                    runStep !i !sig !bag =
                      if i >= len
                        then pure (sig, bag)
                        else
                          let op = V.unsafeIndex stepsST i
                          in case op of
                              KernelStepStatefulST _ req forb ref stepFn _ _ _ ->
                                if matchesSig req forb sig
                                  then do
                                    st0 <- readSTRef ref
                                    let (sig', bag', stNext) = stepFn e sig bag st0
                                    stNext `seq` writeSTRef ref stNext
                                    runStep (i + 1) sig' bag'
                                  else runStep (i + 1) sig bag
                              KernelStepStatelessST _ req forb stepFn _ ->
                                if matchesSig req forb sig
                                  then do
                                    let (sig', bag') = stepFn e sig bag
                                    runStep (i + 1) sig' bag'
                                  else runStep (i + 1) sig bag
                in do
                  (sig1, bag1) <- runStep 0 sig0 bag0
                  pure (E.EntityRow eid' sig1 bag1)
          forM_ [0 .. rowCount - 1] $ \i -> do
            let row0 = V.unsafeIndex rows0 i
            row' <- runRow row0
            MV.unsafeWrite rowsM i row'
          stepsFinal <-
            V.generateM len $ \i ->
              case V.unsafeIndex stepsST i of
                KernelStepStatefulST pid req forb ref stepFn doneFn mergeFn splitFn -> do
                  st <- readSTRef ref
                  pure (KernelStepStateful pid req forb st stepFn doneFn mergeFn splitFn)
                KernelStepStatelessST pid req forb stepFn doneConst ->
                  pure (KernelStepStateless pid req forb stepFn doneConst)
          rowsFinal <- V.unsafeFreeze rowsM
          pure (rowsFinal, stepsFinal)

    runPendingSteps state0 wStep =
      case state0 of
        PendingState steps0 hasStateful ->
          if V.null steps0
            then (wStep, state0)
            else
              let rows0 = E.entityRowsV wStep
                  (rows', steps') =
                    if hasStateful
                      then runKernelRowsStateful steps0 rows0
                      else runKernelRowsStateless steps0 rows0
                  w' = E.setEntityRowsVSameShape rows' wStep
              in (w', PendingState steps' hasStateful)

    runPendingStepsPar state0 wStep =
      case state0 of
        PendingState steps0 hasStateful ->
          if V.null steps0
            then (wStep, state0)
            else
              let rows0 = E.entityRowsV wStep
                  rowCount = V.length rows0
                  chunkCount = chooseParallelChunks rowCount (V.length steps0)
                  runChunk =
                    if hasStateful
                      then runKernelRowsStateful
                      else runKernelRowsStateless
              in
                if chunkCount <= 1
                  then runPendingSteps state0 wStep
                  else
                    let rowChunks = splitRowChunks chunkCount rows0
                    in
                      if length rowChunks <= 1
                        then runPendingSteps state0 wStep
                        else
                          let stepChunks =
                                if hasStateful
                                  then splitStepsForChunks steps0 rowChunks
                                  else replicate (length rowChunks) steps0
                              (headRows, tailRows) =
                                case rowChunks of
                                  r : rs -> (r, rs)
                                  [] -> error "runPendingStepsPar: empty row chunks"
                              (headSteps, tailSteps) =
                                case stepChunks of
                                  s : ss -> (s, ss)
                                  [] -> error "runPendingStepsPar: empty step chunks"
                              -- Keep one chunk on the current capability and spark the rest.
                              headResult = runChunk headSteps headRows
                              tailResults =
                                withStrategy (parList rseq) $
                                  zipWith runChunk tailSteps tailRows
                              chunkResults = headResult : tailResults
                              rows' = V.concat (map fst chunkResults)
                              steps' =
                                if hasStateful
                                  then mergeChunkSteps (map snd chunkResults)
                                  else steps0
                              w' = E.setEntityRowsVSameShape rows' wStep
                          in (w', PendingState steps' hasStateful)

    finalizePendingState (PendingState steps0 _) groups =
      let len = V.length steps0
          -- IntMap.insertWith passes (new, old). Preserve original op order as old <> new.
          combine new old =
            Acc
              { accRes = accRes old <> accRes new
              , accOut = accOut old <> accOut new
              , accCount = accCount old + accCount new
              }
          go i pendingUpdates accOut =
            if i >= len
              then (pendingUpdates, outToList accOut)
              else
                let step = V.unsafeIndex steps0 i
                    (pid, aAny, bout) =
                      case step of
                        KernelStepStateful pid _ _ st _ doneFn _ _ ->
                          let (a, bout) = doneFn st
                          in (pid, a, bout)
                        KernelStepStateless pid _ _ _ doneConst ->
                          let (a, bout) = doneConst
                          in (pid, a, bout)
                    delta = Acc (buildOne aAny) bout 1
                    !accOut' = accOut <> boOut bout
                in go (i + 1) ((pid, delta) : pendingUpdates) accOut'
          (accUpdatesRev, outList) = go 0 [] mempty
          accUpdates = reverse accUpdatesRev
          accMap =
            foldl'
              (\acc (pid, delta) -> IntMap.insertWith combine pid delta acc)
              IntMap.empty
              accUpdates
          applyUpdate acc pid accVal =
            if accCount accVal == 0
              then acc
              else case IntMap.lookup pid groups of
                Nothing -> acc
                Just grp ->
                  let results = V.fromList (buildToList (accRes accVal))
                      aAny = bgK grp results
                      progAny = bgCont grp aAny
                      locals0 = bgLocals grp
                      locals' = boLocals (accOut accVal) locals0
                  in IntMap.insert pid (ProgramUpdate locals' progAny) acc
          updates = IntMap.foldlWithKey' applyUpdate IntMap.empty accMap
      in (updates, outList)

    applyProgramUpdates progs updates = go progs
      where
        go [] = []
        go (ProgramSlot (h :: Handle a) locals0 prog0 base0 : rest) =
          let pid = handleId h
              slot' =
                case IntMap.lookup pid updates of
                  Nothing -> ProgramSlot h locals0 prog0 base0
                  Just (ProgramUpdate locals progAny) ->
                    let progM = unsafeCoerce progAny :: ProgramM c msg a
                    in ProgramSlot h locals progM base0
          in slot' : go rest

    updateRunFlags progs flags resumed =
      go progs flags
      where
        go [] _ = []
        go _ [] = []
        go (ProgramSlot h _ _ _ : ps) (runFlag : fs) =
          let runFlag' =
                IntSet.member (handleId h) resumed || runFlag
          in runFlag' : go ps fs

programId :: ProgramSlot c msg -> ProgramId
programId (ProgramSlot h _ _ _) = handleId h
