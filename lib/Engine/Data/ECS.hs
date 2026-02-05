{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.ECS
  ( Entity(..)
  , World
  , Bag
  , Key
  , Steps
  , StepSlot(..)
  , Sig
  , EntityRow(..)
  , emptyWorld
  , entities
  , entityRows
  , entityRowsV
  , matchingArchetypes
  , matchingIndices
  , matchingCandidates
  , applyEntityRowUpdates
  , applyArchetypeRunUpdates
  , stepWorld
  , foldEntities
  , foldEntitiesFrom
  , mapEntities
  , setEntityRows
  , setEntityRowsV
  , nextId
  , spawn
  , kill
  , Component(..)
  , ComponentId(..)
  , ComponentBit(..)
  , Bundle(..)
  , Has(..)
  , Not(..)
  , has
  , get
  , set
  , del
  , Query(..)
  , QueryInfo(..)
  , queryInfo
  , Plan(..)
  , plan
  , planRec
  , planMap
  , sigFromBag
  , runQuerySig
  , keyOf
  , keyOfType
  , BagEdit
  , bagEditSet
  , bagEditSetStep
  , bagEditUpdate
  , bagEditDel
  , bagApplyEdit
  , bagApplyEditUnsafe
  , bagSetDirect
  , bagSet2Direct
  , bagSet3Direct
  , bagSetStepDirect
  , bagUpdateDirect
  , bagDelDirect
  , bagGet
  , bagSet
  , bagSetStep
  , bagDel
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
import Data.Bits (bit, complement, countTrailingZeros, finiteBitSize, popCount, (.&.), (.|.), xor)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Constraint, Type)
import qualified Data.Foldable as Foldable
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, cast, typeRep, typeRepFingerprint)
import qualified Data.Vector as V
import qualified Data.RRBVector as RV
import Data.Word (Word64)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Exts (Any, isTrue#, reallyUnsafePtrEquality#)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)
import qualified Engine.Data.FRP as F

newtype Entity = Entity
  { eid :: Int
  } deriving (Eq, Ord, Show)

type TypeId = Int

typeIdOf :: forall a. Typeable a => TypeId
typeIdOf =
  let Fingerprint w1 w2 = typeRepFingerprint (typeRep (Proxy @a))
  in fromIntegral (w1 `xor` w2)

newtype Key c = Key Int
  deriving (Eq, Ord, Show)

keyOf :: ComponentId c => c -> Key c
keyOf = Key . componentBit

data StepSlot = StepSlot
  { stepVal :: !Any
  , stepState :: !Any
  }

newtype Steps = Steps
  { stepsMap :: IntMap StepSlot
  }

data Bag c = Bag
  { bagStaticMask :: !Sig
  , bagStaticVals :: !(V.Vector Any)
  , bagSteps :: !Steps
  }

type Sig = Word64

type ArchKey = Int

archKey :: Sig -> ArchKey
archKey = fromIntegral

sigFromKey :: ArchKey -> Sig
sigFromKey = fromIntegral

data EntityRow c = EntityRow
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Sig
  !(Bag c)

data BagOp
  = BagOpSet !Int !Any
  | BagOpSetStep !Int !StepSlot
  | BagOpUpdate !Int (Any -> Any)
  | BagOpDel !Int

newtype BagEdit c = BagEdit
  { bagEditOps :: [BagOp]
  }

instance Semigroup (BagEdit c) where
  BagEdit xs <> BagEdit ys = BagEdit (ys <> xs)

instance Monoid (BagEdit c) where
  mempty = BagEdit []

stepsEmpty :: Steps
stepsEmpty = Steps IntMap.empty

stepsLookup :: Int -> Steps -> Maybe StepSlot
stepsLookup bitIx (Steps m) = IntMap.lookup bitIx m

stepsInsert :: Int -> StepSlot -> Steps -> Steps
stepsInsert bitIx slot (Steps m) = Steps (IntMap.insert bitIx slot m)

stepsDelete :: Int -> Steps -> Steps
stepsDelete bitIx (Steps m) = Steps (IntMap.delete bitIx m)

stepsStepAll :: F.DTime -> Steps -> Steps
stepsStepAll d (Steps m) =
  let stepSlotD (StepSlot _ sAny) =
        let s = unsafeCoerce sAny :: F.Step () Any
            (v', s') = F.stepS s d ()
        in StepSlot v' (unsafeCoerce s')
  in Steps (IntMap.map stepSlotD m)

stepsNull :: Steps -> Bool
stepsNull (Steps m) = IntMap.null m

emptyBag :: forall c. ComponentId c => Bag c
emptyBag =
  let size = componentCount @c
      limit = finiteBitSize (0 :: Word64)
  in if size > limit
      then error ("ComponentId: componentCount exceeds Sig bit width (" <> show limit <> ")")
      else Bag 0 V.empty stepsEmpty

stepsMerge :: Steps -> Steps -> Steps
stepsMerge (Steps base) (Steps extra) =
  Steps (IntMap.union extra base)

maskHas :: Sig -> Int -> Bool
{-# INLINE maskHas #-}
maskHas mask bitIx = (mask .&. bit bitIx) /= 0

lowerBitsMask :: Int -> Sig
{-# INLINE lowerBitsMask #-}
lowerBitsMask bitIx
  | bitIx <= 0 = 0
  | otherwise = bit bitIx - 1

staticIndex :: Sig -> Int -> Int
{-# INLINE staticIndex #-}
staticIndex mask bitIx = popCount (mask .&. lowerBitsMask bitIx)

vectorInsertAt :: Int -> Any -> V.Vector Any -> V.Vector Any
{-# INLINE vectorInsertAt #-}
vectorInsertAt i x vec =
  let (l, r) = V.splitAt i vec
  in l V.++ V.singleton x V.++ r

vectorDeleteAt :: Int -> V.Vector Any -> V.Vector Any
{-# INLINE vectorDeleteAt #-}
vectorDeleteAt i vec =
  let (l, r0) = V.splitAt i vec
      r = V.drop 1 r0
  in l V.++ r

staticLookup :: Sig -> V.Vector Any -> Int -> Maybe Any
{-# INLINE staticLookup #-}
staticLookup mask vals bitIx =
  if maskHas mask bitIx
    then Just (V.unsafeIndex vals (staticIndex mask bitIx))
    else Nothing

staticSetAny :: Sig -> V.Vector Any -> Int -> Any -> (Sig, V.Vector Any)
{-# INLINE staticSetAny #-}
staticSetAny mask vals bitIx vAny =
  if maskHas mask bitIx
    then
      let i = staticIndex mask bitIx
          old = V.unsafeIndex vals i
      in if ptrEq old vAny
          then (mask, vals)
          else (mask, vals V.// [(i, vAny)])
    else
      let i = staticIndex mask bitIx
          mask' = mask .|. bit bitIx
      in (mask', vectorInsertAt i vAny vals)

staticDelAny :: Sig -> V.Vector Any -> Int -> (Sig, V.Vector Any)
{-# INLINE staticDelAny #-}
staticDelAny mask vals bitIx =
  if maskHas mask bitIx
    then
      let i = staticIndex mask bitIx
          mask' = mask .&. complement (bit bitIx)
      in (mask', vectorDeleteAt i vals)
    else (mask, vals)

mergeStatic :: Sig -> V.Vector Any -> Sig -> V.Vector Any -> (Sig, V.Vector Any)
mergeStatic mask0 vals0 maskExtra valsExtra =
  let go !mask !vals !i !sigBits =
        if sigBits == 0
          then (mask, vals)
          else
            let bitIx = countTrailingZeros sigBits
                sigBits' = sigBits .&. (sigBits - 1)
                vAny = V.unsafeIndex valsExtra i
                (mask', vals') = staticSetAny mask vals bitIx vAny
            in go mask' vals' (i + 1) sigBits'
  in go mask0 vals0 0 maskExtra

instance ComponentId c => Semigroup (Bag c) where
  Bag mask1 vals1 st1 <> Bag mask2 vals2 st2 =
    let (mask', vals') = mergeStatic mask1 vals1 mask2 vals2
    in Bag mask' vals' (stepsMerge st1 st2)

instance ComponentId c => Monoid (Bag c) where
  mempty = emptyBag

data Loc = Loc !Sig !Int

data World c = World
  { nextIdW :: !Int
  , entityCountW :: !Int
  , archW :: !(IntMap (RV.Vector (EntityRow c)))
  , entityLocW :: !(IntMap Loc)
  , resourcesW :: !(IntMap Any)
  , relOutW :: !(IntMap (IntMap IntSet))
  , relInW :: !(IntMap (IntMap IntSet))
  }

emptyWorld :: World c
emptyWorld =
  World
    { nextIdW = 0
    , entityCountW = 0
    , archW = IntMap.empty
    , entityLocW = IntMap.empty
    , resourcesW = IntMap.empty
    , relOutW = IntMap.empty
    , relInW = IntMap.empty
    }

locInsert :: Int -> Loc -> IntMap Loc -> IntMap Loc
locInsert = IntMap.insert

locDelete :: Int -> IntMap Loc -> IntMap Loc
locDelete = IntMap.delete

entities :: World c -> [Entity]
entities w =
  foldr
    (\(EntityRow eid' _ _) acc -> Entity eid' : acc)
    []
    (entityRows w)

entityRows :: World c -> [EntityRow c]
entityRows w =
  IntMap.foldr
    (\rows acc -> Foldable.toList rows <> acc)
    []
    (archW w)

entityRowsV :: World c -> RV.Vector (EntityRow c)
entityRowsV w = RV.fromList (entityRows w)

nextId :: World c -> Int
nextId = nextIdW

foldEntitiesFrom :: [EntityRow c] -> (Entity -> Sig -> Bag c -> s -> s) -> s -> World c -> s
foldEntitiesFrom rows f s0 _ =
  foldl'
    (\acc (EntityRow eid' sig bag) -> f (Entity eid') sig bag acc)
    s0
    rows

foldEntities :: (Entity -> Sig -> Bag c -> s -> s) -> s -> World c -> s
foldEntities f s0 w =
  IntMap.foldl'
    (Foldable.foldl'
      (\acc' (EntityRow eid' sig bag) -> f (Entity eid') sig bag acc')
    )
    s0
    (archW w)

mapEntities :: (Entity -> Sig -> Bag c -> Bag c) -> World c -> World c
mapEntities f w =
  let stepRow (archAcc, locAcc, n) (EntityRow eid' sig bag) =
        let e = Entity eid'
            bag' = f e sig bag
            sig' = sigFromBag bag'
            row' = EntityRow eid' sig' bag'
            (archAcc', locAcc') = insertRowArchLoc row' archAcc locAcc
        in (archAcc', locAcc', n + 1)
      stepRun = Foldable.foldl' stepRow
      (archTmp, locs', nRows) =
        IntMap.foldl'
          stepRun
          (IntMap.empty, IntMap.empty, 0)
          (archW w)
      arch' = finalizeArchRuns archTmp
  in w
      { archW = arch'
      , entityLocW = locs'
      , entityCountW = nRows
      }

setEntityRows :: [EntityRow c] -> World c -> World c
setEntityRows rows = setEntityRowsV (RV.fromList rows)

setEntityRowsV :: RV.Vector (EntityRow c) -> World c -> World c
setEntityRowsV rows w =
  let (arch', locs') = buildArchAndLocsV rows
  in w
      { archW = arch'
      , entityLocW = locs'
      , entityCountW = Foldable.length rows
      }

buildArchAndLocsV :: RV.Vector (EntityRow c) -> (IntMap (RV.Vector (EntityRow c)), IntMap Loc)
buildArchAndLocsV rows =
  let step _ (archAcc, locAcc) row =
        insertRowArchLoc row archAcc locAcc
      (archTmp, locs) = RV.ifoldl' step (IntMap.empty, IntMap.empty) rows
      arch = finalizeArchRuns archTmp
  in (arch, locs)

insertRowArchLoc
  :: EntityRow c
  -> IntMap (Int, [EntityRow c])
  -> IntMap Loc
  -> (IntMap (Int, [EntityRow c]), IntMap Loc)
insertRowArchLoc row@(EntityRow eid' sig _) archAcc locAcc =
  let key = archKey sig
  in case IntMap.lookup key archAcc of
      Nothing ->
        let archAcc' = IntMap.insert key (1 :: Int, [row]) archAcc
            locAcc' = locInsert eid' (Loc sig 0) locAcc
        in (archAcc', locAcc')
      Just (n, rs) ->
        let archAcc' = IntMap.insert key (n + 1, row : rs) archAcc
            locAcc' = locInsert eid' (Loc sig n) locAcc
        in (archAcc', locAcc')

finalizeArchRuns :: IntMap (Int, [EntityRow c]) -> IntMap (RV.Vector (EntityRow c))
finalizeArchRuns =
  IntMap.foldlWithKey'
    (\archAcc key (_, rs) ->
      IntMap.insert key (RV.fromList (reverse rs)) archAcc
    )
    IntMap.empty

applyEntityRowUpdates :: [(Sig, Int, EntityRow c)] -> World c -> World c
applyEntityRowUpdates updates w =
  case updates of
    [] -> w
    _ ->
      let updatesBySig =
            foldl'
              (\acc (sig, idx, row) ->
                IntMap.insertWith IntMap.union (archKey sig) (IntMap.singleton idx row) acc
              )
              IntMap.empty
              updates
          arch0 = archW w
          locs0 = entityLocW w
          (arch1, locs1, moved) =
            IntMap.foldlWithKey'
              (\(archAcc, locAcc, movedAcc) key updMap ->
                let sig = sigFromKey key
                in case IntMap.lookup key archAcc of
                  Nothing -> (archAcc, locAcc, movedAcc)
                  Just rows0 ->
                    let anyMoved =
                          IntMap.foldr
                            (\(EntityRow _ newSig _) acc -> newSig /= sig || acc)
                            False
                            updMap
                    in
                      if not anyMoved
                        then
                          let rows' = RV.imap (\i row -> IntMap.findWithDefault row i updMap) rows0
                              archAcc' = IntMap.insert key rows' archAcc
                          in (archAcc', locAcc, movedAcc)
                        else
                          let step i (rowsAcc, movedRowsAcc) row =
                                case IntMap.lookup i updMap of
                                  Nothing -> (row : rowsAcc, movedRowsAcc)
                                  Just row' ->
                                    let EntityRow _ newSig _ = row'
                                    in
                                      if newSig == sig
                                        then (row' : rowsAcc, movedRowsAcc)
                                        else (rowsAcc, IntMap.insertWith (++) (archKey newSig) [row'] movedRowsAcc)
                              (rowsRev, movedAcc') = RV.ifoldl' step ([], movedAcc) rows0
                              rows' = RV.fromList (reverse rowsRev)
                              archAcc' =
                                if Foldable.null rows'
                                  then IntMap.delete key archAcc
                                  else IntMap.insert key rows' archAcc
                              locAcc' = setLocsForRun locAcc sig rows'
                          in (archAcc', locAcc', movedAcc')
              )
              (arch0, locs0, IntMap.empty)
              updatesBySig
          (arch2, locs2) =
            IntMap.foldlWithKey'
              (\(archAcc, locAcc) key rowsToAdd ->
                let sig = sigFromKey key
                    rows0 = IntMap.findWithDefault RV.empty key archAcc
                    start = Foldable.length rows0
                    rows1 = rows0 RV.>< RV.fromList rowsToAdd
                    locAcc' =
                      foldl'
                        (\acc (i, EntityRow eid' _ _) ->
                          locInsert eid' (Loc sig (start + i)) acc
                        )
                        locAcc
                        (zip [0 ..] rowsToAdd)
                in
                  (IntMap.insert key rows1 archAcc, locAcc')
              )
              (arch1, locs1)
              moved
      in w
          { archW = arch2
          , entityLocW = locs2
          }

-- | Replace whole archetype runs without touching the entity->loc table.
--
-- This is safe / only valid when:
-- - Every row in the run keeps the same archetype signature.
-- - Entity ordering and run lengths are unchanged (so indices remain valid).
--
-- It exists to support fast "update all rows in-place" style operations (e.g. program stepping)
-- without constructing per-row update lists.
applyArchetypeRunUpdates :: [(Sig, RV.Vector (EntityRow c))] -> World c -> World c
applyArchetypeRunUpdates runs w =
  case runs of
    [] -> w
    _ ->
      let arch1 =
            foldl'
              (\archAcc (sig, rows) ->
                IntMap.insert (archKey sig) rows archAcc
              )
              (archW w)
              runs
      in w
          { archW = arch1 }

setLocsForRun :: IntMap Loc -> Sig -> RV.Vector (EntityRow c) -> IntMap Loc
setLocsForRun locs sig =
  RV.ifoldl'
    (\i acc (EntityRow eid' _ _) -> locInsert eid' (Loc sig i) acc)
    locs

matchingArchetypes :: Sig -> Sig -> World c -> [(Sig, RV.Vector (EntityRow c))]
matchingArchetypes req forb w =
  IntMap.foldrWithKey'
    (\key rows acc ->
      let sig = sigFromKey key
      in if (sig .&. req) == req && (sig .&. forb) == 0
          then (sig, rows) : acc
          else acc
    )
    []
    (archW w)

matchingIndices :: Sig -> Sig -> World c -> [Int]
matchingIndices req forb w =
  foldr
    (\(_, rows) acc ->
      Foldable.foldr
        (\(EntityRow eid' _ _) acc' -> eid' : acc')
        acc
        rows
    )
    []
    (matchingArchetypes req forb w)

matchingCandidates :: Sig -> Sig -> World c -> Maybe [Int]
matchingCandidates req forb w =
  if req == 0
    then Nothing
    else Just (matchingIndices req forb w)

class Typeable a => Component c a where
  inj :: a -> c
  prj :: c -> Maybe a
  default inj :: (Generic c, GInj (Rep c) a) => a -> c
  inj a =
    case gInj @(Rep c) a of
      Just v -> to v
      Nothing -> error "Component: type not present in component wrapper"
  default prj :: (Generic c, GPrj (Rep c) a) => c -> Maybe a
  prj c = gPrj (from c)

instance {-# OVERLAPPABLE #-} (Typeable a, Generic c, Assert (HasType (Rep c) a) a c, GInj (Rep c) a, GPrj (Rep c) a) => Component c a

type family EqT (a :: Type) (b :: Type) :: Bool where
  EqT a a = 'True
  EqT _ _ = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False b = b

type family HasType (f :: Type -> Type) (a :: Type) :: Bool where
  HasType (l :+: r) a = Or (HasType l a) (HasType r a)
  HasType (M1 _ _ f) a = HasType f a
  HasType (K1 _ b) a = EqT a b
  HasType U1 _ = 'False
  HasType (l :*: r) _ =
    TypeError ('Text "Component wrapper must be a sum of unary constructors; product found")
  HasType _ _ = 'False

type family Assert (b :: Bool) (a :: Type) (c :: Type) :: Constraint where
  Assert 'True _ _ = ()
  Assert 'False a c =
    TypeError
      ( 'Text "Component type "
          ':<>: 'ShowType a
          ':<>: 'Text " not present in wrapper "
          ':<>: 'ShowType c
      )

class GInj f a where
  gInj :: a -> Maybe (f p)

instance (GInj l a, GInj r a) => GInj (l :+: r) a where
  gInj x =
    case gInj @l x of
      Just v -> Just (L1 v)
      Nothing -> R1 <$> gInj @r x

instance GInj f a => GInj (M1 i c f) a where
  gInj x = M1 <$> gInj @f x

instance (Typeable a, Typeable b) => GInj (K1 i b) a where
  gInj x = K1 <$> cast x

instance GInj U1 a where
  gInj _ = Nothing

instance (GInj a1 a, GInj b1 a) => GInj (a1 :*: b1) a where
  gInj _ = error "Component: product constructors not supported"

class GPrj f a where
  gPrj :: f p -> Maybe a

instance (GPrj l a, GPrj r a) => GPrj (l :+: r) a where
  gPrj (L1 l) = gPrj l
  gPrj (R1 r) = gPrj r

instance GPrj f a => GPrj (M1 i c f) a where
  gPrj (M1 x) = gPrj x

instance (Typeable a, Typeable b) => GPrj (K1 i b) a where
  gPrj (K1 b) = cast b

instance GPrj U1 a where
  gPrj _ = Nothing

instance (GPrj a1 a, GPrj b1 a) => GPrj (a1 :*: b1) a where
  gPrj _ = error "Component: product constructors not supported"

class ComponentId c where
  componentBit :: c -> Int
  componentCount :: Int
  default componentBit :: (Generic c, GConstrIndex (Rep c)) => c -> Int
  componentBit = gConstrIndex . from
  default componentCount :: (Generic c, GCount (Rep c)) => Int
  componentCount = gCount @(Rep c)

class GCount f where
  gCount :: Int

instance (GCount l, GCount r) => GCount (l :+: r) where
  gCount = gCount @l + gCount @r

instance GCount f => GCount (M1 i c f) where
  gCount = gCount @f

instance GCount (K1 i a) where
  gCount = 1

instance GCount U1 where
  gCount = 1

instance (GCount a, GCount b) => GCount (a :*: b) where
  gCount = error "ComponentId: product constructors not supported"

class GConstrIndex f where
  gConstrIndex :: f p -> Int

instance (GConstrIndex l, GConstrIndex r, GCount l) => GConstrIndex (l :+: r) where
  gConstrIndex (L1 l) = gConstrIndex l
  gConstrIndex (R1 r) = gCount @l + gConstrIndex r

instance GConstrIndex f => GConstrIndex (M1 i c f) where
  gConstrIndex (M1 x) = gConstrIndex x

instance GConstrIndex (K1 i a) where
  gConstrIndex _ = 0

instance GConstrIndex U1 where
  gConstrIndex _ = 0

instance (GConstrIndex a, GConstrIndex b) => GConstrIndex (a :*: b) where
  gConstrIndex _ = error "ComponentId: product constructors not supported"

class ComponentBit c a where
  componentBitOf :: Int
  default componentBitOf :: (Typeable a, Generic c, GComponentBit (Rep c) a, GCount (Rep c)) => Int
  componentBitOf =
    case gComponentBit @(Rep c) @a 0 of
      Just ix -> ix
      Nothing -> error "ComponentBit: type not present in component wrapper"

instance {-# OVERLAPPABLE #-} (Typeable a, Component c a, Generic c, GComponentBit (Rep c) a, GCount (Rep c)) => ComponentBit c a

class GComponentBit f a where
  gComponentBit :: Int -> Maybe Int

instance (GComponentBit l a, GComponentBit r a, GCount l) => GComponentBit (l :+: r) a where
  gComponentBit n =
    case gComponentBit @l @a n of
      Just ix -> Just ix
      Nothing -> gComponentBit @r @a (n + gCount @l)

instance GComponentBit f a => GComponentBit (M1 i c f) a where
  gComponentBit = gComponentBit @f @a

instance (Typeable a, Typeable b) => GComponentBit (K1 i b) a where
  gComponentBit n =
    if typeRep (Proxy @a) == typeRep (Proxy @b)
      then Just n
      else Nothing

instance GComponentBit U1 a where
  gComponentBit _ = Nothing

instance (GComponentBit a a1, GComponentBit b a1) => GComponentBit (a :*: b) a1 where
  gComponentBit _ = error "ComponentBit: product constructors not supported"

class ComponentId c => Bundle c a where
  bundle :: a -> Bag c

instance {-# OVERLAPPING #-} ComponentId c => Bundle c () where
  bundle _ = emptyBag

instance {-# INCOHERENT #-} (ComponentId c, Component c a, ComponentBit c a) => Bundle c a where
  bundle a = bagSet a emptyBag

instance Bundle c a => Bundle c (Maybe a) where
  bundle = maybe emptyBag bundle

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b) => Bundle c (a, b) where
  bundle (a, b) = bundle a <> bundle b

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d) => Bundle c (a, b, d) where
  bundle (a, b, d) = bundle a <> bundle b <> bundle d

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d, Bundle c e) => Bundle c (a, b, d, e) where
  bundle (a, b, d, e) = bundle a <> bundle b <> bundle d <> bundle e

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f) => Bundle c (a, b, d, e, f) where
  bundle (a, b, d, e, f) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g) => Bundle c (a, b, d, e, f, g) where
  bundle (a, b, d, e, f, g) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g, Bundle c h) => Bundle c (a, b, d, e, f, g, h) where
  bundle (a, b, d, e, f, g, h) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g <> bundle h

instance {-# OVERLAPPING #-} (Bundle c a, Bundle c b, Bundle c d, Bundle c e, Bundle c f, Bundle c g, Bundle c h, Bundle c i) => Bundle c (a, b, d, e, f, g, h, i) where
  bundle (a, b, d, e, f, g, h, i) = bundle a <> bundle b <> bundle d <> bundle e <> bundle f <> bundle g <> bundle h <> bundle i

spawn :: Bundle c a => a -> World c -> (Entity, World c)
spawn a w =
  let e = Entity (nextIdW w)
      bag = bundle a
      sig = sigFromBag bag
      eid' = eid e
      key = archKey sig
      rows0 = IntMap.findWithDefault RV.empty (archKey sig) (archW w)
      idx = Foldable.length rows0
      rows' = rows0 RV.|> EntityRow eid' sig bag
      arch' = IntMap.insert key rows' (archW w)
      locs' = locInsert eid' (Loc sig idx) (entityLocW w)
      w' =
        w
          { nextIdW = nextIdW w + 1
          , entityCountW = entityCountW w + 1
          , archW = arch'
          , entityLocW = locs'
          }
  in (e, w')

kill :: Entity -> World c -> World c
kill e w =
  let eid' = eid e
      out' = dropRelEntity eid' (relOutW w)
      in' = dropRelEntity eid' (relInW w)
  in case IntMap.lookup eid' (entityLocW w) of
      Nothing ->
        w { relOutW = out', relInW = in' }
      Just (Loc sig idx) ->
        let arch0 = archW w
            key = archKey sig
            rows0 = IntMap.findWithDefault RV.empty key arch0
            len = Foldable.length rows0
            lastIdx = len - 1
            rows1 =
              if idx == lastIdx
                then RV.take lastIdx rows0
                else
                  let rowLast = rows0 RV.! lastIdx
                      rows' = RV.update idx rowLast rows0
                  in RV.take lastIdx rows'
            arch1 =
              if Foldable.null rows1
                then IntMap.delete key arch0
                else IntMap.insert key rows1 arch0
            locs1 =
              if idx == lastIdx
                then locDelete eid' (entityLocW w)
                else
                  let EntityRow eidLast _ _ = rows0 RV.! lastIdx
                      acc1 = locDelete eid' (entityLocW w)
                  in locInsert eidLast (Loc sig idx) acc1
        in w
            { archW = arch1
            , entityLocW = locs1
            , entityCountW = entityCountW w - 1
            , relOutW = out'
            , relInW = in'
            }

dropRelEntity :: Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
dropRelEntity eid' =
  IntMap.mapMaybe (\m ->
    let m' = IntMap.map (IntSet.delete eid') (IntMap.delete eid' m)
        m'' = IntMap.filter (not . IntSet.null) m'
    in if IntMap.null m'' then Nothing else Just m'')

sigFromBag :: Bag c -> Sig
sigFromBag bag =
  let sigStatic = bagStaticMask bag
      steps = bagSteps bag
      sigSteps = IntMap.foldlWithKey' (\acc bitIx _ -> acc .|. bit bitIx) 0 (stepsMap steps)
  in sigStatic .|. sigSteps

matchSig :: QueryInfo -> Sig -> Bool
matchSig info sig =
  (sig .&. requireQ info) == requireQ info
    && (sig .&. forbidQ info) == 0

runQuerySig :: Query c a -> Sig -> Entity -> Bag c -> Maybe a
runQuerySig q sig e bag =
  if matchSig (queryInfoQ q) sig
    then runQuery q e bag
    else Nothing

keyOfType :: forall c a. (Component c a, ComponentBit c a) => Key c
keyOfType = Key (componentBitOf @c @a)

bagGetBy :: Int -> Bag c -> Maybe Any
bagGetBy bitIx (Bag mask vals steps) =
  if stepsNull steps
    then staticLookup mask vals bitIx
    else
      case stepsLookup bitIx steps of
        Just slot -> Just (stepVal slot)
        Nothing -> staticLookup mask vals bitIx

bagGetByTyped :: forall a c. Int -> Bag c -> Maybe a
bagGetByTyped bitIx bag = unsafeCoerce <$> bagGetBy bitIx bag

bagGetByUnsafe :: forall a c. Int -> Bag c -> a
bagGetByUnsafe bitIx bag =
  case bagGetBy bitIx bag of
    Just v -> unsafeCoerce v
    Nothing -> error "bagGetByUnsafe: missing component (signature mismatch?)"

bagGetByUnsafe2 :: forall a b c. Int -> Int -> Bag c -> (a, b)
{-# INLINE bagGetByUnsafe2 #-}
bagGetByUnsafe2 bitA bitB (Bag mask vals steps) =
  let unsafeStatic bitIx =
        case staticLookup mask vals bitIx of
          Just v -> v
          Nothing -> error "bagGetByUnsafe2: missing component (signature mismatch?)"
  in if stepsNull steps
      then
        ( unsafeCoerce (unsafeStatic bitA)
        , unsafeCoerce (unsafeStatic bitB)
        )
      else
        let getAny bitIx =
              case stepsLookup bitIx steps of
                Just slot -> stepVal slot
                Nothing -> unsafeStatic bitIx
        in (unsafeCoerce (getAny bitA), unsafeCoerce (getAny bitB))

bagGetByUnsafe3 :: forall a b d c. Int -> Int -> Int -> Bag c -> (a, b, d)
{-# INLINE bagGetByUnsafe3 #-}
bagGetByUnsafe3 bitA bitB bitD (Bag mask vals steps) =
  let unsafeStatic bitIx =
        case staticLookup mask vals bitIx of
          Just v -> v
          Nothing -> error "bagGetByUnsafe3: missing component (signature mismatch?)"
  in if stepsNull steps
      then
        ( unsafeCoerce (unsafeStatic bitA)
        , unsafeCoerce (unsafeStatic bitB)
        , unsafeCoerce (unsafeStatic bitD)
        )
      else
        let getAny bitIx =
              case stepsLookup bitIx steps of
                Just slot -> stepVal slot
                Nothing -> unsafeStatic bitIx
        in
          ( unsafeCoerce (getAny bitA)
          , unsafeCoerce (getAny bitB)
          , unsafeCoerce (getAny bitD)
          )

planRunForSig1 :: forall a c. Int -> Sig -> Bag c -> a
{-# INLINE planRunForSig1 #-}
planRunForSig1 bitA sig =
  if maskHas sig bitA
    then
      let idxA = staticIndex sig bitA
      in \bag@(Bag staticMask vals steps) ->
          if stepsNull steps && staticMask == sig
            then unsafeCoerce (V.unsafeIndex vals idxA)
            else bagGetByUnsafe bitA bag
    else bagGetByUnsafe bitA

planRunForSig2 :: forall a b c. Int -> Int -> Sig -> Bag c -> (a, b)
{-# INLINE planRunForSig2 #-}
planRunForSig2 bitA bitB sig =
  if maskHas sig bitA && maskHas sig bitB
    then
      let idxA = staticIndex sig bitA
          idxB = staticIndex sig bitB
      in \bag@(Bag staticMask vals steps) ->
          if stepsNull steps && staticMask == sig
            then
              ( unsafeCoerce (V.unsafeIndex vals idxA)
              , unsafeCoerce (V.unsafeIndex vals idxB)
              )
            else bagGetByUnsafe2 bitA bitB bag
    else bagGetByUnsafe2 bitA bitB

planRunForSig3 :: forall a b d c. Int -> Int -> Int -> Sig -> Bag c -> (a, b, d)
{-# INLINE planRunForSig3 #-}
planRunForSig3 bitA bitB bitD sig =
  if maskHas sig bitA && maskHas sig bitB && maskHas sig bitD
    then
      let idxA = staticIndex sig bitA
          idxB = staticIndex sig bitB
          idxD = staticIndex sig bitD
      in \bag@(Bag staticMask vals steps) ->
          if stepsNull steps && staticMask == sig
            then
              ( unsafeCoerce (V.unsafeIndex vals idxA)
              , unsafeCoerce (V.unsafeIndex vals idxB)
              , unsafeCoerce (V.unsafeIndex vals idxD)
              )
            else bagGetByUnsafe3 bitA bitB bitD bag
    else bagGetByUnsafe3 bitA bitB bitD

bagGet :: forall c a. (Component c a, ComponentBit c a) => Bag c -> Maybe a
bagGet = bagGetByTyped (componentBitOf @c @a)

ptrEq :: Any -> Any -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

bagSetByAny :: Int -> Any -> Bag c -> Bag c
{-# INLINE bagSetByAny #-}
bagSetByAny bitIx vAny (Bag mask vals steps) =
  case stepsLookup bitIx steps of
    Just (StepSlot vOld sAny) ->
      let steps' =
            if ptrEq vOld vAny
              then steps
              else stepsInsert bitIx (StepSlot vAny sAny) steps
          (mask', vals') = staticDelAny mask vals bitIx
      in Bag mask' vals' steps'
    Nothing ->
      let (mask', vals') = staticSetAny mask vals bitIx vAny
      in Bag mask' vals' steps

bagSetStepSlotBy :: Int -> StepSlot -> Bag c -> Bag c
{-# INLINE bagSetStepSlotBy #-}
bagSetStepSlotBy bitIx slot (Bag mask vals steps) =
  let steps' = stepsInsert bitIx slot steps
      (mask', vals') = staticDelAny mask vals bitIx
  in Bag mask' vals' steps'

bagUpdateByAny :: Int -> (Any -> Any) -> Bag c -> Bag c
{-# INLINE bagUpdateByAny #-}
bagUpdateByAny bitIx fAny (Bag mask vals steps) =
  case stepsLookup bitIx steps of
    Just (StepSlot vAny sAny) ->
      let vAny' = fAny vAny
      in if ptrEq vAny vAny'
          then Bag mask vals steps
          else Bag mask vals (stepsInsert bitIx (StepSlot vAny' sAny) steps)
    Nothing ->
      case staticLookup mask vals bitIx of
        Nothing -> Bag mask vals steps
        Just vAny ->
          let vAny' = fAny vAny
          in if ptrEq vAny vAny'
              then Bag mask vals steps
              else
                let (mask', vals') = staticSetAny mask vals bitIx vAny'
                in Bag mask' vals' steps

bagDelByBit :: Int -> Bag c -> Bag c
{-# INLINE bagDelByBit #-}
bagDelByBit bitIx (Bag mask vals steps) =
  let steps' = stepsDelete bitIx steps
      (mask', vals') = staticDelAny mask vals bitIx
  in Bag mask' vals' steps'

bagEditSet :: forall c a. (Component c a, ComponentBit c a) => a -> BagEdit c
bagEditSet a =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in BagEdit [BagOpSet bitIx vAny]

bagEditSetStep :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> BagEdit c
bagEditSetStep s0 =
  let stepAny = unsafeCoerce s0 :: F.Step () Any
      (vAny, s1) = F.stepS stepAny 0 ()
      bitIx = componentBitOf @c @a
      slot = StepSlot vAny (unsafeCoerce s1)
  in BagEdit [BagOpSetStep bitIx slot]

bagEditUpdate :: forall c a. (Component c a, ComponentBit c a) => (a -> a) -> BagEdit c
bagEditUpdate f =
  let bitIx = componentBitOf @c @a
      fAny v = unsafeCoerce (f (unsafeCoerce v))
  in BagEdit [BagOpUpdate bitIx fAny]

bagEditDel :: forall c a. (Component c a, ComponentBit c a) => BagEdit c
bagEditDel =
  let bitIx = componentBitOf @c @a
  in BagEdit [BagOpDel bitIx]

bagApplyEdit :: BagEdit c -> Bag c -> Bag c
bagApplyEdit edit bag0 =
  foldl'
    (\bag op ->
      case op of
        BagOpSet bitIx vAny -> bagSetByAny bitIx vAny bag
        BagOpSetStep bitIx slot -> bagSetStepSlotBy bitIx slot bag
        BagOpUpdate bitIx fAny -> bagUpdateByAny bitIx fAny bag
        BagOpDel bitIx -> bagDelByBit bitIx bag
    )
    bag0
    (bagEditOps edit)

bagApplyEditUnsafe :: BagEdit c -> Bag c -> Bag c
bagApplyEditUnsafe = bagApplyEdit

bagSetDirect :: forall c a. (Component c a, ComponentBit c a) => a -> Bag c -> Bag c
{-# INLINE bagSetDirect #-}
bagSetDirect a bag =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in bagSetByAny bitIx vAny bag

bagSet2Direct :: forall c a b.
  (Component c a, ComponentBit c a, Component c b, ComponentBit c b)
  => a -> b -> Bag c -> Bag c
{-# INLINE bagSet2Direct #-}
bagSet2Direct a b bag0 =
  let bitA = componentBitOf @c @a
      bitB = componentBitOf @c @b
      vA = unsafeCoerce a
      vB = unsafeCoerce b
  in bagSetByAny bitB vB (bagSetByAny bitA vA bag0)

bagSet3Direct :: forall c a b d.
  (Component c a, ComponentBit c a
  , Component c b, ComponentBit c b
  , Component c d, ComponentBit c d
  ) => a -> b -> d -> Bag c -> Bag c
{-# INLINE bagSet3Direct #-}
bagSet3Direct a b d bag0 =
  let bitA = componentBitOf @c @a
      bitB = componentBitOf @c @b
      bitD = componentBitOf @c @d
      vA = unsafeCoerce a
      vB = unsafeCoerce b
      vD = unsafeCoerce d
  in bagSetByAny bitD vD (bagSetByAny bitB vB (bagSetByAny bitA vA bag0))

bagSetStepDirect :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> Bag c -> Bag c
bagSetStepDirect s0 bag =
  let stepAny = unsafeCoerce s0 :: F.Step () Any
      (vAny, s1) = F.stepS stepAny 0 ()
      bitIx = componentBitOf @c @a
      slot = StepSlot vAny (unsafeCoerce s1)
  in bagSetStepSlotBy bitIx slot bag

bagUpdateDirect :: forall c a. (Component c a, ComponentBit c a) => (a -> a) -> Bag c -> Bag c
{-# INLINE bagUpdateDirect #-}
bagUpdateDirect f bag =
  let bitIx = componentBitOf @c @a
      fAny v = unsafeCoerce (f (unsafeCoerce v))
  in bagUpdateByAny bitIx fAny bag

bagDelDirect :: forall c a. (Component c a, ComponentBit c a) => Bag c -> Bag c
{-# INLINE bagDelDirect #-}
bagDelDirect bag =
  let bitIx = componentBitOf @c @a
  in bagDelByBit bitIx bag

bagSet :: forall a c. (Component c a, ComponentBit c a) => a -> Bag c -> Bag c
bagSet = bagSetDirect

bagSetStep :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> Bag c -> Bag c
bagSetStep = bagSetStepDirect

bagDel :: forall a c. (Component c a, ComponentBit c a) => Bag c -> Bag c
bagDel = bagDelDirect @c @a

get :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> Maybe a
get e w =
  case lookupRow (eid e) w of
    Nothing -> Nothing
    Just (_, bag) -> bagGet bag

set :: forall a c. (Component c a, ComponentBit c a) => Entity -> a -> World c -> World c
set e a = updateEntityBag (bagSet a) e

del :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> World c
del = updateEntityBag (bagDel @a)

updateEntityBag :: (Bag c -> Bag c) -> Entity -> World c -> World c
{-# INLINE updateEntityBag #-}
updateEntityBag updateBag e w =
  let eid' = eid e
  in case IntMap.lookup eid' (entityLocW w) of
      Nothing -> w
      Just (Loc sig idx) ->
        case IntMap.lookup (archKey sig) (archW w) of
          Nothing -> w
          Just rows ->
            let EntityRow _ oldSig bag = rows RV.! idx
                bag' = updateBag bag
                sig' = sigFromBag bag'
                row' = EntityRow eid' sig' bag'
            in if sig' == oldSig
                then
                  let rows' = RV.update idx row' rows
                      arch' = IntMap.insert (archKey sig) rows' (archW w)
                  in w { archW = arch' }
                else
                  let (arch1, locs1) = removeFromRun sig idx (archW w) (entityLocW w)
                      (arch2, locs2) = appendToRun sig' row' arch1 locs1
                  in w
                      { archW = arch2
                      , entityLocW = locs2
                      }

has :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> Bool
has e w = isJust (get @a e w)

lookupRow :: Int -> World c -> Maybe (Sig, Bag c)
lookupRow eid' w =
  case IntMap.lookup eid' (entityLocW w) of
    Nothing -> Nothing
    Just (Loc sig idx) ->
      case IntMap.lookup (archKey sig) (archW w) of
        Nothing -> Nothing
        Just rows ->
          let EntityRow _ sig' bag = rows RV.! idx
          in Just (sig', bag)

removeFromRun :: Sig -> Int -> IntMap (RV.Vector (EntityRow c)) -> IntMap Loc
              -> (IntMap (RV.Vector (EntityRow c)), IntMap Loc)
removeFromRun sig idx arch0 locs0 =
  let key = archKey sig
      rows0 = IntMap.findWithDefault RV.empty key arch0
      len = Foldable.length rows0
      lastIdx = len - 1
      rows1 =
        if idx == lastIdx
          then RV.take lastIdx rows0
          else
            let rowLast = rows0 RV.! lastIdx
                rows' = RV.update idx rowLast rows0
            in RV.take lastIdx rows'
      arch1 =
        if Foldable.null rows1
          then IntMap.delete key arch0
          else IntMap.insert key rows1 arch0
      locs1 =
        if idx == lastIdx
          then locs0
          else
            let EntityRow eidLast _ _ = rows0 RV.! lastIdx
            in locInsert eidLast (Loc sig idx) locs0
  in (arch1, locs1)

appendToRun :: Sig -> EntityRow c -> IntMap (RV.Vector (EntityRow c)) -> IntMap Loc
            -> (IntMap (RV.Vector (EntityRow c)), IntMap Loc)
appendToRun sig row@(EntityRow eid' _ _) arch0 locs0 =
  let key = archKey sig
      rows0 = IntMap.findWithDefault RV.empty key arch0
      idx = Foldable.length rows0
      rows1 = rows0 RV.|> row
      arch1 = IntMap.insert key rows1 arch0
      locs1 = locInsert eid' (Loc sig idx) locs0
  in (arch1, locs1)

data QueryInfo = QueryInfo
  { requireQ :: Sig
  , forbidQ :: Sig
  } deriving (Eq, Show)

instance Semigroup QueryInfo where
  QueryInfo r1 f1 <> QueryInfo r2 f2 = QueryInfo (r1 .|. r2) (f1 .|. f2)

instance Monoid QueryInfo where
  mempty = QueryInfo 0 0

data Query c a = Query
  { runQuery :: Entity -> Bag c -> Maybe a
  , queryInfoQ :: QueryInfo
  }

data Plan c a = Plan
  { planReq :: !Sig
  , planForbid :: !Sig
  , planRun :: !(Bag c -> a)
  , planForSig :: !(Maybe (Sig -> Bag c -> a))
  }

queryInfo :: Query c a -> QueryInfo
queryInfo = queryInfoQ

instance Functor (Query c) where
  fmap f (Query q qi) = Query (\e bag -> fmap f (q e bag)) qi

instance Applicative (Query c) where
  pure a = Query (\_ _ -> Just a) mempty
  Query f qiF <*> Query g qiG = Query (\e bag -> f e bag <*> g e bag) (qiF <> qiG)

instance Monad (Query c) where
  Query q _ >>= f = Query (\e bag -> q e bag >>= \a -> runQuery (f a) e bag) mempty

instance Alternative (Query c) where
  empty = Query (\_ _ -> Nothing) mempty
  Query a _ <|> Query b _ = Query (\e bag -> a e bag <|> b e bag) mempty

comp :: forall a c. (Component c a, ComponentBit c a) => Query c a
comp =
  let bitIx = componentBitOf @c @a
  in Query (\_ bag -> bagGetByTyped bitIx bag) (QueryInfo (bit bitIx) 0)

opt :: forall a c. (Component c a, ComponentBit c a) => Query c (Maybe a)
opt =
  let bitIx = componentBitOf @c @a
  in Query (\_ bag -> Just (bagGetByTyped bitIx bag)) mempty

data Has a = Has deriving (Eq, Show)
data Not a = Not deriving (Eq, Show)

hasQ :: forall a c. (Component c a, ComponentBit c a) => Query c (Has a)
hasQ =
  let bitIx = componentBitOf @c @a
  in Query (\_ bag -> if isJust (bagGetByTyped @a bitIx bag) then Just Has else Nothing)
      (QueryInfo (bit bitIx) 0)

notQ :: forall a c. (Component c a, ComponentBit c a) => Query c (Not a)
notQ =
  let bitIx = componentBitOf @c @a
  in Query (\_ bag -> if isJust (bagGetByTyped @a bitIx bag) then Nothing else Just Not)
      (QueryInfo 0 (bit bitIx))

runq :: Query c a -> World c -> [(Entity, a)]
runq q w =
  let info = queryInfoQ q
      req = requireQ info
      forb = forbidQ info
      stepRow (EntityRow eid' _ bag) acc =
        case runQuery q (Entity eid') bag of
          Nothing -> acc
          Just a -> (Entity eid', a) : acc
      stepRun key rows acc =
        let sig = sigFromKey key
        in if (sig .&. req) == req && (sig .&. forb) == 0
            then Foldable.foldr stepRow acc rows
            else acc
  in IntMap.foldrWithKey' stepRun [] (archW w)

foldq :: Query c a -> (Entity -> a -> s -> s) -> s -> World c -> s
foldq q step s0 w =
  let info = queryInfoQ q
      req = requireQ info
      forb = forbidQ info
      stepRow acc (EntityRow eid' _ bag) =
        case runQuery q (Entity eid') bag of
          Nothing -> acc
          Just a -> step (Entity eid') a acc
      stepRun acc key rows =
        let sig = sigFromKey key
        in if (sig .&. req) == req && (sig .&. forb) == 0
            then Foldable.foldl' stepRow acc rows
            else acc
  in IntMap.foldlWithKey' stepRun s0 (archW w)

filterQ :: (a -> Bool) -> Query c a -> Query c a
filterQ f (Query q qi) =
  Query (\e bag -> do
    a <- q e bag
    if f a then Just a else Nothing
  ) qi

class QueryField c a where
  fieldQuery :: Query c a

instance {-# OVERLAPPABLE #-} (Component c a, ComponentBit c a) => QueryField c a where
  fieldQuery = comp

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => QueryField c (Maybe a) where
  fieldQuery = opt

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => QueryField c (Has a) where
  fieldQuery = hasQ

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => QueryField c (Not a) where
  fieldQuery = notQ

class PlanField c a where
  fieldPlan :: (Sig, Sig, Bag c -> a)

instance {-# OVERLAPPABLE #-} (Component c a, ComponentBit c a) => PlanField c a where
  fieldPlan =
    let bitIx = componentBitOf @c @a
    in (bit bitIx, 0, bagGetByUnsafe bitIx)

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => PlanField c (Maybe a) where
  fieldPlan =
    let bitIx = componentBitOf @c @a
    in (0, 0, bagGetByTyped bitIx)

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => PlanField c (Has a) where
  fieldPlan =
    let bitIx = componentBitOf @c @a
    in (bit bitIx, 0, const Has)

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => PlanField c (Not a) where
  fieldPlan =
    let bitIx = componentBitOf @c @a
    in (0, bit bitIx, const Not)

class Queryable c a where
  queryC :: Query c a
  default queryC :: (Generic a, GQueryable c (Rep a)) => Query c a
  queryC = to <$> gquery

class QueryableSum c a where
  querySumC :: Query c a
  default querySumC :: (Generic a, GQueryableSum c (Rep a)) => Query c a
  querySumC = to <$> gquerySum

class Plannable c a where
  planC :: Plan c a

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a, Component c b, ComponentBit c b) => Plannable c (a, b) where
  planC =
    let bitA = componentBitOf @c @a
        bitB = componentBitOf @c @b
        req = bit bitA .|. bit bitB
    in Plan req 0 (bagGetByUnsafe2 bitA bitB) (Just (planRunForSig2 bitA bitB))

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a, Component c b, ComponentBit c b, Component c d, ComponentBit c d) => Plannable c (a, b, d) where
  planC =
    let bitA = componentBitOf @c @a
        bitB = componentBitOf @c @b
        bitD = componentBitOf @c @d
        req = bit bitA .|. bit bitB .|. bit bitD
    in Plan req 0 (bagGetByUnsafe3 bitA bitB bitD) (Just (planRunForSig3 bitA bitB bitD))

instance {-# OVERLAPPABLE #-} (PlanField c a, PlanField c b) => Plannable c (a, b) where
  planC =
    let (reqA, forbA, runA) = fieldPlan @c @a
        (reqB, forbB, runB) = fieldPlan @c @b
    in Plan (reqA .|. reqB) (forbA .|. forbB) (\bag -> (runA bag, runB bag)) Nothing

instance {-# OVERLAPPABLE #-} (PlanField c a, PlanField c b, PlanField c d) => Plannable c (a, b, d) where
  planC =
    let (reqA, forbA, runA) = fieldPlan @c @a
        (reqB, forbB, runB) = fieldPlan @c @b
        (reqD, forbD, runD) = fieldPlan @c @d
    in Plan
      (reqA .|. reqB .|. reqD)
      (forbA .|. forbB .|. forbD)
      (\bag -> (runA bag, runB bag, runD bag))
      Nothing

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => Plannable c a where
  planC =
    let bitA = componentBitOf @c @a
        req = bit bitA
    in Plan req 0 (bagGetByUnsafe bitA) (Just (planRunForSig1 bitA))

instance {-# OVERLAPPABLE #-} PlanField c a => Plannable c a where
  planC =
    let (req, forb, runF) = fieldPlan @c @a
    in Plan req forb runF Nothing

query :: forall a c. Queryable c a => Query c a
query = queryC @c @a

querySum :: forall a c. QueryableSum c a => Query c a
querySum = querySumC @c @a

plan :: forall a c. Plannable c a => Plan c a
{-# NOINLINE plan #-}
plan = planC @c @a

planRec :: forall a c. (Generic a, GPlan c (Rep a)) => Plan c a
{-# NOINLINE planRec #-}
planRec =
  let (req, forb, runF) = gplan @c @(Rep a)
  in Plan req forb (to . runF) Nothing

planMap :: (a -> b) -> Plan c a -> Plan c b
{-# INLINE planMap #-}
planMap f (Plan req forb runP runSigP) =
  Plan req forb (f . runP) (fmap (\runForSig sig -> f . runForSig sig) runSigP)

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

class GPlan c f where
  gplan :: (Sig, Sig, Bag c -> f p)

instance GPlan c U1 where
  gplan = (0, 0, const U1)

instance GPlan c f => GPlan c (M1 i m f) where
  gplan =
    let (req, forb, runF) = gplan @c @f
    in (req, forb, M1 . runF)

instance (GPlan c a, GPlan c b) => GPlan c (a :*: b) where
  gplan =
    let (reqA, forbA, runA) = gplan @c @a
        (reqB, forbB, runB) = gplan @c @b
        run bag = runA bag :*: runB bag
    in (reqA .|. reqB, forbA .|. forbB, run)

instance PlanField c a => GPlan c (K1 i a) where
  gplan =
    let (req, forb, runF) = fieldPlan @c @a
    in (req, forb, K1 . runF)

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
stepWorld :: F.DTime -> World c -> World c
stepWorld d w =
  let stepRow (EntityRow eid' sig bag) =
        if stepsNull (bagSteps bag)
          then EntityRow eid' sig bag
          else EntityRow eid' sig (stepBag d bag)
  in w { archW = IntMap.map (RV.map stepRow) (archW w) }

stepBag :: F.DTime -> Bag c -> Bag c
stepBag d bag = bag { bagSteps = stepsStepAll d (bagSteps bag) }
