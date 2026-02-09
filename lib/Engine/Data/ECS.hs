{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  , Sig
  , EntityRow(..)
  , emptyWorld
  , entities
  , entityRows
  , entityRowsV
  , matchingRows
  , applyEntityRowUpdates
  , applyArchetypeRunUpdates
  , stepWorld
  , worldHasSteps
  , driveStep
  , foldEntities
  , foldEntitiesFrom
  , mapEntities
  , setEntityRows
  , setEntityRowsV
  , setEntityRowsVSameShape
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
  , sigFromBag
  , runQuerySig
  , keyOf
  , keyOfType
  , BagEdit
  , bagEditSet
  , bagEditUpdate
  , bagEditDel
  , bagApplyEdit
  , bagApplyEditUnsafe
  , bagApplyEditPacked
  , bagApplyEditPackedUnsafe
  , bagSetDirect
  , bagUpdateDirect
  , bagDelDirect
  , bagGet
  , bagSet
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
import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
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
import qualified Data.Vector.Mutable as MV
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

data Bag c = Bag
  {-# UNPACK #-} !Sig
  !(V.Vector Any)

bagMask :: Bag c -> Sig
{-# INLINE bagMask #-}
bagMask (Bag mask _) = mask

data AnyStepState where
  AnyStepState :: !(F.Step () a) -> AnyStepState

newtype StepStore = StepStore (IntMap (IntMap AnyStepState))

type Sig = Word64


data EntityRow c = EntityRow
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Sig
  !(Bag c)

data BagOp
  = BagOpSet !Int !Any
  | BagOpUpdate !Int (Any -> Any)
  | BagOpDel !Int

newtype BagEdit c = BagEdit
  { bagEditOps :: [BagOp]
  }

instance Semigroup (BagEdit c) where
  BagEdit xs <> BagEdit ys = BagEdit (ys <> xs)

instance Monoid (BagEdit c) where
  mempty = BagEdit []

stepStoreEmpty :: StepStore
stepStoreEmpty = StepStore IntMap.empty

stepStoreHasAny :: StepStore -> Bool
stepStoreHasAny (StepStore m) = not (IntMap.null m)

stepStoreInsert :: Int -> Int -> F.Step () a -> StepStore -> StepStore
stepStoreInsert bitIx eid' st (StepStore m) =
  let entMap = IntMap.singleton eid' (AnyStepState st)
  in StepStore (IntMap.insertWith IntMap.union bitIx entMap m)

stepStoreDeleteEntity :: Int -> StepStore -> StepStore
stepStoreDeleteEntity eid' (StepStore m) =
  let dropOne entMap =
        let entMap' = IntMap.delete eid' entMap
        in if IntMap.null entMap' then Nothing else Just entMap'
  in StepStore (IntMap.mapMaybe dropOne m)

emptyBag :: forall c. ComponentId c => Bag c
emptyBag =
  let size = componentCount @c
      limit = finiteBitSize (0 :: Word64)
  in if size > limit
      then error ("ComponentId: componentCount exceeds Sig bit width (" <> show limit <> ")")
      else Bag 0 V.empty

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
  Bag mask1 vals1 <> Bag mask2 vals2 =
    let (mask', vals') = mergeStatic mask1 vals1 mask2 vals2
    in Bag mask' vals'

instance ComponentId c => Monoid (Bag c) where
  mempty = emptyBag

data Loc = Loc !Int

type LocTable = V.Vector (Maybe Loc)

data World c = World
  { nextIdW :: !Int
  , entityCountW :: !Int
  , rowsW :: !(V.Vector (EntityRow c))
  , entityLocW :: !LocTable
  , stepStoreW :: !StepStore
  , resourcesW :: !(IntMap Any)
  , relOutW :: !(IntMap (IntMap IntSet))
  , relInW :: !(IntMap (IntMap IntSet))
  }

emptyWorld :: World c
emptyWorld =
  World
    { nextIdW = 0
    , entityCountW = 0
    , rowsW = V.empty
    , entityLocW = locInit 0
    , stepStoreW = stepStoreEmpty
    , resourcesW = IntMap.empty
    , relOutW = IntMap.empty
    , relInW = IntMap.empty
    }

locInit :: Int -> LocTable
locInit n
  | n <= 0 = V.empty
  | otherwise = V.replicate n Nothing

locLookupWorld :: Int -> World c -> Maybe Loc
locLookupWorld eid' w
  | eid' < 0 || eid' >= nextIdW w = Nothing
  | otherwise = entityLocW w V.! eid'

vecUpdate :: Int -> a -> V.Vector a -> V.Vector a
vecUpdate i x vec = V.modify (\mv -> MV.unsafeWrite mv i x) vec
{-# INLINE vecUpdate #-}

locInsertKnown :: Int -> Loc -> LocTable -> LocTable
locInsertKnown i loc locs = vecUpdate i (Just loc) locs

locDeleteKnown :: Int -> LocTable -> LocTable
locDeleteKnown i locs = vecUpdate i Nothing locs

locAppend :: Loc -> LocTable -> LocTable
locAppend loc locs = V.snoc locs (Just loc)

locSetGrow :: Int -> Loc -> LocTable -> Int -> (LocTable, Int)
locSetGrow eid' loc locs locLen
  | eid' < 0 = (locs, locLen)
  | eid' < locLen = (locInsertKnown eid' loc locs, locLen)
  | eid' == locLen = (locAppend loc locs, locLen + 1)
  | otherwise =
      let fillCount = eid' - locLen
          tailCells =
            if fillCount == 0
              then [Just loc]
              else replicate fillCount Nothing <> [Just loc]
          locs' = locs V.++ V.fromList tailCells
      in (locs', eid' + 1)

entities :: World c -> [Entity]
entities w =
  Foldable.foldr
    (\(EntityRow eid' _ _) acc -> Entity eid' : acc)
    []
    (rowsW w)

entityRows :: World c -> [EntityRow c]
entityRows = Foldable.toList . rowsW

entityRowsV :: World c -> V.Vector (EntityRow c)
entityRowsV = rowsW

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
  Foldable.foldl'
    (\acc' (EntityRow eid' sig bag) -> f (Entity eid') sig bag acc')
    s0
    (rowsW w)

mapEntities :: (Entity -> Sig -> Bag c -> Bag c) -> World c -> World c
mapEntities f w =
  let rows' =
        V.map
          (\(EntityRow eid' sig bag) ->
            let e = Entity eid'
                bag' = f e sig bag
                staticOld = bagMask bag
                stepBits = sig .&. complement staticOld
                sig' = bagMask bag' .|. stepBits
            in EntityRow eid' sig' bag'
          )
          (rowsW w)
  in setEntityRowsVSameShape rows' w

setEntityRows :: [EntityRow c] -> World c -> World c
setEntityRows rows = setEntityRowsV (V.fromList rows)

setEntityRowsV :: V.Vector (EntityRow c) -> World c -> World c
setEntityRowsV rows w =
  let (locs', locLen') = buildLocsV (nextIdW w) rows
  in w
      { rowsW = rows
      , entityLocW = locs'
      , nextIdW = max (nextIdW w) locLen'
      , entityCountW = Foldable.length rows
      }

-- | Fast replacement for row updates that preserve entity ids and row count.
-- The location table remains valid and is intentionally reused.
setEntityRowsVSameShape :: V.Vector (EntityRow c) -> World c -> World c
setEntityRowsVSameShape rows w =
  w
    { rowsW = rows
    , entityCountW = entityCountW w
    }

buildLocsV :: Int -> V.Vector (EntityRow c) -> (LocTable, Int)
buildLocsV locLen0 rows =
  V.ifoldl'
    (\(locs, locLen) i (EntityRow eid' _ _) ->
      locSetGrow eid' (Loc i) locs locLen
    )
    (locInit locLen0, locLen0)
    rows

applyEntityRowUpdates :: [(Sig, Int, EntityRow c)] -> World c -> World c
applyEntityRowUpdates updates w =
  case updates of
    [] -> w
    _ ->
      let rows0 = rowsW w
          stepOne rowsAcc (_, _, row@(EntityRow eid' _ _)) =
            case locLookupWorld eid' w of
              Nothing -> rowsAcc
              Just (Loc idx) -> vecUpdate idx row rowsAcc
          rows1 = foldl' stepOne rows0 updates
      in setEntityRowsVSameShape rows1 w

applyArchetypeRunUpdates :: [(Sig, V.Vector (EntityRow c))] -> World c -> World c
applyArchetypeRunUpdates runs w =
  case runs of
    [] -> w
    _ ->
      let byEid =
            foldl'
              (\acc (_, rows) ->
                Foldable.foldl'
                  (\acc' row@(EntityRow eid' _ _) -> IntMap.insert eid' row acc')
                  acc
                  rows
              )
              IntMap.empty
              runs
          rows1 =
            V.map
              (\row@(EntityRow eid' _ _) -> IntMap.findWithDefault row eid' byEid)
              (rowsW w)
      in setEntityRowsVSameShape rows1 w

matchingRows :: Sig -> Sig -> World c -> V.Vector (EntityRow c)
matchingRows req forb w =
  V.filter
    (\(EntityRow _ sig _) -> (sig .&. req) == req && (sig .&. forb) == 0)
    (rowsW w)

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
      rows0 = rowsW w
      idx = Foldable.length rows0
      rows' = V.snoc rows0 (EntityRow eid' sig bag)
      locs' = locAppend (Loc idx) (entityLocW w)
      w' =
        w
          { nextIdW = nextIdW w + 1
          , entityCountW = entityCountW w + 1
          , rowsW = rows'
          , entityLocW = locs'
          }
  in (e, w')

kill :: Entity -> World c -> World c
kill e w =
  let eid' = eid e
      out' = dropRelEntity eid' (relOutW w)
      in' = dropRelEntity eid' (relInW w)
  in case locLookupWorld eid' w of
      Nothing ->
        w
          { relOutW = out'
          , relInW = in'
          , stepStoreW = stepStoreDeleteEntity eid' (stepStoreW w)
          }
      Just (Loc idx) ->
        let rows0 = rowsW w
            len = Foldable.length rows0
            lastIdx = len - 1
            rows1 =
              if idx == lastIdx
                then V.take lastIdx rows0
                else
                  let rowLast = rows0 V.! lastIdx
                      rows' = vecUpdate idx rowLast rows0
                  in V.take lastIdx rows'
            locs1 =
              if idx == lastIdx
                then locDeleteKnown eid' (entityLocW w)
                else
                  let EntityRow eidLast _ _ = rows0 V.! lastIdx
                      acc1 = locDeleteKnown eid' (entityLocW w)
                  in locInsertKnown eidLast (Loc idx) acc1
        in w
            { rowsW = rows1
            , entityLocW = locs1
            , entityCountW = entityCountW w - 1
            , relOutW = out'
            , relInW = in'
            , stepStoreW = stepStoreDeleteEntity eid' (stepStoreW w)
            }

dropRelEntity :: Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
dropRelEntity eid' =
  IntMap.mapMaybe (\m ->
    let m' = IntMap.map (IntSet.delete eid') (IntMap.delete eid' m)
        m'' = IntMap.filter (not . IntSet.null) m'
    in if IntMap.null m'' then Nothing else Just m'')

sigFromBag :: Bag c -> Sig
sigFromBag = bagMask

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
bagGetBy bitIx (Bag mask vals) =
  staticLookup mask vals bitIx

bagGetByTyped :: forall a c. Int -> Bag c -> Maybe a
bagGetByTyped bitIx bag = unsafeCoerce <$> bagGetBy bitIx bag

bagGet :: forall c a. (Component c a, ComponentBit c a) => Bag c -> Maybe a
bagGet = bagGetByTyped (componentBitOf @c @a)

ptrEq :: Any -> Any -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

bagSetByAny :: Int -> Any -> Bag c -> Bag c
{-# INLINE bagSetByAny #-}
bagSetByAny bitIx vAny (Bag mask vals) =
  let (mask', vals') = staticSetAny mask vals bitIx vAny
  in Bag mask' vals'


bagUpdateByAny :: Int -> (Any -> Any) -> Bag c -> Bag c
{-# INLINE bagUpdateByAny #-}
bagUpdateByAny bitIx fAny (Bag mask vals) =
  case staticLookup mask vals bitIx of
    Nothing -> Bag mask vals
    Just vAny ->
      let vAny' = fAny vAny
      in if ptrEq vAny vAny'
          then Bag mask vals
          else
            let (mask', vals') = staticSetAny mask vals bitIx vAny'
            in Bag mask' vals'

bagDelByBit :: Int -> Bag c -> Bag c
{-# INLINE bagDelByBit #-}
bagDelByBit bitIx (Bag mask vals) =
  let (mask', vals') = staticDelAny mask vals bitIx
  in Bag mask' vals'

bagEditSet :: forall c a. (Component c a, ComponentBit c a) => a -> BagEdit c
bagEditSet a =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in BagEdit [BagOpSet bitIx vAny]

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
        BagOpUpdate bitIx fAny -> bagUpdateByAny bitIx fAny bag
        BagOpDel bitIx -> bagDelByBit bitIx bag
    )
    bag0
    (bagEditOps edit)

bagApplyEditUnsafe :: BagEdit c -> Bag c -> Bag c
bagApplyEditUnsafe = bagApplyEdit

bagApplyEditPacked :: BagEdit c -> Bag c -> Bag c
bagApplyEditPacked edit@(BagEdit ops) bag0@(Bag mask0 vals0) =
  case ops of
    [] -> bag0
    [_] -> bagApplyEdit edit bag0
    [_, _] -> bagApplyEdit edit bag0
    _ ->
      runST $ do
        let maxBits = finiteBitSize (0 :: Sig)
        mv <- MV.replicate maxBits Nothing
        let fillExisting !mask !i
              | mask == 0 = pure ()
              | otherwise = do
                  let bitIx = countTrailingZeros mask
                      mask' = mask .&. (mask - 1)
                      vAny = V.unsafeIndex vals0 i
                  MV.unsafeWrite mv bitIx (Just vAny)
                  fillExisting mask' (i + 1)
        fillExisting mask0 0
        forM_ ops $ \op ->
          case op of
            BagOpSet bitIx vAny -> MV.unsafeWrite mv bitIx (Just vAny)
            BagOpUpdate bitIx fAny -> do
              cur <- MV.unsafeRead mv bitIx
              case cur of
                Nothing -> pure ()
                Just vAny -> MV.unsafeWrite mv bitIx (Just (fAny vAny))
            BagOpDel bitIx -> MV.unsafeWrite mv bitIx Nothing
        let buildMask !i !mask !count
              | i >= maxBits = pure (mask, count)
              | otherwise = do
                  cur <- MV.unsafeRead mv i
                  case cur of
                    Nothing -> buildMask (i + 1) mask count
                    Just _ -> buildMask (i + 1) (mask .|. bit i) (count + 1)
        (mask', count) <- buildMask 0 0 0
        mvals <- MV.new count
        let fillVals !i !j
              | i >= maxBits = pure ()
              | otherwise = do
                  cur <- MV.unsafeRead mv i
                  case cur of
                    Nothing -> fillVals (i + 1) j
                    Just vAny -> do
                      MV.unsafeWrite mvals j vAny
                      fillVals (i + 1) (j + 1)
        fillVals 0 0
        vals' <- V.unsafeFreeze mvals
        pure (Bag mask' vals')

bagApplyEditPackedUnsafe :: BagEdit c -> Bag c -> Bag c
bagApplyEditPackedUnsafe = bagApplyEditPacked

bagSetDirect :: forall c a. (Component c a, ComponentBit c a) => a -> Bag c -> Bag c
{-# INLINE bagSetDirect #-}
bagSetDirect a bag =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in bagSetByAny bitIx vAny bag

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

driveStep :: forall a c. (Component c a, ComponentBit c a) => Entity -> F.Step () a -> World c -> World c
driveStep e s0 w =
  let eid' = eid e
  in case locLookupWorld eid' w of
      Nothing -> w
      Just (Loc idx) ->
        let (v, s1) = F.stepS s0 0 ()
            bitIx = componentBitOf @c @a
            rows0 = rowsW w
            EntityRow _ sig bag = rows0 V.! idx
            sig' = sig .|. bit bitIx
            bag' = bagSetDirect @c @a v bag
            row' = EntityRow eid' sig' bag'
            rows' = vecUpdate idx row' rows0
            store' = stepStoreInsert bitIx eid' s1 (stepStoreW w)
        in w { rowsW = rows', stepStoreW = store' }

updateEntityBag :: (Bag c -> Bag c) -> Entity -> World c -> World c
{-# INLINE updateEntityBag #-}
updateEntityBag updateBag e w =
  let eid' = eid e
  in case locLookupWorld eid' w of
      Nothing -> w
      Just (Loc idx) ->
        let rows = rowsW w
            EntityRow _ sig0 bag = rows V.! idx
            bag' = updateBag bag
            staticOld = bagMask bag
            stepBits = sig0 .&. complement staticOld
            sig' = bagMask bag' .|. stepBits
            row' = EntityRow eid' sig' bag'
            rows' = vecUpdate idx row' rows
        in w { rowsW = rows' }

has :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> Bool
has e w = isJust (get @a e w)

lookupRow :: Int -> World c -> Maybe (Sig, Bag c)
lookupRow eid' w =
  case locLookupWorld eid' w of
    Nothing -> Nothing
    Just (Loc idx) ->
      let rows = rowsW w
          EntityRow _ sig' bag = rows V.! idx
      in Just (sig', bag)

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
      stepRun (EntityRow eid' sig bag) acc =
        if (sig .&. req) == req && (sig .&. forb) == 0
          then stepRow (EntityRow eid' sig bag) acc
          else acc
  in Foldable.foldr stepRun [] (rowsW w)

foldq :: Query c a -> (Entity -> a -> s -> s) -> s -> World c -> s
foldq q step s0 w =
  let info = queryInfoQ q
      req = requireQ info
      forb = forbidQ info
      stepRow acc (EntityRow eid' _ bag) =
        case runQuery q (Entity eid') bag of
          Nothing -> acc
          Just a -> step (Entity eid') a acc
      stepRun acc (EntityRow eid' sig bag) =
        if (sig .&. req) == req && (sig .&. forb) == 0
          then stepRow acc (EntityRow eid' sig bag)
          else acc
  in Foldable.foldl' stepRun s0 (rowsW w)

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

class Queryable c a where
  queryC :: Query c a

class QueryableAuto (isComp :: Bool) c a where
  queryAuto :: Query c a

instance (Component c a, ComponentBit c a) => QueryableAuto 'True c a where
  queryAuto = comp

instance (Generic a, GQueryable c (Rep a)) => QueryableAuto 'False c a where
  queryAuto = to <$> gquery

instance (Generic c, QueryableAuto (HasType (Rep c) a) c a) => Queryable c a where
  queryC = queryAuto @(HasType (Rep c) a) @c @a

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
stepWorld :: F.DTime -> World c -> World c
stepWorld d w =
  let StepStore store0 = stepStoreW w
  in
    if IntMap.null store0
      then w
      else runST $ do
        rowsM <- V.thaw (rowsW w)
        store' <- stepStoreStepAll d (nextIdW w) (entityLocW w) rowsM store0
        rows' <- V.unsafeFreeze rowsM
        pure w { rowsW = rows', stepStoreW = StepStore store' }
  where
    stepStoreStepAll d' nextIdLimit locs rowsM store0 =
      foldM (stepComponent d' nextIdLimit locs rowsM) IntMap.empty (IntMap.toList store0)

    stepComponent d' nextIdLimit locs rowsM acc (bitIx, entMap) = do
      entMap' <- foldM (stepEntity d' nextIdLimit locs rowsM bitIx) IntMap.empty (IntMap.toList entMap)
      pure $
        if IntMap.null entMap'
          then acc
          else IntMap.insert bitIx entMap' acc

    stepEntity d' nextIdLimit locs rowsM bitIx acc (eid', AnyStepState st) =
      if eid' < 0 || eid' >= nextIdLimit
        then pure acc
        else
          case locs V.! eid' of
            Nothing -> pure acc
            Just (Loc idx) -> do
              EntityRow _ sig bag <- MV.unsafeRead rowsM idx
              if (sig .&. bit bitIx) == 0
                then pure acc
                else do
                  let (v, st') = F.stepS st d' ()
                      bag' = bagSetByAny bitIx (unsafeCoerce v) bag
                  MV.unsafeWrite rowsM idx (EntityRow eid' sig bag')
                  pure (IntMap.insert eid' (AnyStepState st') acc)

worldHasSteps :: World c -> Bool
worldHasSteps w = stepStoreHasAny (stepStoreW w)
