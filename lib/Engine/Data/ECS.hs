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
  , EntityRow
  , emptyWorld
  , entities
  , entityRows
  , stepWorld
  , foldEntities
  , foldEntitiesFrom
  , mapEntities
  , setEntityRows
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
import Data.Bits (bit, complement, finiteBitSize, (.&.), (.|.), xor)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, cast, typeRep, typeRepFingerprint)
import qualified Data.Vector as V
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
  { bagStatic :: !(V.Vector (Maybe Any))
  , bagSteps :: !Steps
  }

type Sig = Word64

type EntityRow c = (Int, Sig, Bag c)

data BagOp
  = BagOpSet !Int !Any
  | BagOpSetStep !Int !StepSlot
  | BagOpUpdate !Int (Any -> Any)
  | BagOpDel !Int

type StaticEdits = [(Int, Maybe Any)]

newtype BagEdit c = BagEdit
  { runBagEdit :: forall r. (BagOp -> r -> r) -> r -> r
  }

instance Semigroup (BagEdit c) where
  BagEdit f <> BagEdit g = BagEdit (\k z -> f k (g k z))

instance Monoid (BagEdit c) where
  mempty = BagEdit (\_ z -> z)

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
      _ =
        if size > limit
          then error ("ComponentId: componentCount exceeds Sig bit width (" <> show limit <> ")")
          else ()
      vec =
        if size <= 0
          then V.empty
          else V.replicate size Nothing
  in Bag vec stepsEmpty

stepsMerge :: Steps -> Steps -> Steps
stepsMerge (Steps base) (Steps extra) =
  Steps (IntMap.union extra base)

mergeStatic :: V.Vector (Maybe Any) -> V.Vector (Maybe Any) -> V.Vector (Maybe Any)
mergeStatic s1 s2 =
  let len = V.length s1
  in if len == 0
      then s1
      else V.generate len $ \i ->
            case V.unsafeIndex s2 i of
              Just v -> Just v
              Nothing -> V.unsafeIndex s1 i

instance ComponentId c => Semigroup (Bag c) where
  Bag s1 st1 <> Bag s2 st2 =
    Bag (mergeStatic s1 s2) (stepsMerge st1 st2)

instance ComponentId c => Monoid (Bag c) where
  mempty = emptyBag

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
entities = map (Entity . (\(eid', _, _) -> eid')) . entitiesW

entityRows :: World c -> [EntityRow c]
entityRows = entitiesW

nextId :: World c -> Int
nextId = nextIdW

foldEntitiesFrom :: [EntityRow c] -> (Entity -> Sig -> Bag c -> s -> s) -> s -> World c -> s
foldEntitiesFrom rows f s0 _ =
  foldl'
    (\acc (eid', sig, bag) -> f (Entity eid') sig bag acc)
    s0
    rows

foldEntities :: (Entity -> Sig -> Bag c -> s -> s) -> s -> World c -> s
foldEntities f s0 w = foldEntitiesFrom (entitiesW w) f s0 w

mapEntities :: (Entity -> Sig -> Bag c -> Bag c) -> World c -> World c
mapEntities f w =
  let ents' =
        map
          (\(eid', sig, bag) ->
            let bag' = f (Entity eid') sig bag
                sig' = sigFromBag bag'
            in (eid', sig', bag')
          )
          (entitiesW w)
  in w { entitiesW = ents' }

setEntityRows :: [EntityRow c] -> World c -> World c
setEntityRows rows w = w { entitiesW = rows }

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
  gComponentBit n = gComponentBit @f @a n

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
      ents' = (eid', sig, bag) : entitiesW w
      w' =
        w
          { nextIdW = nextIdW w + 1
          , entitiesW = ents'
          }
  in (e, w')

kill :: Entity -> World c -> World c
kill e w =
  let eid' = eid e
      ents' = filter (\(eid0, _, _) -> eid0 /= eid') (entitiesW w)
      out' = dropRelEntity eid' (relOutW w)
      in' = dropRelEntity eid' (relInW w)
  in w
      { entitiesW = ents'
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
  let static = bagStatic bag
      steps = bagSteps bag
      sigStatic =
        V.ifoldl' (\acc i v -> case v of
                                Just _ -> acc .|. bit i
                                Nothing -> acc
                  ) 0 static
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
bagGetBy bitIx (Bag static steps) =
  if stepsNull steps
    then V.unsafeIndex static bitIx
    else
      case stepsLookup bitIx steps of
        Just slot -> Just (stepVal slot)
        Nothing -> V.unsafeIndex static bitIx

bagGetByTyped :: forall a c. Int -> Bag c -> Maybe a
bagGetByTyped bitIx bag = unsafeCoerce <$> bagGetBy bitIx bag

bagGetByUnsafe :: forall a c. Int -> Bag c -> a
bagGetByUnsafe bitIx bag =
  case bagGetBy bitIx bag of
    Just v -> unsafeCoerce v
    Nothing -> error "bagGetByUnsafe: missing component (signature mismatch?)"

bagGet :: forall c a. (Component c a, ComponentBit c a) => Bag c -> Maybe a
bagGet bag = bagGetByTyped (componentBitOf @c @a) bag

ptrEq :: Any -> Any -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

eqMaybe :: Maybe Any -> Maybe Any -> Bool
eqMaybe (Just a) (Just b) = ptrEq a b
eqMaybe Nothing Nothing = True
eqMaybe _ _ = False

updateStatic :: V.Vector (Maybe Any) -> Int -> Maybe Any -> V.Vector (Maybe Any)
updateStatic static bitIx newVal =
  let cur = V.unsafeIndex static bitIx
  in if eqMaybe cur newVal
      then static
      else static V.// [(bitIx, newVal)]

lookupStatic :: V.Vector (Maybe Any) -> StaticEdits -> Int -> Maybe Any
lookupStatic static edits bitIx =
  case lookupEdit bitIx edits of
    Just v -> v
    Nothing -> V.unsafeIndex static bitIx

lookupEdit :: Int -> StaticEdits -> Maybe (Maybe Any)
lookupEdit _ [] = Nothing
lookupEdit bitIx ((i, v) : rest)
  | i == bitIx = Just v
  | otherwise = lookupEdit bitIx rest

upsertEdit :: Int -> Maybe Any -> StaticEdits -> StaticEdits
upsertEdit bitIx newVal [] = [(bitIx, newVal)]
upsertEdit bitIx newVal ((i, v) : rest)
  | i == bitIx = (bitIx, newVal) : rest
  | otherwise = (i, v) : upsertEdit bitIx newVal rest

setStaticIfChanged :: V.Vector (Maybe Any) -> Int -> Maybe Any -> StaticEdits -> StaticEdits
setStaticIfChanged static bitIx newVal edits =
  let cur =
        case lookupEdit bitIx edits of
          Just v -> v
          Nothing -> V.unsafeIndex static bitIx
  in if eqMaybe cur newVal
      then edits
      else upsertEdit bitIx newVal edits

bagEditSet :: forall c a. (Component c a, ComponentBit c a) => a -> BagEdit c
bagEditSet a =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in BagEdit (\k z -> k (BagOpSet bitIx vAny) z)

bagEditSetStep :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> BagEdit c
bagEditSetStep s0 =
  let stepAny = unsafeCoerce s0 :: F.Step () Any
      (vAny, s1) = F.stepS stepAny 0 ()
      bitIx = componentBitOf @c @a
      slot = StepSlot vAny (unsafeCoerce s1)
  in BagEdit (\k z -> k (BagOpSetStep bitIx slot) z)

bagEditUpdate :: forall c a. (Component c a, ComponentBit c a) => (a -> a) -> BagEdit c
bagEditUpdate f =
  let bitIx = componentBitOf @c @a
      fAny v = unsafeCoerce (f (unsafeCoerce v))
  in BagEdit (\k z -> k (BagOpUpdate bitIx fAny) z)

bagEditDel :: forall c a. (Component c a, ComponentBit c a) => BagEdit c
bagEditDel =
  let bitIx = componentBitOf @c @a
  in BagEdit (\k z -> k (BagOpDel bitIx) z)

bagApplyEdit :: BagEdit c -> Bag c -> Bag c
bagApplyEdit edit (Bag static steps) =
  let step op (stepsAcc, edits) =
        case op of
          BagOpSet bitIx vAny ->
            case stepsLookup bitIx stepsAcc of
              Just (StepSlot vOld sAny) ->
                if ptrEq vOld vAny
                  then (stepsAcc, edits)
                  else
                    let steps' = stepsInsert bitIx (StepSlot vAny sAny) stepsAcc
                        edits' = setStaticIfChanged static bitIx Nothing edits
                    in (steps', edits')
              Nothing ->
                let edits' = setStaticIfChanged static bitIx (Just vAny) edits
                in (stepsAcc, edits')
          BagOpSetStep bitIx slot ->
            let steps' = stepsInsert bitIx slot stepsAcc
                edits' = setStaticIfChanged static bitIx Nothing edits
            in (steps', edits')
          BagOpUpdate bitIx fAny ->
            case stepsLookup bitIx stepsAcc of
              Just (StepSlot vAny sAny) ->
                let vAny' = fAny vAny
                in if ptrEq vAny vAny'
                    then (stepsAcc, edits)
                    else (stepsInsert bitIx (StepSlot vAny' sAny) stepsAcc, edits)
              Nothing ->
                case lookupStatic static edits bitIx of
                  Nothing -> (stepsAcc, edits)
                  Just vAny ->
                    let vAny' = fAny vAny
                    in if ptrEq vAny vAny'
                        then (stepsAcc, edits)
                        else (stepsAcc, setStaticIfChanged static bitIx (Just vAny') edits)
          BagOpDel bitIx ->
            let steps' = stepsDelete bitIx stepsAcc
                edits' = setStaticIfChanged static bitIx Nothing edits
            in (steps', edits')
      (steps', staticEdits) = runBagEdit edit step (steps, [])
      static' =
        if null staticEdits
          then static
          else static V.// staticEdits
  in Bag static' steps'

bagApplyEditUnsafe :: BagEdit c -> Bag c -> Bag c
bagApplyEditUnsafe = bagApplyEdit

bagSetDirect :: forall c a. (Component c a, ComponentBit c a) => a -> Bag c -> Bag c
{-# INLINE bagSetDirect #-}
bagSetDirect a (Bag static steps) =
  let bitIx = componentBitOf @c @a
      vAny = unsafeCoerce a
  in case stepsLookup bitIx steps of
      Just (StepSlot vOld sAny) ->
        let steps' =
              if ptrEq vOld vAny
                then steps
                else stepsInsert bitIx (StepSlot vAny sAny) steps
            static' = updateStatic static bitIx Nothing
        in Bag static' steps'
      Nothing ->
        let static' = updateStatic static bitIx (Just vAny)
        in Bag static' steps

applySetStep :: Steps -> Int -> Any -> (Steps, Maybe Any)
{-# INLINE applySetStep #-}
applySetStep steps bitIx vAny =
  case stepsLookup bitIx steps of
    Just (StepSlot vOld sAny) ->
      let steps' =
            if ptrEq vOld vAny
              then steps
              else stepsInsert bitIx (StepSlot vAny sAny) steps
      in (steps', Nothing)
    Nothing ->
      (steps, Just vAny)

updateStatic2 :: V.Vector (Maybe Any) -> Int -> Maybe Any -> Int -> Maybe Any -> V.Vector (Maybe Any)
{-# INLINE updateStatic2 #-}
updateStatic2 static i vI j vJ =
  let curI = V.unsafeIndex static i
      curJ = V.unsafeIndex static j
      edits =
        case eqMaybe curI vI of
          True ->
            if eqMaybe curJ vJ
              then []
              else [(j, vJ)]
          False ->
            if eqMaybe curJ vJ
              then [(i, vI)]
              else [(i, vI), (j, vJ)]
  in if null edits
      then static
      else static V.// edits

addEdit3 :: (Int, Maybe Any) -> [(Int, Maybe Any)] -> [(Int, Maybe Any)]
addEdit3 edit edits =
  case edits of
    [] -> [edit]
    [a] -> [a, edit]
    [a, b] -> [a, b, edit]
    _ -> edits ++ [edit]

updateStatic3 :: V.Vector (Maybe Any) -> Int -> Maybe Any -> Int -> Maybe Any -> Int -> Maybe Any
              -> V.Vector (Maybe Any)
{-# INLINE updateStatic3 #-}
updateStatic3 static i vI j vJ k vK =
  let curI = V.unsafeIndex static i
      curJ = V.unsafeIndex static j
      curK = V.unsafeIndex static k
      edits2 =
        case eqMaybe curI vI of
          True ->
            if eqMaybe curJ vJ
              then []
              else [(j, vJ)]
          False ->
            if eqMaybe curJ vJ
              then [(i, vI)]
              else [(i, vI), (j, vJ)]
      edits =
        if eqMaybe curK vK
          then edits2
          else addEdit3 (k, vK) edits2
  in if null edits
      then static
      else static V.// edits

bagSet2Direct :: forall c a b.
  (Component c a, ComponentBit c a, Component c b, ComponentBit c b)
  => a -> b -> Bag c -> Bag c
{-# INLINE bagSet2Direct #-}
bagSet2Direct a b (Bag static steps0) =
  let bitA = componentBitOf @c @a
      bitB = componentBitOf @c @b
      vA = unsafeCoerce a
      vB = unsafeCoerce b
      (steps1, staticA) = applySetStep steps0 bitA vA
      (steps2, staticB) = applySetStep steps1 bitB vB
      static' = updateStatic2 static bitA staticA bitB staticB
  in Bag static' steps2

bagSet3Direct :: forall c a b d.
  (Component c a, ComponentBit c a
  , Component c b, ComponentBit c b
  , Component c d, ComponentBit c d
  ) => a -> b -> d -> Bag c -> Bag c
{-# INLINE bagSet3Direct #-}
bagSet3Direct a b d (Bag static steps0) =
  let bitA = componentBitOf @c @a
      bitB = componentBitOf @c @b
      bitD = componentBitOf @c @d
      vA = unsafeCoerce a
      vB = unsafeCoerce b
      vD = unsafeCoerce d
      (steps1, staticA) = applySetStep steps0 bitA vA
      (steps2, staticB) = applySetStep steps1 bitB vB
      (steps3, staticD) = applySetStep steps2 bitD vD
      static' = updateStatic3 static bitA staticA bitB staticB bitD staticD
  in Bag static' steps3

bagSetStepDirect :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> Bag c -> Bag c
bagSetStepDirect s0 (Bag static steps) =
  let stepAny = unsafeCoerce s0 :: F.Step () Any
      (vAny, s1) = F.stepS stepAny 0 ()
      bitIx = componentBitOf @c @a
      slot = StepSlot vAny (unsafeCoerce s1)
      steps' = stepsInsert bitIx slot steps
      static' = updateStatic static bitIx Nothing
  in Bag static' steps'

bagUpdateDirect :: forall c a. (Component c a, ComponentBit c a) => (a -> a) -> Bag c -> Bag c
{-# INLINE bagUpdateDirect #-}
bagUpdateDirect f (Bag static steps) =
  let bitIx = componentBitOf @c @a
      fAny v = unsafeCoerce (f (unsafeCoerce v))
  in case stepsLookup bitIx steps of
      Just (StepSlot vAny sAny) ->
        let vAny' = fAny vAny
        in if ptrEq vAny vAny'
            then Bag static steps
            else Bag static (stepsInsert bitIx (StepSlot vAny' sAny) steps)
      Nothing ->
        case V.unsafeIndex static bitIx of
          Nothing -> Bag static steps
          Just vAny ->
            let vAny' = fAny vAny
            in if ptrEq vAny vAny'
                then Bag static steps
                else Bag (updateStatic static bitIx (Just vAny')) steps

bagDelDirect :: forall c a. (Component c a, ComponentBit c a) => Bag c -> Bag c
{-# INLINE bagDelDirect #-}
bagDelDirect (Bag static steps) =
  let bitIx = componentBitOf @c @a
      steps' = stepsDelete bitIx steps
      static' = updateStatic static bitIx Nothing
  in Bag static' steps'

bagSet :: forall a c. (Component c a, ComponentBit c a) => a -> Bag c -> Bag c
bagSet = bagSetDirect

bagSetStep :: forall c a. (Component c a, ComponentBit c a) => F.Step () a -> Bag c -> Bag c
bagSetStep = bagSetStepDirect

bagDel :: forall a c. (Component c a, ComponentBit c a) => Bag c -> Bag c
bagDel bag = bagDelDirect @c @a bag

get :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> Maybe a
get e w =
  case lookupRow (eid e) (entitiesW w) of
    Nothing -> Nothing
    Just (_, bag) -> bagGet bag

set :: forall a c. (Component c a, ComponentBit c a) => Entity -> a -> World c -> World c
set e a w =
  let eid' = eid e
      bitC = bit (componentBitOf @c @a)
  in case lookupRow eid' (entitiesW w) of
      Nothing -> w
      Just (sig, bag) ->
        let bag' = bagSet a bag
            sig' = sig .|. bitC
            ents' = replaceRow eid' sig' bag' (entitiesW w)
        in w { entitiesW = ents' }

del :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> World c
del e w =
  let eid' = eid e
      bitC = bit (componentBitOf @c @a)
  in case lookupRow eid' (entitiesW w) of
      Nothing -> w
      Just (sig, bag) ->
        let bag' = bagDel @a bag
            sig' = sig .&. complement bitC
            ents' = replaceRow eid' sig' bag' (entitiesW w)
        in w { entitiesW = ents' }

has :: forall a c. (Component c a, ComponentBit c a) => Entity -> World c -> Bool
has e w = isJust (get @a e w)

lookupRow :: Int -> [EntityRow c] -> Maybe (Sig, Bag c)
lookupRow _ [] = Nothing
lookupRow eid' ((eid0, sig, bag) : rest)
  | eid' == eid0 = Just (sig, bag)
  | eid' > eid0 = Nothing
  | otherwise = lookupRow eid' rest

replaceRow :: Int -> Sig -> Bag c -> [EntityRow c] -> [EntityRow c]
replaceRow _ _ _ [] = []
replaceRow eid' sig' bag' (row@(eid0, sig0, bag0) : rest)
  | eid' == eid0 = (eid', sig', bag') : rest
  | eid' > eid0 = row : rest
  | otherwise = row : replaceRow eid' sig' bag' rest

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
  let step (eid', sig, bag) acc =
        if matchSig (queryInfoQ q) sig
          then case runQuery q (Entity eid') bag of
            Nothing -> acc
            Just a -> (Entity eid', a) : acc
          else acc
  in foldr step [] (entitiesW w)

foldq :: Query c a -> (Entity -> a -> s -> s) -> s -> World c -> s
foldq q step s0 w =
  foldl'
    (\acc (eid', sig, bag) ->
      if matchSig (queryInfoQ q) sig
        then case runQuery q (Entity eid') bag of
          Nothing -> acc
          Just a -> step (Entity eid') a acc
        else acc
    )
    s0
    (entitiesW w)

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
    in (bit bitIx, 0, \_ -> Has)

instance {-# OVERLAPPING #-} (Component c a, ComponentBit c a) => PlanField c (Not a) where
  fieldPlan =
    let bitIx = componentBitOf @c @a
    in (0, bit bitIx, \_ -> Not)

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

instance PlanField c a => Plannable c a where
  planC =
    let (req, forb, runF) = fieldPlan @c @a
    in Plan req forb runF

query :: forall a c. Queryable c a => Query c a
query = queryC @c @a

querySum :: forall a c. QueryableSum c a => Query c a
querySum = querySumC @c @a

plan :: forall a c. Plannable c a => Plan c a
plan = planC @c @a

planRec :: forall a c. (Generic a, GPlan c (Rep a)) => Plan c a
planRec =
  let (req, forb, runF) = gplan @c @(Rep a)
  in Plan req forb (to . runF)

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
  gplan = (0, 0, \_ -> U1)

instance GPlan c f => GPlan c (M1 i m f) where
  gplan =
    let (req, forb, runF) = gplan @c @f
    in (req, forb, \bag -> M1 (runF bag))

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
  let stepRow (eid', sig, bag) (hasSteps, acc)
        | stepsNull (bagSteps bag) = (hasSteps, (eid', sig, bag) : acc)
        | otherwise =
            let bag' = stepBag d bag
            in (True, (eid', sig, bag') : acc)
      (hasSteps, ents') = foldr stepRow (False, []) (entitiesW w)
  in if not hasSteps
      then w
      else w { entitiesW = ents' }

stepBag :: F.DTime -> Bag c -> Bag c
stepBag d bag = bag { bagSteps = stepsStepAll d (bagSteps bag) }
