module Engine.Data.Transform
  ( ParentOf
  , Mat4(..)
  , identity
  , mul
  , inverse
  , translate
  , scale
  , rotateX
  , rotateY
  , rotateZ
  , compose
  , Transform(..)
  , Local(..)
  , Global(..)
  , identityLocal
  , attach
  , detach
  , parent
  , children
  , propagate
  ) where

import Prelude

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Engine.Data.ECS (Entity, World)
import qualified Engine.Data.ECS as E

data ParentOf

data Mat4 = Mat4
  { m00 :: Double, m01 :: Double, m02 :: Double, m03 :: Double
  , m10 :: Double, m11 :: Double, m12 :: Double, m13 :: Double
  , m20 :: Double, m21 :: Double, m22 :: Double, m23 :: Double
  , m30 :: Double, m31 :: Double, m32 :: Double, m33 :: Double
  } deriving (Eq, Show)

identity :: Mat4
identity = Mat4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

mul :: Mat4 -> Mat4 -> Mat4
mul a b = Mat4
  (rowCol a b 0 0) (rowCol a b 0 1) (rowCol a b 0 2) (rowCol a b 0 3)
  (rowCol a b 1 0) (rowCol a b 1 1) (rowCol a b 1 2) (rowCol a b 1 3)
  (rowCol a b 2 0) (rowCol a b 2 1) (rowCol a b 2 2) (rowCol a b 2 3)
  (rowCol a b 3 0) (rowCol a b 3 1) (rowCol a b 3 2) (rowCol a b 3 3)

rowCol :: Mat4 -> Mat4 -> Int -> Int -> Double
rowCol a b r c =
  get a r 0 * get b 0 c +
  get a r 1 * get b 1 c +
  get a r 2 * get b 2 c +
  get a r 3 * get b 3 c

get :: Mat4 -> Int -> Int -> Double
get m r c =
  case (r, c) of
    (0,0) -> m00 m; (0,1) -> m01 m; (0,2) -> m02 m; (0,3) -> m03 m
    (1,0) -> m10 m; (1,1) -> m11 m; (1,2) -> m12 m; (1,3) -> m13 m
    (2,0) -> m20 m; (2,1) -> m21 m; (2,2) -> m22 m; (2,3) -> m23 m
    (3,0) -> m30 m; (3,1) -> m31 m; (3,2) -> m32 m; (3,3) -> m33 m
    _ -> 0

det3 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
det3 a00 a01 a02 a10 a11 a12 a20 a21 a22 =
  a00 * (a11 * a22 - a12 * a21) -
  a01 * (a10 * a22 - a12 * a20) +
  a02 * (a10 * a21 - a11 * a20)

inverse :: Mat4 -> Maybe Mat4
inverse m =
  let a00 = m00 m; a01 = m01 m; a02 = m02 m
      a10 = m10 m; a11 = m11 m; a12 = m12 m
      a20 = m20 m; a21 = m21 m; a22 = m22 m
      tx = m03 m; ty = m13 m; tz = m23 m
      det = det3 a00 a01 a02 a10 a11 a12 a20 a21 a22
  in if det == 0
    then Nothing
    else
      let invDet = 1 / det
          i00 =  (a11 * a22 - a12 * a21) * invDet
          i01 = -(a01 * a22 - a02 * a21) * invDet
          i02 =  (a01 * a12 - a02 * a11) * invDet
          i10 = -(a10 * a22 - a12 * a20) * invDet
          i11 =  (a00 * a22 - a02 * a20) * invDet
          i12 = -(a00 * a12 - a02 * a10) * invDet
          i20 =  (a10 * a21 - a11 * a20) * invDet
          i21 = -(a00 * a21 - a01 * a20) * invDet
          i22 =  (a00 * a11 - a01 * a10) * invDet
          itx = -(i00 * tx + i01 * ty + i02 * tz)
          ity = -(i10 * tx + i11 * ty + i12 * tz)
          itz = -(i20 * tx + i21 * ty + i22 * tz)
      in Just (Mat4
        i00 i01 i02 itx
        i10 i11 i12 ity
        i20 i21 i22 itz
        0   0   0   1)

translate :: (Double, Double, Double) -> Mat4
translate (x, y, z) = Mat4
  1 0 0 x
  0 1 0 y
  0 0 1 z
  0 0 0 1

scale :: (Double, Double, Double) -> Mat4
scale (x, y, z) = Mat4
  x 0 0 0
  0 y 0 0
  0 0 z 0
  0 0 0 1

rotateX :: Double -> Mat4
rotateX a =
  let c = cos a
      s = sin a
  in Mat4
    1 0 0 0
    0 c (-s) 0
    0 s c 0
    0 0 0 1

rotateY :: Double -> Mat4
rotateY a =
  let c = cos a
      s = sin a
  in Mat4
    c 0 s 0
    0 1 0 0
    (-s) 0 c 0
    0 0 0 1

rotateZ :: Double -> Mat4
rotateZ a =
  let c = cos a
      s = sin a
  in Mat4
    c (-s) 0 0
    s c 0 0
    0 0 1 0
    0 0 0 1

compose :: [Mat4] -> Mat4
compose = foldl' mul identity

class Transform t where
  ident :: t
  (<.>) :: t -> t -> t

instance Transform Mat4 where
  ident = identity
  (<.>) = mul

instance Transform Local where
  ident = Local identity
  (Local a) <.> (Local b) = Local (mul a b)

instance Transform Global where
  ident = Global identity
  (Global a) <.> (Global b) = Global (mul a b)

newtype Local = Local Mat4
  deriving (Eq, Show)

newtype Global = Global Mat4
  deriving (Eq, Show)

identityLocal :: Local
identityLocal = Local identity

attach :: Entity -> Entity -> World -> World
attach p c w =
  let w1 = detach c w
  in E.relate @ParentOf p c w1

detach :: Entity -> World -> World
detach c w =
  case parent c w of
    Nothing -> w
    Just p -> E.unrelate @ParentOf p c w

parent :: Entity -> World -> Maybe Entity
parent e w =
  case E.inn @ParentOf e w of
    [] -> Nothing
    (p : _) -> Just p

children :: Entity -> World -> [Entity]
children = E.out @ParentOf

propagate :: World -> World
propagate w0 =
  let roots = Prelude.filter (\e -> parent e w0 == Nothing) (E.entities w0)
  in foldl' (propRoot w0) w0 roots

propRoot :: World -> World -> Entity -> World
propRoot w0 w e =
  let l = localOrIdentity w0 e
      g = toGlobal l
      w1 = E.set e g w
  in propChildren w0 w1 (IntSet.singleton (E.eid e)) e g

propChildren :: World -> World -> IntSet -> Entity -> Global -> World
propChildren w0 w visited p gp =
  foldl' (propChild w0 visited gp) w (children p w0)

propChild :: World -> IntSet -> Global -> World -> Entity -> World
propChild w0 visited gp w e =
  if IntSet.member (E.eid e) visited
    then w
    else
      let l = localOrIdentity w0 e
          g = combine gp l
          w1 = E.set e g w
          visited' = IntSet.insert (E.eid e) visited
      in propChildren w0 w1 visited' e g

localOrIdentity :: World -> Entity -> Local
localOrIdentity w e =
  case E.get @Local e w of
    Just l -> l
    Nothing -> identityLocal

toGlobal :: Local -> Global
toGlobal (Local m) = Global m

combine :: Global -> Local -> Global
combine (Global pm) (Local lm) = Global (mul pm lm)
