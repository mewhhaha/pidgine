module Engine.Data.FRP
  ( Time
  , DTime
  , Step(..)
  , id
  , Signal
  , Events
  , run
  , dt
  , time
  , int
  , after
  , delay
  , hold
  , acc
  , edge
  , gate
  , once
  , switch
  , switchd
  ) where

import Prelude hiding ((.), id)

import Control.Arrow (Arrow(..), ArrowChoice(..), ArrowLoop(..))
import Control.Category (Category(..))

type Time = Double
type DTime = Double

newtype Step a b = Step
  { stepS :: DTime -> a -> (b, Step a b)
  }

type Signal a = Step () a

type Events a = [a]

run :: Step a b -> [(DTime, a)] -> [b]
run _ [] = []
run s ((d, a) : rest) =
  let (out, s') = stepS s d a
  in out : run s' rest

dt :: Step a DTime
dt = Step $ \dt' _ -> (dt', dt)

time :: Step a Time
time = int . pure (1 :: Time)

int :: Fractional a => Step a a
int = go 0
  where
    go total = Step $ \d a ->
      let total' = total + a * realToFrac d
      in (total', go total')

after :: Time -> Step a (Events ())
after t = go 0
  where
    go elapsed = Step $ \d _ ->
      let elapsed' = elapsed + realToFrac d
      in if elapsed' >= t
          then ([()], done)
          else ([], go elapsed')
    done = Step $ \_ _ -> ([], done)

delay :: b -> Step b b
delay b0 = Step $ \_ b -> (b0, delay b)

hold :: a -> Step (Events a) a
hold a0 = Step $ \_ as ->
  let a1 = case reverse as of
        [] -> a0
        (x : _) -> x
  in (a1, hold a1)

acc :: s -> Step (Events (s -> s)) s
acc s0 = Step $ \_ fs ->
  let s1 = foldl (\s f -> f s) s0 fs
  in (s1, acc s1)


edge :: Eq a => Step a (Events a)
edge = go Nothing
  where
    go prev = Step $ \_ a ->
      let evs = case prev of
            Nothing -> [a]
            Just p -> if p /= a then [a] else []
      in (evs, go (Just a))

gate :: Step (Bool, Events a) (Events a)
gate = Step $ \_ (ok, evs) ->
  (if ok then evs else [], gate)

once :: Step (Events a) (Events a)
once = go False
  where
    go done = Step $ \_ evs ->
      if done
        then ([], go True)
        else case evs of
          [] -> ([], go False)
          (x : _) -> ([x], go True)

switch :: Step a (b, Events (Step a b)) -> Step a b
switch s0 = Step $ \d a ->
  let ((b, evs), s1) = stepS s0 d a
  in case lastEvent evs of
      Nothing -> (b, switch s1)
      Just sNext -> (b, sNext)

switchd :: Step a (b, Events (Step a b)) -> Step a b
switchd s0 = Step $ \d a ->
  let ((b, evs), s1) = stepS s0 d a
  in case lastEvent evs of
      Nothing -> (b, switchd s1)
      Just sNext -> (b, delayOnce s1 sNext)

instance Category Step where
  id = Step $ \_ a -> (a, id)
  (Step g) . (Step f) = Step $ \d a ->
    let (b, f') = f d a
        (c, g') = g d b
    in (c, g' . f')

instance Arrow Step where
  arr f = Step $ \_ a -> (f a, arr f)
  first (Step f) = Step $ \d (b, c) ->
    let (b', f') = f d b
    in ((b', c), first f')

instance ArrowChoice Step where
  left (Step f) = Step $ \d eab ->
    case eab of
      Left a ->
        let (b, f') = f d a
        in (Left b, left f')
      Right c -> (Right c, left (Step f))

instance ArrowLoop Step where
  loop (Step f) = Step $ \d a ->
    let ((b, c), f') = f d (a, c)
    in (b, loop f')

instance Functor (Step a) where
  fmap f (Step g) = Step $ \d a ->
    let (b, g') = g d a
    in (f b, fmap f g')

instance Applicative (Step a) where
  pure x = Step $ \_ _ -> (x, pure x)
  Step f <*> Step g = Step $ \d a ->
    let (f', fstep) = f d a
        (b, gstep) = g d a
    in (f' b, fstep <*> gstep)

instance Semigroup b => Semigroup (Step a b) where
  (Step f) <> (Step g) = Step $ \d a ->
    let (b, f') = f d a
        (c, g') = g d a
    in (b <> c, f' <> g')

instance Monoid b => Monoid (Step a b) where
  mempty = pure mempty

lastEvent :: Events a -> Maybe a
lastEvent = foldl (\_ x -> Just x) Nothing

delayOnce :: Step a (b, Events (Step a b)) -> Step a b -> Step a b
delayOnce sOld sNext = Step $ \d a ->
  let ((b, _), _) = stepS sOld d a
  in (b, sNext)
