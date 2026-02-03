{-# LANGUAGE GADTs #-}

module Engine.Data.Program.Core
  ( Program(..)
  , Gate(..)
  , Input(..)
  , await
  , gateEvent
  , gateEvents
  , gateProgram
  , gateValue
  , ProgState(..)
  , resume
  , replay
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (find)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Engine.Data.FRP (DTime, Events)

data Input msg = Input
  { inputDt :: !DTime
  , inputEvents :: Events msg
  , inputDone :: !IntSet
  , inputValues :: !(IntMap Any)
  }

newtype Gate msg a = Gate
  { runGate :: Input msg -> Maybe a
  }

data Program msg a where
  Done :: a -> Program msg a
  Await :: Gate msg b -> (b -> Program msg a) -> Program msg a

instance Functor (Program msg) where
  fmap f (Done a) = Done (f a)
  fmap f (Await g k) = Await g (fmap f . k)

instance Applicative (Program msg) where
  pure = Done
  Done f <*> p = fmap f p
  Await g k <*> p = Await g (\x -> k x <*> p)

instance Monad (Program msg) where
  Done a >>= k = k a
  Await g k >>= f = Await g (\x -> k x >>= f)

await :: Gate msg a -> Program msg a
await g = Await g Done

gateEvent :: (msg -> Bool) -> Gate msg msg
gateEvent p = Gate $ \input -> find p (inputEvents input)

gateEvents :: Gate msg (Events msg)
gateEvents = Gate $ \input ->
  case inputEvents input of
    [] -> Nothing
    xs -> Just xs

gateProgram :: Int -> Gate msg ()
gateProgram sid = Gate $ \input ->
  if IntSet.member sid (inputDone input)
    then Just ()
    else Nothing

gateValue :: Int -> Gate msg a
gateValue sid = Gate $ \input ->
  case IntMap.lookup sid (inputValues input) of
    Nothing -> Nothing
    Just v -> Just (unsafeCoerce v)

data ProgState msg = ProgState
  { consumed :: [Input msg]
  }

resume :: [Input msg] -> Program msg a -> (Program msg a, [Input msg])
resume inputs prog = go inputs [] prog
  where
    go [] acc p = (p, reverse acc)
    go _ acc (Done a) = (Done a, reverse acc)
    go (i:is) acc (Await g k) =
      case runGate g i of
        Nothing -> (Await g k, reverse acc)
        Just x -> go is (i : acc) (k x)

replay :: ProgState msg -> Input msg -> Program msg a -> (ProgState msg, Program msg a, Bool, Maybe a)
replay st input prog =
  let consumed0 = consumed st
      (prog', consumed') = resume (consumed0 ++ [input]) prog
      progressed = length consumed' > length consumed0
      done = case prog' of
        Done a -> Just a
        _ -> Nothing
  in (ProgState consumed', prog', progressed, done)
