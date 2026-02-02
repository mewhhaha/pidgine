{-# LANGUAGE TypeApplications #-}

module SystemProps
  ( system_resume_once
  , system_await_value
  , system_eachm_entity_state
  , prop_system_resume
  , prop_system_await_value
  ) where

import Data.List (foldl')
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.System as S

data WaitGo
data Speed
data Use
data CountLoop
data CountStep

newtype Count = Count Int
  deriving (Eq, Show)

data C
  = CInt Int
  | CCount Count

instance E.Component C Int where
  inj = CInt
  prj c = case c of
    CInt v -> Just v
    _ -> Nothing

instance E.Component C Count where
  inj = CCount
  prj c = case c of
    CCount v -> Just v
    _ -> Nothing

type World = E.World C
type System msg = S.System C msg
type SystemV msg a = S.SystemV C msg a
type Graph msg = S.Graph C msg

system_resume_once :: Bool
system_resume_once =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      sys :: SystemV String ()
      sys = S.system (S.handle 0) $ do
        S.each (E.comp @Int) (+1)
        _ <- S.awaitEvent (== "go")
        S.each (E.comp @Int) (+10)
        pure ()
      g0 :: Graph String
      g0 = S.graph @String sys
      (w1, _, g1) = S.run 0.1 w0 [] g0
      (w2, _, g2) = S.run 0.1 w1 [] g1
      (w3, _, g3) = S.run 0.1 w2 ["go"] g2
      (w4, _, _) = S.run 0.1 w3 [] g3
  in E.get @Int e w1 == Just 1
      && E.get @Int e w2 == Just 1
      && E.get @Int e w3 == Just 11
      && E.get @Int e w4 == Just 12

system_await_value :: Bool
system_await_value =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      speedSys :: SystemV String Int
      speedSys = S.system (S.handle 0) (pure 3)
      useSys :: System String
      useSys = S.system (S.handle 1) $ do
        v <- S.await (S.handle 0)
        S.each (E.comp @Int) (const v)
      g0 :: Graph String
      g0 = S.graph @String useSys speedSys
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 3

countStep :: F.Step (F.Events (Int -> Int)) Int
countStep = F.acc 0

countSys :: System ()
countSys = S.system (S.handle 0) $ do
  S.eachM @CountLoop (E.comp @Count) $ \_ -> do
    n <- S.step @CountStep countStep [(+1)]
    S.edit (S.set (Count n))

system_eachm_entity_state :: Bool
system_eachm_entity_state =
  let (e1, w1) = E.spawn (Count 0) (E.emptyWorld :: World)
      (e2, w2) = E.spawn (Count 0) w1
      g0 :: Graph ()
      g0 = S.graph @() countSys
      (w3, _, g1) = S.run 0.1 w2 [] g0
      (w4, _, _) = S.run 0.1 w3 [] g1
  in E.get @Count e1 w3 == Just (Count 1)
      && E.get @Count e2 w3 == Just (Count 1)
      && E.get @Count e1 w4 == Just (Count 2)
      && E.get @Count e2 w4 == Just (Count 2)

data Phase = Start | Wait
  deriving (Eq, Show)

stepPhase :: Phase -> Bool -> (Phase, Int)
stepPhase Start ev =
  if ev
    then (Start, 10)
    else (Wait, 1)
stepPhase Wait ev =
  if ev
    then (Start, 10)
    else (Wait, 0)

expectedValue :: [Bool] -> Int
expectedValue evs =
  snd (foldl' go (Start, 0) evs)
  where
    go (ph, acc) ev =
      let (ph', delta) = stepPhase ph ev
      in (ph', acc + delta)

runFrames :: [Bool] -> Int
runFrames evs =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      sys :: SystemV String ()
      sys = S.system (S.handle 0) $ do
        S.each (E.comp @Int) (+1)
        _ <- S.awaitEvent (== "go")
        S.each (E.comp @Int) (+10)
        pure ()
      g0 :: Graph String
      g0 = S.graph @String sys
      step (w, g) ev =
        let inbox = if ev then ["go"] else []
            (w', _, g') = S.run 0.1 w inbox g
        in (w', g')
      (wf, _) = foldl' step (w0, g0) evs
  in case E.get @Int e wf of
      Nothing -> 0
      Just v -> v

prop_system_resume :: [Bool] -> Bool
prop_system_resume evs =
  runFrames evs == expectedValue evs

prop_system_await_value :: Int -> Bool
prop_system_await_value v =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      speedSys :: SystemV String Int
      speedSys = S.system (S.handle 0) (pure v)
      useSys :: System String
      useSys = S.system (S.handle 1) $ do
        v' <- S.await (S.handle 0)
        S.each (E.comp @Int) (const v')
      g0 :: Graph String
      g0 = S.graph @String useSys speedSys
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just v
