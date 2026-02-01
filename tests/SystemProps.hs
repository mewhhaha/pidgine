{-# LANGUAGE TypeApplications #-}

module SystemProps
  ( system_resume_once
  , system_await_value
  , prop_system_resume
  , prop_system_await_value
  ) where

import Data.List (foldl')
import qualified Engine.Data.ECS as E
import qualified Engine.Data.System as S

data WaitGo
data Speed
data Use

system_resume_once :: Bool
system_resume_once =
  let (e, w0) = E.spawn (0 :: Int) E.empty
      sys :: S.SystemV String ()
      sys = S.system @WaitGo $ do
        S.each (E.comp @Int) (+1)
        _ <- S.await (S.Event (== "go"))
        S.each (E.comp @Int) (+10)
        pure ()
      g0 :: S.Graph String
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
  let (e, w0) = E.spawn (0 :: Int) E.empty
      speedSys :: S.SystemV String Int
      speedSys = S.system @Speed (pure 3)
      useSys :: S.System String
      useSys = S.system @Use $ do
        v <- S.await speedSys
        S.each (E.comp @Int) (const v)
      g0 :: S.Graph String
      g0 = S.graph @String useSys speedSys
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 3

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
  let (e, w0) = E.spawn (0 :: Int) E.empty
      sys :: S.SystemV String ()
      sys = S.system @WaitGo $ do
        S.each (E.comp @Int) (+1)
        _ <- S.await (S.Event (== "go"))
        S.each (E.comp @Int) (+10)
        pure ()
      g0 :: S.Graph String
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
  let (e, w0) = E.spawn (0 :: Int) E.empty
      speedSys :: S.SystemV String Int
      speedSys = S.system @Speed (pure v)
      useSys :: S.System String
      useSys = S.system @Use $ do
        v' <- S.await speedSys
        S.each (E.comp @Int) (const v')
      g0 :: S.Graph String
      g0 = S.graph @String useSys speedSys
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just v
