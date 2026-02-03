{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ProgramProps
  ( program_resume_once
  , program_await_value
  , program_eachm_entity_state
  , program_compute_fused_order
  , prop_program_resume
  , prop_program_await_value
  ) where

import Data.List (foldl')
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data WaitGo
data Speed
data Use
data CountLoop
data CountStep
data Marker = Marker
  deriving (Eq, Show)

newtype Count = Count Int
  deriving (Eq, Show)

data C
  = CInt Int
  | CCount Count
  | CMarker Marker
  deriving (Generic)

instance E.ComponentId C

type World = E.World C
type Program msg a = S.Program C msg a
type Graph msg = S.Graph C msg

qInt :: E.Query C Int
qInt = E.comp

qMarker :: E.Query C Marker
qMarker = E.comp

qCount :: E.Query C Count
qCount = E.comp

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

computeU :: S.Batch C () a -> S.Batch C () a
computeU = S.compute

program_resume_once :: Bool
program_resume_once =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      sys :: Program String ()
      sys = S.program (S.handle 0) $ do
        _ <- S.await $ computeS $ do
          S.each qInt $ \n ->
            S.set (n + 1)
        _ <- S.await (== "go")
        _ <- S.await $ computeS $ do
          S.each qInt $ \n ->
            S.set (n + 10)
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

program_await_value :: Bool
program_await_value =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      speedProg :: Program String Int
      speedProg = S.program (S.handle 0) (pure 3)
      useProg :: Program String ()
      useProg = S.program (S.handle 1) $ do
        v <- S.await (S.handle 0)
        _ <- S.await $ computeS $ do
          S.each qInt $ \_ ->
            S.set (v :: Int)
        pure ()
      g0 :: Graph String
      g0 = S.graph @String useProg speedProg
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 3

countStep :: F.Step (F.Events (Int -> Int)) Int
countStep = F.acc 0

countProg :: Program () ()
countProg = S.program (S.handle 0) $ do
  _ <- S.await $ computeU $ do
    S.eachM @CountLoop qCount $ \_ -> do
      n <- S.step @CountStep countStep [(+1)]
      S.edit (S.set (Count n))
  pure ()

program_eachm_entity_state :: Bool
program_eachm_entity_state =
  let (e1, w1) = E.spawn (Count 0) (E.emptyWorld :: World)
      (e2, w2) = E.spawn (Count 0) w1
      g0 :: Graph ()
      g0 = S.graph @() countProg
      (w3, _, g1) = S.run 0.1 w2 [] g0
      (w4, _, _) = S.run 0.1 w3 [] g1
  in E.get @Count e1 w3 == Just (Count 1)
      && E.get @Count e2 w3 == Just (Count 1)
      && E.get @Count e1 w4 == Just (Count 2)
      && E.get @Count e2 w4 == Just (Count 2)

program_compute_fused_order :: Bool
program_compute_fused_order =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      markProg :: Program () ()
      markProg = S.program (S.handle 0) $ do
        marks <- S.await $ computeU $
          S.each qInt (\_ -> S.set Marker) *> S.collect qMarker
        S.world (S.at e (S.set (length marks)))
        pure ()
      g0 :: Graph ()
      g0 = S.graph @() markProg
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 1

data Phase = Start | Wait
  deriving (Eq, Show)

stepPhase :: Phase -> Bool -> (Phase, Int)
stepPhase Start ev =
  if ev
    then (Start, 11)
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
      sys :: Program String ()
      sys = S.program (S.handle 0) $ do
        _ <- S.await $ computeS $ do
          S.each qInt $ \n ->
            S.set (n + 1)
        _ <- S.await (== "go")
        _ <- S.await $ computeS $ do
          S.each qInt $ \n ->
            S.set (n + 10)
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

prop_program_resume :: [Bool] -> Bool
prop_program_resume evs =
  runFrames evs == expectedValue evs

prop_program_await_value :: Int -> Bool
prop_program_await_value v =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      speedProg :: Program String Int
      speedProg = S.program (S.handle 0) (pure v)
      useProg :: Program String ()
      useProg = S.program (S.handle 1) $ do
        v' <- S.await (S.handle 0)
        _ <- S.await $ computeS $ do
          S.each qInt $ \_ ->
            S.set (v' :: Int)
        pure ()
      g0 :: Graph String
      g0 = S.graph @String useProg speedProg
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just v
