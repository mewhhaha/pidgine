{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ProgramProps
  ( program_resume_once
  , program_await_value
  , program_eachm_entity_state
  , program_eachm_enemy_state_machine
  , program_each_tuple_query
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
data Marker = Marker
  deriving (Eq, Show)

newtype Count = Count Int
  deriving (Eq, Show)

data EnemyTag = EnemyTag
  deriving (Eq, Show)

data EnemyAI
  = Idle
  | Windup
  | Attack
  | Recover
  deriving (Eq, Show)

data C
  = CInt Int
  | CBool Bool
  | CCount Count
  | CMarker Marker
  | CEnemy EnemyTag
  | CEnemyAI EnemyAI
  deriving (Generic)

instance E.ComponentId C

type World = E.World C
type Graph msg = S.Graph C msg
type ProgramM msg a = S.ProgramM C msg a

qMarker :: E.Query C Marker
qMarker = E.comp

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

computeU :: S.Batch C () a -> S.Batch C () a
computeU = S.compute

program_resume_once :: Bool
program_resume_once =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      sys :: ProgramM String ()
      sys = do
        _ <- S.await $ computeS $ do
          S.each @Int $ \n ->
            S.set (n + 1)
        _ <- S.await (== "go")
        _ <- S.await $ computeS $ do
          S.each @Int $ \n ->
            S.set (n + 10)
        pure ()
      g0 :: Graph String
      g0 =
        S.graph $ do
          _ <- S.program sys
          pure ()
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
      g0 :: Graph String
      g0 =
        S.graph $ do
          speedH <- S.program (pure 3)
          _ <- S.program $ do
            v <- S.await speedH
            _ <- S.await $ computeS $ do
              S.each @Int $ \_ ->
                S.set (v :: Int)
            pure ()
          pure ()
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 3

countStep :: F.Step (F.Events (Int -> Int)) Int
countStep = F.acc 0

countProg :: ProgramM () ()
countProg = do
  _ <- S.await $ computeU $ do
    S.eachM @Count $ \_ -> do
      n <- S.step countStep [(+1)]
      S.edit (S.set (Count n))
  pure ()

tagEvent :: b -> F.Events a -> F.Events b
tagEvent b = fmap (const b)

idleStep :: F.Step () (EnemyAI, F.Events (F.Step () EnemyAI))
idleStep = (\ev -> (Idle, tagEvent windupState ev)) <$> F.after 0.5

windupState :: F.Step () EnemyAI
windupState = F.switch windupStep

windupStep :: F.Step () (EnemyAI, F.Events (F.Step () EnemyAI))
windupStep = (\ev -> (Windup, tagEvent attackState ev)) <$> F.after 0.2

attackState :: F.Step () EnemyAI
attackState = F.switch attackStep

attackStep :: F.Step () (EnemyAI, F.Events (F.Step () EnemyAI))
attackStep = (\ev -> (Attack, tagEvent recoverState ev)) <$> F.after 0.1

recoverState :: F.Step () EnemyAI
recoverState = F.switch recoverStep

recoverStep :: F.Step () (EnemyAI, F.Events (F.Step () EnemyAI))
recoverStep = (\ev -> (Recover, tagEvent idleState ev)) <$> F.after 0.4

idleState :: F.Step () EnemyAI
idleState = F.switch idleStep

enemyAiProg :: ProgramM () ()
enemyAiProg = do
  _ <- S.await $ computeU $ do
    S.eachM @EnemyTag $ \_ -> do
      st <- S.step idleState ()
      S.edit (S.set st)
  pure ()

runFrameU :: World -> Graph () -> (World, Graph ())
runFrameU w g =
  let (w', _, g') = S.run 0.1 w [] g
  in (w', g')

program_eachm_enemy_state_machine :: Bool
program_eachm_enemy_state_machine =
  let (e1, w0) = E.spawn (EnemyTag, Idle) (E.emptyWorld :: World)
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program enemyAiProg
          pure ()
      (w1, g1) = runFrameU w0 g0
      (w2, g2) = runFrameU w1 g1
      (w3, g3) = runFrameU w2 g2
      (e2, w3') = E.spawn (EnemyTag, Idle) w3
      (w4, g4) = runFrameU w3' g3
      (w5, g5) = runFrameU w4 g4
      (w6, g6) = runFrameU w5 g5
      (w7, g7) = runFrameU w6 g6
      (w8, g8) = runFrameU w7 g7
      (w9, _) = runFrameU w8 g8
  in E.get @EnemyAI e1 w4 == Just Idle
      && E.get @EnemyAI e1 w6 == Just Windup
      && E.get @EnemyAI e1 w8 == Just Attack
      && E.get @EnemyAI e2 w4 == Just Idle
      && E.get @EnemyAI e2 w8 == Just Idle
      && E.get @EnemyAI e2 w9 == Just Windup

program_eachm_entity_state :: Bool
program_eachm_entity_state =
  let (e1, w1) = E.spawn (Count 0) (E.emptyWorld :: World)
      (e2, w2) = E.spawn (Count 0) w1
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program countProg
          pure ()
      (w3, _, g1) = S.run 0.1 w2 [] g0
      (w4, _, _) = S.run 0.1 w3 [] g1
  in E.get @Count e1 w3 == Just (Count 1)
      && E.get @Count e2 w3 == Just (Count 1)
      && E.get @Count e1 w4 == Just (Count 2)
      && E.get @Count e2 w4 == Just (Count 2)

program_each_tuple_query :: Bool
program_each_tuple_query =
  let (e, w0) = E.spawn ((3 :: Int), True) (E.emptyWorld :: World)
      prog :: ProgramM () ()
      prog = do
        _ <- S.await $ computeU $ do
          S.each @(Int, Bool) $ \(n, alive) ->
            if alive
              then S.set (n + 1)
              else mempty
        pure ()
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program prog
          pure ()
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just 4

program_compute_fused_order :: Bool
program_compute_fused_order =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      markProg :: ProgramM () ()
      markProg = do
        marks <- S.await $ computeU $
          S.each @Int (\_ -> S.set Marker) *> S.collect qMarker
        S.world (S.at e (S.set (length marks)))
        pure ()
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program markProg
          pure ()
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
      sys :: ProgramM String ()
      sys = do
        _ <- S.await $ computeS $ do
          S.each @Int $ \n ->
            S.set (n + 1)
        _ <- S.await (== "go")
        _ <- S.await $ computeS $ do
          S.each @Int $ \n ->
            S.set (n + 10)
        pure ()
      g0 :: Graph String
      g0 =
        S.graph $ do
          _ <- S.program sys
          pure ()
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
      g0 :: Graph String
      g0 =
        S.graph $ do
          speedH <- S.program (pure v)
          _ <- S.program $ do
            v' <- S.await speedH
            _ <- S.await $ computeS $ do
              S.each @Int $ \_ ->
                S.set (v' :: Int)
            pure ()
          pure ()
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Int e w1 == Just v
