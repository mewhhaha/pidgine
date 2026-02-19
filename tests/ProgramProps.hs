{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ProgramProps
  ( program_resume_once
  , program_await_value
  , program_eachm_entity_state
  , program_eachm_enemy_state_machine
  , program_eachm_independent_loops
  , program_step_independent_callsites
  , program_drive_del_stops
  , program_each_tuple_query
  , program_compute_fused_order
  , program_collect_fused_order
  , program_event_chain
  , prop_program_resume
  , prop_program_await_value
  , prop_program_patch_identity
  , prop_program_patch_assoc
  , prop_program_collect_fused
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

newtype LoopA = LoopA Int
  deriving (Eq, Show)

newtype LoopB = LoopB Int
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
  | CLoopA LoopA
  | CLoopB LoopB
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

runPatch :: S.Patch C -> World -> World
runPatch p w0 =
  let g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program (S.world p)
          pure ()
      (w1, _, _) = S.run 0.1 w0 [] g0
  in w1

countsCompute :: S.Batch C (Int, Int) (Int, Int)
countsCompute =
  (,) <$> (length . map snd <$> S.collect qMarker)
      <*> (S.each @Int (const (S.set Marker)) *> fmap length (S.collect qMarker))

runCollectGraph :: World -> (World, S.Events (Int, Int), Graph (Int, Int))
runCollectGraph w0 =
  let g0 :: Graph (Int, Int)
      g0 =
        S.graph $ do
          _ <- S.program $ do
            counts <- S.await $ S.compute countsCompute
            S.send [counts]
          pure ()
  in S.run 0.1 w0 [] g0

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

program_eachm_independent_loops :: Bool
program_eachm_independent_loops =
  let (e, w0) = E.spawn (Count 0) (E.emptyWorld :: World)
      prog :: ProgramM () ()
      prog = do
        _ <- S.await $ computeU $
          S.eachM @Count (\_ -> do
            n <- S.step (F.acc (0 :: Int)) [(+1)]
            S.edit (S.set (LoopA n))
          ) *>
          S.eachM @Count (\_ -> do
            n <- S.step (F.acc (100 :: Int)) [(+10)]
            S.edit (S.set (LoopB n))
          )
        pure ()
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program prog
          pure ()
      (w1, _, g1) = S.run 0.1 w0 [] g0
      (w2, _, _) = S.run 0.1 w1 [] g1
  in E.get @LoopA e w1 == Just (LoopA 1)
      && E.get @LoopB e w1 == Just (LoopB 110)
      && E.get @LoopA e w2 == Just (LoopA 2)
      && E.get @LoopB e w2 == Just (LoopB 120)

program_step_independent_callsites :: Bool
program_step_independent_callsites =
  let prog :: ProgramM (Int, Int) ()
      prog = do
        a <- S.step (F.acc (0 :: Int)) [(+1)]
        b <- S.step (F.acc (100 :: Int)) [(+10)]
        S.send [(a, b)]
      g0 :: Graph (Int, Int)
      g0 =
        S.graph $ do
          _ <- S.program prog
          pure ()
      (w1, out1, g1) = S.run 0.1 (E.emptyWorld :: World) [] g0
      (_, out2, _) = S.run 0.1 w1 [] g1
  in out1 == [(1, 110)] && out2 == [(2, 120)]

counterStep :: Int -> F.Step () Int
counterStep n = F.Step $ \_ () -> (n, counterStep (n + 1))

tickDriven :: ProgramM () ()
tickDriven = do
  _ <- S.await $ computeU $
    S.each @() (const mempty)
  pure ()

program_drive_del_stops :: Bool
program_drive_del_stops =
  let (e, w0) = E.spawn () (E.emptyWorld :: World)
      w1 = runPatch (S.drive e (counterStep 0)) w0
      g0 :: Graph ()
      g0 =
        S.graph $ do
          _ <- S.program tickDriven
          pure ()
      (w2, _, g1) = S.run 0.1 w1 [] g0
      w3 = runPatch (S.at e (S.del @Int)) w2
      (w4, _, g2) = S.run 0.1 w3 [] g1
      (w5, _, _) = S.run 0.1 w4 [] g2
  in E.get @Int e w1 == Just 0
      && E.get @Int e w2 == Just 1
      && E.get @Int e w3 == Nothing
      && E.get @Int e w4 == Nothing
      && E.get @Int e w5 == Nothing

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

prop_program_patch_identity :: Int -> Bool
prop_program_patch_identity x =
  let (e, w0) = E.spawn (x :: Int) (E.emptyWorld :: World)
      w1 = runPatch (mempty :: S.Patch C) w0
  in E.get @Int e w1 == Just x

prop_program_patch_assoc :: Int -> Bool
prop_program_patch_assoc x =
  let (e, w0) = E.spawn (x :: Int) (E.emptyWorld :: World)
      p1 = S.at e (S.set (x + 1))
      p2 = S.at e (S.set (x + 2))
      p3 = S.at e (S.set (x + 3))
      w1 = runPatch (p1 <> (p2 <> p3)) w0
      w2 = runPatch ((p1 <> p2) <> p3) w0
  in E.get @Int e w1 == Just (x + 3)
      && E.get @Int e w1 == E.get @Int e w2

program_collect_fused_order :: Bool
program_collect_fused_order =
  let (e, w0) = E.spawn (0 :: Int) (E.emptyWorld :: World)
      (w1, out, _) = runCollectGraph w0
  in out == [(0, 1)] && E.get @Marker e w1 == Just Marker

prop_program_collect_fused :: Int -> Bool
prop_program_collect_fused x =
  let (_, w0) = E.spawn (x :: Int) (E.emptyWorld :: World)
      (_, out, _) = runCollectGraph w0
  in out == [(0, 1)]

program_event_chain :: Bool
program_event_chain =
  let (e, w0) = E.spawn (Count 0) (E.emptyWorld :: World)
      g0 :: Graph Int
      g0 =
        S.graph $ do
          _ <- S.program (S.send [1 :: Int])
          _ <- S.program $ do
            _ <- S.await (== (1 :: Int))
            S.world (S.at e (S.set (Count 1)))
            S.send [2 :: Int]
          _ <- S.program $ do
            _ <- S.await (== (2 :: Int))
            S.world (S.at e (S.set (Count 2)))
            S.send [3 :: Int]
          _ <- S.program $ do
            _ <- S.await (== (3 :: Int))
            S.world (S.at e (S.set (Count 3)))
          pure ()
      (w1, _, _) = S.run 0.1 w0 [] g0
  in E.get @Count e w1 == Just (Count 3)

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
