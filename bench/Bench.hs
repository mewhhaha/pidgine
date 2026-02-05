{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.List (foldl')
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Pos = Pos {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Vel = Vel {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Acc = Acc {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Hp = Hp Int
  deriving (Eq, Show)

data C
  = CPos Pos
  | CVel Vel
  | CAcc Acc
  | CHp Hp
  deriving (Generic)

instance E.ComponentId C


type World = E.World C
type Program msg a = S.Program C msg a
type Graph msg = S.Graph C msg

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

forceEachm :: World -> Double
forceEachm w =
  V.foldl'
    (\acc (E.EntityRow _ _ bag) ->
      let sumPos =
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> x + y
              Nothing -> 0
          sumVel =
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> vx + vy
              Nothing -> 0
      in acc + sumPos + sumVel
    )
    0
    (E.entityRowsV w)

data MoveLoop
data MoveLoop1
data MoveLoop2

data Move3 = Move3
  { pos3 :: Pos
  , vel3 :: Vel
  , acc3 :: Acc
  } deriving (Generic)

instance E.Queryable C Move3

buildWorld :: Int -> World
buildWorld n = snd (foldl' add (0 :: Int, E.emptyWorld) [1 .. n])
  where
    add (i, w) _ =
      let (_, w1) =
            E.spawn
              ( Pos (fromIntegral i) 0
              , Vel 1 1
              , Acc 0.1 0.1
              , Hp 100
              )
              w
      in (i + 1, w1)

pPV :: E.Plan C (Pos, Vel)
pPV = E.plan @(Pos, Vel)

pP :: E.Plan C Pos
pP = E.plan @Pos

pVA :: E.Plan C (Vel, Acc)
pVA = E.plan @(Vel, Acc)

pMove3 :: E.Plan C Move3
pMove3 = E.planMap (\(p, v, a) -> Move3 p v a) (E.plan @(Pos, Vel, Acc))

qPVQ :: E.Query C (Pos, Vel)
qPVQ = (,) <$> (E.comp :: E.Query C Pos) <*> (E.comp :: E.Query C Vel)

qPQ :: E.Query C Pos
qPQ = E.comp

moveProg :: Program String ()
moveProg = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachP pPV $ \(Pos x y, Vel vx vy) ->
      let p = Pos (x + vx) (y + vy)
          v = Vel vx vy
      in S.set p <> S.set v
  pure ()

moveProgSmall :: Program String ()
moveProgSmall = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachP pP $ \(Pos x y) ->
      let p = Pos (x + 1) y
      in S.set p
  pure ()

moveProgJoin3 :: Program String ()
moveProgJoin3 = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachP pMove3 $ \q ->
      let Pos x y = pos3 q
          Vel vx vy = vel3 q
          Acc ax ay = acc3 q
          vx' = vx + ax
          vy' = vy + ay
          p' = Pos (x + vx') (y + vy')
      in S.set p' <> S.set (Vel vx' vy')
  pure ()

moveProgTwoEach :: Program String ()
moveProgTwoEach = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    (S.eachP pPV $ \(Pos x y, Vel vx vy) ->
      let p = Pos (x + vx) (y + vy)
      in S.set p)
    *>
    (S.eachP pVA $ \(Vel vx vy, Acc ax ay) ->
      let v = Vel (vx + ax) (vy + ay)
      in S.set v)
  pure ()

moveProgTwoCompute :: Program String ()
moveProgTwoCompute = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachP pPV $ \(Pos x y, Vel vx vy) ->
      let p = Pos (x + vx) (y + vy)
      in S.set p
  _ <- S.await $ computeS $
    S.eachP pVA $ \(Vel vx vy, Acc ax ay) ->
      let v = Vel (vx + ax) (vy + ay)
      in S.set v
  pure ()

mkMoveProg :: Int -> Program String ()
mkMoveProg n = S.program (S.handle n) $ do
  _ <- S.await $ computeS $
    S.eachP pPV $ \(Pos x y, Vel vx vy) ->
      let p = Pos (x + vx) (y + vy)
      in S.set p
  pure ()

moveProgTenEaches :: Program String ()
moveProgTenEaches = S.program (S.handle 0) $ do
  let one =
        S.eachP pPV $ \(Pos x y, Vel vx vy) ->
          let p = Pos (x + vx) (y + vy)
          in S.set p
  _ <- S.await $ computeS $
    foldl' (*>) (pure ()) (replicate 10 one)
  pure ()

advance :: Double -> Pos -> Vel -> (Pos, Vel)
advance dt (Pos x y) (Vel vx vy) =
  let ax = sin (x * 0.01) * 0.5
      ay = cos (y * 0.01) * 0.5
      vx' = vx + ax * dt
      vy' = vy + ay * dt
      x' = x + vx' * dt + sin vx'
      y' = y + vy' * dt + cos vy'
  in (Pos x' y', Vel vx' vy')

moveProgLogic :: Program String ()
moveProgLogic = S.program (S.handle 0) $ do
  dt <- S.dt
  _ <- S.await $ computeS $
    S.eachP pPV $ \(p, v) ->
      let (p', v') = advance dt p v
      in S.set2 p' v'
  pure ()

moveStep :: F.Step (Pos, Vel) (Pos, Vel)
moveStep = F.Step $ \dt (Pos x y, Vel vx vy) ->
  let p = Pos (x + vx * dt) (y + vy * dt)
      v = Vel vx vy
  in ((p, v), moveStep)

moveProgStep :: Program String ()
moveProgStep = S.program (S.handle 1) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoop pPV $ \(p, v) -> do
      (p', v') <- S.step @MoveLoop moveStep (p, v)
      S.edit (S.set2 p' v')
  pure ()

movePatch :: (Pos, Vel) -> S.EntityPatch C
movePatch (Pos x y, Vel vx vy) =
  let p = Pos (x + vx) (y + vy)
      v = Vel vx vy
  in S.set2 p v

moveDirect :: (Pos, Vel) -> S.DirectPatch C
moveDirect (Pos x y, Vel vx vy) =
  let p = Pos (x + vx) (y + vy)
      v = Vel vx vy
  in S.set2Direct p v

moveSet2 :: (Pos, Vel) -> (Pos, Vel)
moveSet2 (Pos x y, Vel vx vy) =
  let p = Pos (x + vx) (y + vy)
      v = Vel vx vy
  in (p, v)

moveProgM :: Program String ()
moveProgM = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachPSet2 pPV moveSet2
  pure ()

moveProgMPure :: Program String ()
moveProgMPure = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachMPure pPV movePatch
  pure ()

moveProgMSmall :: Program String ()
moveProgMSmall = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoop pP $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p)
  pure ()

moveProgMJoin3 :: Program String ()
moveProgMJoin3 = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoop pMove3 $ \q -> do
      let Pos x y = pos3 q
          Vel vx vy = vel3 q
          Acc ax ay = acc3 q
          vx' = vx + ax
          vy' = vy + ay
          p' = Pos (x + vx') (y + vy')
      S.edit (S.set2 p' (Vel vx' vy'))
  pure ()

moveProgMTwoEach :: Program String ()
moveProgMTwoEach = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    (S.eachMP @MoveLoop1 pPV $ \(Pos x y, Vel vx vy) -> do
      let p = Pos (x + vx) (y + vy)
      S.edit (S.set p))
    *>
    (S.eachMP @MoveLoop2 pVA $ \(Vel vx vy, Acc ax ay) -> do
      let v = Vel (vx + ax) (vy + ay)
      S.edit (S.set v))
  pure ()

moveProgMLogic :: Program String ()
moveProgMLogic = S.program (S.handle 0) $ do
  dt <- S.dt
  _ <- S.await $ computeS $
    S.eachMP @MoveLoop pPV $ \(p, v) -> do
      let (p', v') = advance dt p v
      S.edit (S.set2 p' v')
  pure ()

graphEach :: Graph String
graphEach = S.graph @String moveProg

graphEachS :: Graph String
graphEachS = S.graph @String moveProgStep

graphEachM :: Graph String
graphEachM = S.graph @String moveProgM

graphEachSmall :: Graph String
graphEachSmall = S.graph @String moveProgSmall

graphEachJoin3 :: Graph String
graphEachJoin3 = S.graph @String moveProgJoin3

graphEachTwoEach :: Graph String
graphEachTwoEach = S.graph @String moveProgTwoEach

graphEachTwoCompute :: Graph String
graphEachTwoCompute = S.graph @String moveProgTwoCompute

graphEach10Programs :: Graph String
graphEach10Programs = S.graphList (map mkMoveProg [0 .. 9])

graphEach10InOne :: Graph String
graphEach10InOne = S.graph @String moveProgTenEaches

graphEachLogic :: Graph String
graphEachLogic = S.graph @String moveProgLogic

graphEachMSmall :: Graph String
graphEachMSmall = S.graph @String moveProgMSmall

graphEachMJoin3 :: Graph String
graphEachMJoin3 = S.graph @String moveProgMJoin3

graphEachMTwoEach :: Graph String
graphEachMTwoEach = S.graph @String moveProgMTwoEach

graphEachMLogic :: Graph String
graphEachMLogic = S.graph @String moveProgMLogic

graphEachMPure :: Graph String
graphEachMPure = S.graph @String moveProgMPure

runWarmTick :: Double -> World -> Graph String -> World
runWarmTick dt w0 g0 =
  let (w1, _, g1) = S.run dt w0 [] g0
      _ = length (E.entities w1)
      (w2, _, _) = S.run dt w1 [] g1
      _ = length (E.entities w2)
  in w2

main :: IO ()
main = defaultMain
  [ bgroup "ecs"
      [ let w = buildWorld 10000
        in bench "query-10k" $ nf (length . E.runq qPVQ) w
      , let w = buildWorld 10000
        in bench "query-small-10k" $ nf (length . E.runq qPQ) w
      , bench "spawn-10k" $ nf (length . E.entities . buildWorld) 10000
      ]
  , bgroup "program"
      [ bgroup "10k"
          [ let w = buildWorld 10000
            in bench "each" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-two" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-two-compute" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachTwoCompute
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-10-progs" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach10Programs
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-10-in-one" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEach10InOne
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachLogic
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-step" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-small" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-join3" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-two" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-logic" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMLogic
                in length (E.entities w1)
              ) w
          ]
      , bgroup "10k-warm"
          [ let w = buildWorld 10000
            in bench "each" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-small" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-join3" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-two" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-two-compute" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachTwoCompute
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-10-progs" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEach10Programs
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-10-in-one" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEach10InOne
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-logic" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachLogic
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-step" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachS
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachM
                in forceEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-pure" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachMPure
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-small" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachMSmall
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-join3" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachMJoin3
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-two" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachMTwoEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "eachm-logic" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachMLogic
                in length (E.entities w1)
              ) w
          ]
      ]
  ]
