{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Bits (bit, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(runIdentity))
import GHC.Generics (Generic)
import qualified Aztecs.ECS as AZ
import qualified Aztecs.ECS.Query as AZQ
import qualified Aztecs.ECS.World as AZW
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

data AzPos = AzPos {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data AzVel = AzVel {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data AzAcc = AzAcc {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

newtype AzHp = AzHp Int
  deriving (Eq, Show)

instance (Monad m) => AZ.Component m AzPos
instance (Monad m) => AZ.Component m AzVel
instance (Monad m) => AZ.Component m AzAcc
instance (Monad m) => AZ.Component m AzHp

type AzWorld = AZ.World Identity

azMoveQ :: AZ.Query Identity AzPos
azMoveQ =
  AZ.queryMapWith
    (\(AzVel vx vy) (AzPos x y) -> AzPos (x + vx) (y + vy))
    (AZ.query @Identity @AzVel)

azPosQ :: AZ.Query Identity AzPos
azPosQ =
  AZ.queryMapWith
    (\(AzPos x y) _ -> AzPos (x + 1) y)
    (AZ.query @Identity @AzPos)

azVelQ :: AZ.Query Identity AzVel
azVelQ =
  AZ.queryMapWith
    (\(AzVel vx vy) _ -> AzVel (vx + 1) vy)
    (AZ.query @Identity @AzVel)

azAccQ :: AZ.Query Identity AzAcc
azAccQ =
  AZ.queryMapWith
    (\(AzAcc ax ay) _ -> AzAcc (ax + 1) ay)
    (AZ.query @Identity @AzAcc)

azHpQ :: AZ.Query Identity AzHp
azHpQ =
  AZ.queryMapWith
    (\(AzHp hp) _ -> AzHp (hp + 1))
    (AZ.query @Identity @AzHp)

runAzTickEachm :: AzWorld -> AzWorld
runAzTickEachm w0 =
  let (_, w1) =
        runIdentity $
          AZ.runAccess
            (AZ.system (AZ.runQuery azMoveQ))
            w0
  in w1

runAzTickEachm4Progs :: AzWorld -> AzWorld
runAzTickEachm4Progs w0 =
  let sys = AZ.system (AZ.runQuery azMoveQ)
      (_, w1) =
        runIdentity $
          AZ.runAccess
            (sys *> sys *> sys *> sys)
            w0
  in w1

runAzTickSplit :: AzWorld -> AzWorld
runAzTickSplit w0 =
  let (_, w1) =
        runIdentity $
          AZ.runAccess
            ( AZ.system (AZ.runQuery azPosQ)
                *> AZ.system (AZ.runQuery azVelQ)
            )
            w0
  in w1

runAzTickSplit4 :: AzWorld -> AzWorld
runAzTickSplit4 w0 =
  let (_, w1) =
        runIdentity $
          AZ.runAccess
            ( AZ.system (AZ.runQuery azPosQ)
                *> AZ.system (AZ.runQuery azVelQ)
                *> AZ.system (AZ.runQuery azAccQ)
                *> AZ.system (AZ.runQuery azHpQ)
            )
            w0
  in w1

buildAzWorld :: Int -> AzWorld
buildAzWorld n = snd (foldl' add (0 :: Int, AZW.empty) [1 .. n])
  where
    add (i, w) _ =
      let (_, w1, _) =
            AZW.spawn
              ( AZ.bundle (AzPos (fromIntegral i) 0)
                  <> AZ.bundle (AzVel 1 1)
                  <> AZ.bundle (AzAcc 0.1 0.1)
                  <> AZ.bundle (AzHp 100)
              )
              w
      in (i + 1, w1)

buildAzWorldPlayers :: Int -> Int -> AzWorld
buildAzWorldPlayers unrelatedCount playerCount =
  let addUnrelated w _ =
        let (_, w1, _) = AZW.spawn (AZ.bundle (AzHp 100)) w
        in w1
      addPlayer (i, w) _ =
        let (_, w1, _) =
              AZW.spawn
                (AZ.bundle (AzPos (fromIntegral i) 0) <> AZ.bundle (AzVel 1 1))
                w
        in (i + 1, w1)
      w0 = foldl' addUnrelated AZW.empty [1 .. unrelatedCount]
  in snd (foldl' addPlayer (0 :: Int, w0) [1 .. playerCount])

buildAzWorldSplit :: Int -> Int -> AzWorld
buildAzWorldSplit posCount velCount =
  let addPos (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzPos (fromIntegral i) 0)) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzVel (fromIntegral i) 1)) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, AZW.empty) [1 .. posCount]
  in snd (foldl' addVel (0 :: Int, w1) [1 .. velCount])

buildAzWorldSplit4 :: Int -> Int -> Int -> Int -> AzWorld
buildAzWorldSplit4 posCount velCount accCount hpCount =
  let addPos (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzPos (fromIntegral i) 0)) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzVel (fromIntegral i) 1)) w
        in (i + 1, w1)
      addAcc (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzAcc (fromIntegral i) 1)) w
        in (i + 1, w1)
      addHp (i, w) _ =
        let (_, w1, _) =
              AZW.spawn (AZ.bundle (AzHp (i + 100))) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, AZW.empty) [1 .. posCount]
      (_, w2) = foldl' addVel (0 :: Int, w1) [1 .. velCount]
      (_, w3) = foldl' addAcc (0 :: Int, w2) [1 .. accCount]
  in snd (foldl' addHp (0 :: Int, w3) [1 .. hpCount])

forceAzEachm :: AzWorld -> Double
forceAzEachm w =
  let q =
        (,)
          <$> (AZQ.query @Identity @AzPos)
          <*> (AZQ.query @Identity @AzVel)
      (vals, _) = runIdentity (AZQ.readQuery q (AZW.entities w))
  in V.foldl'
      (\acc (AzPos x y, AzVel vx vy) -> acc + x + y + vx + vy)
      0
      vals

forceAzSplit :: AzWorld -> Double
forceAzSplit w =
  let (posVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzPos) (AZW.entities w))
      (velVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzVel) (AZW.entities w))
      sumPos =
        V.foldl'
          (\acc (AzPos x y) -> acc + x + y)
          0
          posVals
      sumVel =
        V.foldl'
          (\acc (AzVel vx vy) -> acc + vx + vy)
          0
          velVals
  in sumPos + sumVel

forceAzSplit4 :: AzWorld -> Double
forceAzSplit4 w =
  let (posVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzPos) (AZW.entities w))
      (velVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzVel) (AZW.entities w))
      (accVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzAcc) (AZW.entities w))
      (hpVals, _) = runIdentity (AZQ.readQuery (AZQ.query @Identity @AzHp) (AZW.entities w))
      sumPos =
        V.foldl'
          (\acc (AzPos x y) -> acc + x + y)
          0
          posVals
      sumVel =
        V.foldl'
          (\acc (AzVel vx vy) -> acc + vx + vy)
          0
          velVals
      sumAcc =
        V.foldl'
          (\acc (AzAcc ax ay) -> acc + ax + ay)
          0
          accVals
      sumHp =
        V.foldl'
          (\acc (AzHp hp) -> acc + fromIntegral hp)
          0
          hpVals
  in sumPos + sumVel + sumAcc + sumHp

computeS :: S.Batch C String a -> S.Batch C String a
computeS = S.compute

reqPV :: E.Sig
reqPV =
  bit (E.componentBitOf @C @Pos)
    .|. bit (E.componentBitOf @C @Vel)

reqP :: E.Sig
reqP = bit (E.componentBitOf @C @Pos)

reqV :: E.Sig
reqV = bit (E.componentBitOf @C @Vel)

reqA :: E.Sig
reqA = bit (E.componentBitOf @C @Acc)

reqH :: E.Sig
reqH = bit (E.componentBitOf @C @Hp)

forceEachm :: World -> Double
forceEachm w =
  foldl'
    (\acc (_, rows) ->
      Foldable.foldl'
        (\acc' (E.EntityRow _ _ bag) ->
      let sumPos =
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> x + y
              Nothing -> 0
          sumVel =
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> vx + vy
              Nothing -> 0
      in acc' + sumPos + sumVel
        )
        acc
        rows
    )
    0
    (E.matchingArchetypes reqPV 0 w)

forceSplitMatched :: World -> Double
forceSplitMatched w =
  let sumPos =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Pos bag of
                  Just (Pos x y) -> acc' + x + y
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqP 0 w)
      sumVel =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Vel bag of
                  Just (Vel vx vy) -> acc' + vx + vy
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqV 0 w)
  in sumPos + sumVel

forceSplit4Matched :: World -> Double
forceSplit4Matched w =
  let sumPos =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Pos bag of
                  Just (Pos x y) -> acc' + x + y
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqP 0 w)
      sumVel =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Vel bag of
                  Just (Vel vx vy) -> acc' + vx + vy
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqV 0 w)
      sumAcc =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Acc bag of
                  Just (Acc ax ay) -> acc' + ax + ay
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqA 0 w)
      sumHp =
        foldl'
          (\acc (_, rows) ->
            Foldable.foldl'
              (\acc' (E.EntityRow _ _ bag) ->
                case E.bagGet @C @Hp bag of
                  Just (Hp hp) -> acc' + fromIntegral hp
                  Nothing -> acc'
              )
              acc
              rows
          )
          0
          (E.matchingArchetypes reqH 0 w)
  in sumPos + sumVel + sumAcc + sumHp

forceSplitFullScan :: World -> Double
forceSplitFullScan w =
  let sumPos =
        E.foldEntities
          (\_ _ bag acc ->
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> acc + x + y
              Nothing -> acc
          )
          0
          w
      sumVel =
        E.foldEntities
          (\_ _ bag acc ->
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> acc + vx + vy
              Nothing -> acc
          )
          0
          w
  in sumPos + sumVel

data MoveLoop
data MoveLoop1
data MoveLoop2
data MoveLoopSplitP
data MoveLoopSplitV
data MoveLoopSplitA
data MoveLoopSplitH

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

buildWorldPlayers :: Int -> Int -> World
buildWorldPlayers unrelatedCount playerCount =
  let addUnrelated w _ =
        let (_, w1) = E.spawn (Hp 100) w
        in w1
      addPlayer (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0, Vel 1 1) w
        in (i + 1, w1)
      w0 = foldl' addUnrelated E.emptyWorld [1 .. unrelatedCount]
  in snd (foldl' addPlayer (0 :: Int, w0) [1 .. playerCount])

buildWorldSplit :: Int -> Int -> World
buildWorldSplit posCount velCount =
  let addPos (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1) = E.spawn (Vel (fromIntegral i) 1) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, E.emptyWorld) [1 .. posCount]
  in snd (foldl' addVel (0 :: Int, w1) [1 .. velCount])

buildWorldSplit4 :: Int -> Int -> Int -> Int -> World
buildWorldSplit4 posCount velCount accCount hpCount =
  let addPos (i, w) _ =
        let (_, w1) = E.spawn (Pos (fromIntegral i) 0) w
        in (i + 1, w1)
      addVel (i, w) _ =
        let (_, w1) = E.spawn (Vel (fromIntegral i) 1) w
        in (i + 1, w1)
      addAcc (i, w) _ =
        let (_, w1) = E.spawn (Acc (fromIntegral i) 1) w
        in (i + 1, w1)
      addHp (i, w) _ =
        let (_, w1) = E.spawn (Hp (i + 100)) w
        in (i + 1, w1)
      (_, w1) = foldl' addPos (0 :: Int, E.emptyWorld) [1 .. posCount]
      (_, w2) = foldl' addVel (0 :: Int, w1) [1 .. velCount]
      (_, w3) = foldl' addAcc (0 :: Int, w2) [1 .. accCount]
  in snd (foldl' addHp (0 :: Int, w3) [1 .. hpCount])

pPV :: E.Plan C (Pos, Vel)
pPV = E.plan @(Pos, Vel)

pP :: E.Plan C Pos
pP = E.plan @Pos

pV :: E.Plan C Vel
pV = E.plan @Vel

pVA :: E.Plan C (Vel, Acc)
pVA = E.plan @(Vel, Acc)

pA :: E.Plan C Acc
pA = E.plan @Acc

pH :: E.Plan C Hp
pH = E.plan @Hp

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

moveProgEachQ :: Program String ()
moveProgEachQ = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.each qPVQ movePatch
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
    S.eachSet2 @C @Pos @Vel moveSet2
  pure ()

moveProgMStateful :: Program String ()
moveProgMStateful = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoop pPV $ \(Pos x y, Vel vx vy) -> do
      let p = Pos (x + vx) (y + vy)
      S.edit (S.set2 p (Vel vx vy))
  pure ()

mkMoveProgM :: Int -> Program String ()
mkMoveProgM n = S.program (S.handle n) $ do
  _ <- S.await $ computeS $
    S.eachSet2 @C @Pos @Vel moveSet2
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

moveProgMSplit :: Program String ()
moveProgMSplit = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    (S.eachMP @MoveLoopSplitP pP $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p))
    *>
    (S.eachMP @MoveLoopSplitV pV $ \(Vel vx vy) -> do
      let v = Vel (vx + 1) vy
      S.edit (S.set v))
  pure ()

-- Four eachMP loops in one compute block: compiled to one fused kernel pass.
moveProgMSplit4 :: Program String ()
moveProgMSplit4 = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    (S.eachMP @MoveLoopSplitP pP $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p))
    *>
    (S.eachMP @MoveLoopSplitV pV $ \(Vel vx vy) -> do
      let v = Vel (vx + 1) vy
      S.edit (S.set v))
    *>
    (S.eachMP @MoveLoopSplitA pA $ \(Acc ax ay) -> do
      let a = Acc (ax + 1) ay
      S.edit (S.set a))
    *>
    (S.eachMP @MoveLoopSplitH pH $ \(Hp hp) -> do
      S.edit (S.set (Hp (hp + 1))))
  pure ()

moveProgMSplit4P :: Program String ()
moveProgMSplit4P = S.program (S.handle 0) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoopSplitP pP $ \(Pos x y) -> do
      let p = Pos (x + 1) y
      S.edit (S.set p)
  pure ()

moveProgMSplit4V :: Program String ()
moveProgMSplit4V = S.program (S.handle 1) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoopSplitV pV $ \(Vel vx vy) -> do
      let v = Vel (vx + 1) vy
      S.edit (S.set v)
  pure ()

moveProgMSplit4A :: Program String ()
moveProgMSplit4A = S.program (S.handle 2) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoopSplitA pA $ \(Acc ax ay) -> do
      let a = Acc (ax + 1) ay
      S.edit (S.set a)
  pure ()

moveProgMSplit4H :: Program String ()
moveProgMSplit4H = S.program (S.handle 3) $ do
  _ <- S.await $ computeS $
    S.eachMP @MoveLoopSplitH pH $ \(Hp hp) -> do
      S.edit (S.set (Hp (hp + 1)))
  pure ()

graphEach :: Graph String
graphEach = S.graph @String moveProg

graphEachS :: Graph String
graphEachS = S.graph @String moveProgStep

graphEachM :: Graph String
graphEachM = S.graph @String moveProgM

graphEachMStateful :: Graph String
graphEachMStateful = S.graph @String moveProgMStateful

graphEachM4Programs :: Graph String
graphEachM4Programs = S.graphList (map mkMoveProgM [0 .. 3])

graphEachQ :: Graph String
graphEachQ = S.graph @String moveProgEachQ

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

graphEachMSplit :: Graph String
graphEachMSplit = S.graph @String moveProgMSplit

graphEachMSplit4 :: Graph String
graphEachMSplit4 = S.graph @String moveProgMSplit4

graphEachMSplit4Programs :: Graph String
graphEachMSplit4Programs =
  S.graph @String moveProgMSplit4P moveProgMSplit4V moveProgMSplit4A moveProgMSplit4H

runTickSplitFullScan :: World -> World
runTickSplitFullScan w0 =
  let stepPos _ _ bag =
        case E.bagGet @C @Pos bag of
          Just (Pos x y) -> E.bagSetDirect @C @Pos (Pos (x + 1) y) bag
          Nothing -> bag
      stepVel _ _ bag =
        case E.bagGet @C @Vel bag of
          Just (Vel vx vy) -> E.bagSetDirect @C @Vel (Vel (vx + 1) vy) bag
          Nothing -> bag
      w1 = E.mapEntities stepPos w0
  in E.mapEntities stepVel w1

runTickSplit4FullScan :: World -> World
runTickSplit4FullScan w0 =
  let stepPos _ _ bag =
        case E.bagGet @C @Pos bag of
          Just (Pos x y) -> E.bagSetDirect @C @Pos (Pos (x + 1) y) bag
          Nothing -> bag
      stepVel _ _ bag =
        case E.bagGet @C @Vel bag of
          Just (Vel vx vy) -> E.bagSetDirect @C @Vel (Vel (vx + 1) vy) bag
          Nothing -> bag
      stepAcc _ _ bag =
        case E.bagGet @C @Acc bag of
          Just (Acc ax ay) -> E.bagSetDirect @C @Acc (Acc (ax + 1) ay) bag
          Nothing -> bag
      stepHp _ _ bag =
        case E.bagGet @C @Hp bag of
          Just (Hp hp) -> E.bagSetDirect @C @Hp (Hp (hp + 1)) bag
          Nothing -> bag
      w1 = E.mapEntities stepPos w0
      w2 = E.mapEntities stepVel w1
      w3 = E.mapEntities stepAcc w2
  in E.mapEntities stepHp w3

graphEachMPure :: Graph String
graphEachMPure = S.graph @String moveProgMPure

crossoverSizes :: [Int]
crossoverSizes = [10000, 20000, 40000, 80000, 120000]

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
      , let w = buildWorldPlayers 10000 1
        in bench "query-10k+1" $ nf (length . E.runq qPVQ) w
      , let w = buildWorldPlayers 10000 2
        in bench "query-10k+2" $ nf (length . E.runq qPVQ) w
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
            in bench "each-query" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachQ
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
            in bench "eachm-stateful" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMStateful
                in forceEachm w1
              ) w
          , let w = buildAzWorld 10000
            in bench "eachm-aztecs" $ nf (\w0 ->
                let w1 = runAzTickEachm w0
                in forceAzEachm w1
              ) w
          , let w = buildWorld 10000
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in forceEachm w1
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
      , bgroup "10k+1"
          [ let w = buildWorldPlayers 10000 1
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in forceEachm w1
              ) w
          , let w = buildAzWorldPlayers 10000 1
            in bench "eachm-aztecs" $ nf (\w0 ->
                let w1 = runAzTickEachm w0
                in forceAzEachm w1
              ) w
          , let w = buildWorldPlayers 10000 1
            in bench "each-query" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachQ
                in length (E.entities w1)
              ) w
          , let w = buildWorldPlayers 10000 1
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in forceEachm w1
              ) w
          ]
      , bgroup "10k+2"
          [ let w = buildWorldPlayers 10000 2
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                in forceEachm w1
              ) w
          , let w = buildWorldPlayers 10000 2
            in bench "eachm-pure" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMPure
                in forceEachm w1
              ) w
          ]
      , bgroup "5k+5k+5k+5k"
          [ let w = buildWorldSplit4 5000 5000 5000 5000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSplit4
                in forceSplit4Matched w1
              ) w
          , let w = buildWorldSplit4 5000 5000 5000 5000
            in bench "eachm-fullscan" $ nf (\w0 ->
                let w1 = runTickSplit4FullScan w0
                in forceSplit4Matched w1
              ) w
          , let w = buildAzWorldSplit4 5000 5000 5000 5000
            in bench "eachm-aztecs" $ nf (\w0 ->
                let w1 = runAzTickSplit4 w0
                in forceAzSplit4 w1
              ) w
          ]
      , bgroup "5k/5k/5k/5k"
          [ let w = buildWorldSplit4 5000 5000 5000 5000
            in bench "eachm" $ nf (\w0 ->
                let (w1, _, _) = S.run 0.016 w0 [] graphEachMSplit4Programs
                in forceSplit4Matched w1
              ) w
          , let w = buildAzWorldSplit4 5000 5000 5000 5000
            in bench "eachm-aztecs" $ nf (\w0 ->
                let w1 = runAzTickSplit4 w0
                in forceAzSplit4 w1
              ) w
          ]
      , bgroup "crossover"
          ( concatMap
              (\n ->
                [ let w = buildWorld n
                  in bench (show n <> "/eachm") $ nf (\w0 ->
                      let (w1, _, _) = S.run 0.016 w0 [] graphEachM
                      in forceEachm w1
                    ) w
                , let w = buildAzWorld n
                  in bench (show n <> "/eachm-aztecs") $ nf (\w0 ->
                      let w1 = runAzTickEachm w0
                      in forceAzEachm w1
                    ) w
                ]
              )
              crossoverSizes
          )
      , bgroup "10k-warm"
          [ let w = buildWorld 10000
            in bench "each" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEach
                in length (E.entities w1)
              ) w
          , let w = buildWorld 10000
            in bench "each-query" $ nf (\w0 ->
                let w1 = runWarmTick 0.016 w0 graphEachQ
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
                in forceEachm w1
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
