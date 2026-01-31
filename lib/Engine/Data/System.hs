module Engine.Data.System
  ( Patch(..)
  , patch
  , Sys
  , WorldSys
  , chain
  , stepw
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category(..))
import Engine.Data.ECS (World)
import Engine.Data.FRP (DTime, Step(..))

newtype Patch = Patch { apply :: World -> World }

patch :: (World -> World) -> Patch
patch = Patch

instance Semigroup Patch where
  Patch f <> Patch g = Patch (g . f)

instance Monoid Patch where
  mempty = Patch id

type Sys a b = Step (World, a) (World, b)
type WorldSys = Step World World

chain :: [WorldSys] -> WorldSys
chain = foldr (.) id

stepw :: (DTime -> World -> World) -> WorldSys
stepw f = Step $ \dt w -> (f dt w, stepw f)
