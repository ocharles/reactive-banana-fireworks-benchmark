{-# LANGUAGE OverloadedStrings, RecordWildCards, RecursiveDo #-}

module Main where

import Control.Monad (unless, void)
import Criterion.Main
import Data.Foldable (for_)
import Data.IORef
import Linear
import Linear.Affine
import Physics.Particle
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Random

--------------------------------------------------------------------------------
data Spark = Spark
  { sparkParticle :: Particle
  , sparkIntensity :: Behavior Double
  , sparkFaded :: Event ()
  }

newSpark :: (MonadIO m, MonadMoment m) => Event Double -> m Spark
newSpark stepPhysics = do
  particleMass <- fmap pure (liftIO (randomRIO (0.1, 1)))
  particleInitialVelocity <-
    liftIO (V2 <$> randomRIO (-500, 500) <*> randomRIO (10, (-1000)))
  sparkParticle <-
    newParticle
      stepPhysics
      ParticleProperties
      { particleMass = particleMass
      , particleInitialPosition = P (V2 400 400)
      , particleInitialVelocity = particleInitialVelocity
      , particleGravity = pure (V2 0 15)
      }
  maxAge <- liftIO (randomRIO (0.1, 2))
  age <- accumB 0 ((+) <$> stepPhysics)
  let sparkIntensity = (1 - ) <$> ((/ maxAge) <$> age)
      sparkFaded = void (whenE ((< 0) <$> ((maxAge - ) <$> age)) stepPhysics)
  return Spark {..}

data SparkData = SparkData
  { sdLocation :: !(Point V2 Double)
  , sdIntensity :: !(Double)
  }

--------------------------------------------------------------------------------
fireworks :: Event Double -> MomentIO (Behavior [SparkData])
fireworks stepPhysics =
  mdo sparkCreated <- execute (newSpark stepPhysics <$ stepPhysics)
      sparkSetChanged <- accumE [] (unions [(:) <$> sparkCreated, sparkRemove])
      sparkRemove <-
        switchE (fmap (unions . zipWith deleteOne [0 ..]) sparkSetChanged)
      switchB (pure []) ((sequenceA . fmap observeSpark) <$> sparkSetChanged)
  where
    observeSpark Spark {..} =
      SparkData <$> particlePosition sparkParticle <*> sparkIntensity
    deleteOne i s = deleteAt i <$ sparkFaded s

--------------------------------------------------------------------------------
-- | Delete the ith element from a list. If i is out of bounds, the list is
-- returned unchanged.
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i (x : xs) | i == 0 = xs
                    | otherwise = x : deleteAt (i - 1) xs

--------------------------------------------------------------------------------
main :: IO ()
main =
  defaultMain
    [ bench "Sparks" $
      whnfIO $ do
        (tickAh, onTick) <- newAddHandler
        out <- newIORef []
        compile
          (do tick <- fromAddHandler tickAh
              sparks <- fireworks tick
              reactimate (writeIORef out <$> sparks <@ tick)) >>=
          actuate
        for_ (replicate 20 1e-3) onTick
        readIORef out
    ]
