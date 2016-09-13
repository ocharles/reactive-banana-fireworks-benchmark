{-# LANGUAGE RecordWildCards #-}

module Physics.Particle where

import Linear
import Linear.Affine
import Reactive.Banana

--------------------------------------------------------------------------------
-- | The configuration of a particle.
data ParticleProperties = ParticleProperties
  { particleMass :: Behavior Double
    -- ^ The mass of a particle at any point in time.
  , particleInitialPosition :: Point V2 Double
    -- ^ The initial position of a particle, when it is created.
  , particleInitialVelocity :: V2 Double
    -- ^ The initial velocity of a particle, when it is created.
  , particleGravity :: Behavior (V2 Double)
    -- ^ The force of gravity acting on a particle at any point in time.
  }

-- | A simulated particle.
data Particle = Particle
  { particlePosition :: Behavior (Point V2 Double)
    -- ^ The position of a particle at any point in time
  }

newParticle
  :: (MonadMoment m)
  => Event Double -> ParticleProperties -> m Particle
newParticle stepPhysics ParticleProperties {..} = do
  let particleInvMass = recip <$> particleMass
  particleVelocity <-
    accumB
      particleInitialVelocity
      ((^+^) <$>
       ((^*) <$> ((*^) <$> particleInvMass <*> particleGravity) <@> stepPhysics))
  particlePosition <-
    accumB
      particleInitialPosition
      (flip (.+^) <$> ((^*) <$> particleVelocity <@> stepPhysics))
  return Particle {..}
