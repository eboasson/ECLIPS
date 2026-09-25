{-# LANGUAGE CPP #-}

module PublicStep11Opaque where

#if defined(GENERATOR_SEED_CONSTRUCTOR)
import Eclips.Herald.IdGenerator (GeneratorSeed (..))

forgedGeneratorSeed :: GeneratorSeed
forgedGeneratorSeed = GeneratorSeed undefined
#elif defined(GENERATOR_SEED_BYTES)
import Eclips.Herald.IdGenerator (GeneratorSeed, generatorSeedBytes)

exposedGeneratorSeedBytes :: GeneratorSeed -> ()
exposedGeneratorSeedBytes seed = generatorSeedBytes seed `seq` ()
#elif defined(ID_GENERATOR_STATE)
import Eclips.Herald.IdGenerator.State ()
#elif defined(CONTROLLED_RESERVATION_STATE)
import Eclips.Herald.Controlled.State (ReservationWitness)

privateReservationStateMustRemainHidden :: Maybe ReservationWitness
privateReservationStateMustRemainHidden = Nothing
#elif defined(ENVIRONMENT_ACCESS_CONSTRUCTOR)
import Eclips.Application.Types.Access (EnvironmentAccess (..))

forgedEnvironmentAccess :: EnvironmentAccess
forgedEnvironmentAccess = EnvironmentAccess undefined
#elif defined(STARTUP_ENVIRONMENT_COERCION)
import Data.Coerce (coerce)
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    EnvironmentAccess,
  )

startupIsNotEnvironment :: ApplicationStartupAccess -> EnvironmentAccess
startupIsNotEnvironment = coerce
#else
#error "select one Step-11 opacity fixture"
#endif
