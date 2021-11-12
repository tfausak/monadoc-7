module Monadoc.Type.Settings where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Flag as Flag

data Settings = Settings
  { help :: Bool,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Settings
initial = Settings {help = False, version = False}

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Settings
fromFlags = Monad.foldM applyFlag initial

applyFlag :: Exception.MonadThrow m => Settings -> Flag.Flag -> m Settings
applyFlag settings flag = case flag of
  Flag.Help -> pure settings {help = True}
  Flag.Version -> pure settings {version = True}
