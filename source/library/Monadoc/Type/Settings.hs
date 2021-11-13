module Monadoc.Type.Settings where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.InvalidPort as InvalidPort
import qualified Monadoc.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Settings = Settings
  { help :: Bool,
    port :: Warp.Port,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Settings
initial = Settings {help = False, port = 8080, version = False}

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Settings
fromFlags = Monad.foldM applyFlag initial

applyFlag :: Exception.MonadThrow m => Settings -> Flag.Flag -> m Settings
applyFlag settings flag = case flag of
  Flag.Help -> pure settings {help = True}
  Flag.Port s -> case Read.readMaybe s of
    Nothing -> Exception.throwM $ InvalidPort.InvalidPort s
    Just p -> pure settings {port = p}
  Flag.Version -> pure settings {version = True}
