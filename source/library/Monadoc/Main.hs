module Monadoc.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Version as Version
import qualified Monadoc.Server as Server
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Settings as Settings
import qualified Paths_monadoc as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  let ((warnings, errors), flags) = getFlags arguments
  mapM_ (IO.hPutStrLn IO.stderr) warnings
  mapM_ (IO.hPutStr IO.stderr) errors
  Monad.unless (null errors) Exit.exitFailure

  withSettings name flags $ \settings -> do
    Server.serve $ Settings.port settings

getFlags :: [String] -> (([String], [String]), [Flag.Flag])
getFlags arguments =
  let
    (flags, xs, ys, errors) = Console.getOpt' Console.Permute options arguments
    warnings =
      fmap (mappend "unexpected argument " . quote . newLine) xs
        <> fmap (mappend "unrecognized option " . quote . newLine) ys
  in ((warnings, errors), flags)

options :: [Console.OptDescr Flag.Flag]
options =
  [ Console.Option
    ['h', '?']
    ["help"]
    (Console.NoArg Flag.Help)
    "shows this help message and exits"
  , Console.Option
    ['v']
    ["version"]
    (Console.NoArg Flag.Version)
    "outputs the version number and exits"
  ]

newLine :: String -> String
newLine = snoc '\n'

snoc :: a -> [a] -> [a]
snoc x xs = xs <> [x]

quote :: String -> String
quote = (:) '`' . snoc '\''

withSettings :: String -> [Flag.Flag] -> (Settings.Settings -> IO ()) -> IO ()
withSettings name flags callback = do
  settings <- either Exception.throwM pure $ Settings.fromFlags flags
  if Settings.help settings
    then putStr $ Console.usageInfo (name <> " version " <> version) options
    else if Settings.version settings
      then putStrLn version
      else callback settings

version :: String
version = Version.showVersion Package.version
