module Monadoc.Type.Flag where

data Flag
  = Help
  | Port String
  | Version
  deriving (Eq, Show)
