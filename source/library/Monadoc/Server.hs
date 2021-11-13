module Monadoc.Server where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

serve :: Warp.Port -> IO ()
serve port = Warp.run port application

application :: Wai.Application
application _ respond = respond $ Wai.responseLBS Http.notFound404 [] LazyByteString.empty
