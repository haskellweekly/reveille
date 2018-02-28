module Reveille.Internal.Server where

import Data.Function ((&))

import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Either as Either
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Reveille.Internal.Application as Application
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Unicode as Unicode
import qualified Text.Printf as Printf

startServer :: Stm.TVar Database.Database -> IO ()
startServer database =
  Warp.runSettings serverSettings (Application.application database)

serverSettings :: Warp.Settings
serverSettings =
  Warp.defaultSettings
    & Warp.setBeforeMainLoop serverBeforeMainLoop
    & Warp.setLogger serverLogger
    & Warp.setOnExceptionResponse serverExceptionResponse
    & Warp.setPort serverPort
    & Warp.setServerName serverName

serverBeforeMainLoop :: IO ()
serverBeforeMainLoop = putStrLn "Starting server ..."

serverLogger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
serverLogger request status _ = Printf.printf
  "%s %s%s %d\n"
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.requestMethod request)))
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.rawPathInfo request)))
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.rawQueryString request)))
  (Http.statusCode status)

serverExceptionResponse :: Exception.SomeException -> Wai.Response
serverExceptionResponse _ =
  Wai.responseLBS Http.internalServerError500 [] LazyBytes.empty

serverPort :: Warp.Port
serverPort = 8080

serverName :: Bytes.ByteString
serverName = Bytes.empty
