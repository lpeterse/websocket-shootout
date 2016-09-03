{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Control.Concurrent as C
import Control.Concurrent.BroadcastChan
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Char as DC
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Text.RawString.QQ

type OutChan = BroadcastChan Out ByteString
type InChan = BroadcastChan In ByteString

amendTest :: Maybe Value
amendTest = decode $ [r|
{"type":"broadcast","payload":{"foo": "bar"}}
|]

amendBroadcast :: Value -> Value
amendBroadcast v =
  v & key "type" . _String .~ "broadcastResult"

broadcastThread :: OutChan -> Connection -> IO ()
broadcastThread bc conn = forever $ do
  t <- readBChan bc
  sendTextData conn t

wtf conn =
  sendTextData conn ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

bidiHandler :: InChan -> Connection -> IO ()
bidiHandler inp conn = do
  outp <- newBChanListener inp
  _ <- C.forkIO (broadcastThread outp conn)
  forever $ do
    msg <- receiveDataMessage conn
    case msg of
      Text t -> do
        let Just payload = t ^? key "payload"
        case t ^? key "type" . _String of
          Just "echo" -> sendTextData conn (mkPayload "echo" payload)
          Just "broadcast" -> do
            writeBChan inp (mkPayload "broadcast" payload)
            sendTextData conn (mkPayload "broadcastResult" payload)
          _ -> wtf conn
      _ -> do
        wtf conn

wsApp :: InChan -> ServerApp
wsApp inp pending = do
  conn <- acceptRequest pending
  bidiHandler inp conn

main :: IO ()
main = do
  inp <- newBroadcastChan
  runServer "127.0.0.1" 3000 (wsApp inp)
