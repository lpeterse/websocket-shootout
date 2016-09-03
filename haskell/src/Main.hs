{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent as C
-- import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TC
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.WebSockets

broadcastThread :: TC.TChan ByteString -> Connection -> IO ()
broadcastThread bc conn = forever $ do
  t <- atomically $ TC.readTChan bc
  sendTextData conn t

wtf :: Connection -> IO ()
wtf conn =
  sendTextData conn ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

bidiHandler :: TC.TChan ByteString -> Connection -> IO ()
bidiHandler inp conn = do
  outp <- atomically $ TC.dupTChan inp
  _ <- C.forkIO (broadcastThread outp conn)
  forever $ do
    msg <- receiveDataMessage conn
    case msg of
      Text t -> do
        let Just v = decode t :: Maybe Value
            Just payload = v ^? key "payload"
            eventType = v ^? key "type" . _String
        case eventType of
          Just "echo" -> sendTextData conn (mkPayload "echo" payload)
          Just "broadcast" -> do
            atomically $ TC.writeTChan inp (mkPayload "broadcast" payload)
            sendTextData conn (mkPayload "broadcastResult" payload)
          _ -> wtf conn
      _ -> do
        wtf conn

wsApp :: TC.TChan ByteString -> ServerApp
wsApp inp pending = do
  conn <- acceptRequest pending
  bidiHandler inp conn

main :: IO ()
main = do
  tchan <- atomically $ TC.newBroadcastTChan
  runServer "127.0.0.1" 3000 (wsApp tchan)
