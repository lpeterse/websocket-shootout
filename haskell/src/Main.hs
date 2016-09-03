{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Lifted as LC
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
-- import Data.ByteString (ByteString)
-- import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Yesod.Core
import Yesod.WebSockets
import Control.Monad.Trans.Reader
import Data.Conduit

data App =
  App (InChan ByteString)

instance Yesod App

mkYesod "App" [parseRoutes|
/ws HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
    webSockets wsApp
    defaultLayout $ do
        [whamlet|
            <div #output>
              hi
        |]

broadcastThread :: OutChan ByteString -> WebSocketsT Handler ()
broadcastThread bc = forever $ do
  t <- liftIO $ readChan bc
  sendTextData t

-- wtf :: Connection -> IO ()
wtf =
  sendTextData ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

-- chatApp :: WebSocketsT Handler ()
-- chatApp = do
--     sendTextData ("Welcome to the chat server, please enter your name." :: Text)
--     name <- receiveData
--     sendTextData $ "Welcome, " <> name
--     App writeChan <- getYesod
--     readChan <- atomically $ do
--         writeTChan writeChan $ name <> " has joined the chat"
--         dupTChan writeChan
--     race_
--         (forever $ atomically (readTChan readChan) >>= sendTextData)
--         (sourceWS $$ mapM_C (\msg ->
--             atomically $ writeTChan writeChan $ name <> ": " <> msg))

-- bidiHandler :: InChan ByteString -> Connection -> IO ()
-- bidiHandler inp conn = do
--   outp <- dupChan inp
--   _ <- C.forkIO (broadcastThread outp conn)
--   forever $ do
--     msg <- receiveDataMessage conn
--     case msg of
--       Text t -> do
--         let Just v = decode t :: Maybe Value
--             Just payload = v ^? key "payload"
--             eventType = v ^? key "type" . _String
--         case eventType of
--           Just "echo" -> sendTextData conn (mkPayload "echo" payload)
--           Just "broadcast" -> do
--             writeChan inp (mkPayload "broadcast" payload)
--             sendTextData conn (mkPayload "broadcastResult" payload)
--           _ -> wtf conn
--       _ -> do
--         wtf conn

-- wsApp :: InChan ByteString -> ServerApp
-- wsApp inp pending = do
--   conn <- acceptRequest pending
--   bidiHandler inp conn

wsApp :: WebSocketsT Handler ()
wsApp = do
  App inp <- getYesod
  outp <- liftIO $ dupChan inp
  _ <- LC.fork (broadcastThread outp)
  forever $ do
    msg <- receiveDataMessageE
    case msg of
      Right (WS.Text t) -> do
        let Just v = decode t :: Maybe Value
            Just payload = v ^? key "payload"
            eventType = v ^? key "type" . _String
        case eventType of
          Just "echo" -> sendTextData (mkPayload "echo" payload)
          Just "broadcast" -> do
            liftIO $ writeChan inp (mkPayload "broadcast" payload)
            sendTextData (mkPayload "broadcastResult" payload)
          _ -> wtf
      _ -> do
        wtf

main :: IO ()
main = do
    -- chan <- atomically newBroadcastTChan
    (inp, _) <- newChan
    warp 3000 $ App inp

-- main :: IO ()
-- main = do
--   (inp, _) <- newChan
--   runServer "127.0.0.1" 3000 (wsApp inp)
  -- run 3000 (WaiWS.websocketsOr defaultConnectionOptions (wsApp inp) undefined)
