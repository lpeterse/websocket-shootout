{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- import Data.ByteString.Lazy (ByteString)
import Data.ByteString (ByteString)
import Data.Text (Text)
-- import Network.WebSockets

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
-- import System.IO (hSetBuffering, BufferMode(NoBuffering))
-- import Data.Char

serverHandshake :: ByteString
serverHandshake = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://localhost\r\n\
    \WebSocket-Location: ws://localhost:3000/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n"

main :: IO ()
main = NS.withSocketsDo $ do
  addrinfos <- NS.getAddrInfo
               (Just (NS.defaultHints
                      {NS.addrFlags = [NS.AI_PASSIVE]}))
               Nothing (Just "3000")
  let serveraddr = head addrinfos
  sock <- NS.socket (NS.addrFamily serveraddr)
                     NS.Stream NS.defaultProtocol
  NS.bind sock (NS.addrAddress serveraddr)
  NS.listen sock 5
  (inp, _) <- newChan
  _ <- acceptLoop inp sock
  NS.close sock
  return ()

wtf :: ByteString
wtf =
  "<img src=\"http://bit.ly/1kmRC7Q\" />"

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = BSL.toStrict $ encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

acceptLoop :: InChan ByteString -> NS.Socket -> IO ()
acceptLoop inp socket = forever $ do
  (s,_) <- NS.accept socket
  _ <- NSB.send s serverHandshake
  C.forkIO (listenLoop inp s)

listenLoop :: InChan ByteString -> NS.Socket -> IO ()
listenLoop inp s = do
  outp <- dupChan inp
  m <- M.newEmptyMVar
  _ <- C.forkIO (broadcastThread outp m)
  forever $ do
    msg <- NSB.recv s 10000
    print msg

broadcastThread :: OutChan ByteString -> M.MVar NS.Socket -> IO ()
broadcastThread bc m = forever $ do
  t <- readChan bc
  s <- M.takeMVar m
  NSB.send s t
  M.putMVar m s

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

-- -- main :: IO ()
-- -- main = do
-- --   (inp, _) <- newChan
-- --   runServer "127.0.0.1" 3000 (wsApp inp)
--   -- run 3000 (WaiWS.websocketsOr defaultConnectionOptions (wsApp inp) undefined)
