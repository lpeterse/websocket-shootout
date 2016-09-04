{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Maybe
import Data.Typeable
import Control.Exception
import Control.Concurrent.MVar

import qualified Control.Concurrent as C
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Control.Concurrent.Async (race_)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Char as DC
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.WebSockets
import Text.RawString.QQ

amendTest :: Maybe Value
amendTest = decode $ [r|
{"type":"broadcast","payload":{"foo": "bar"}}
|]

amendBroadcast :: Value -> Value
amendBroadcast v =
  v & key "type" . _String .~ "broadcastResult"

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

bidiHandler :: Broadcast ByteString -> Connection -> IO ()
bidiHandler bc conn =
  handleInput `race_` handleOutput
  where
    handleOutput = do
      bl <- listen bc
      forever $ do
        msg <- accept bl
        sendTextData conn msg
    handleInput = forever $ do
      msg <- receiveDataMessage conn
      case msg of
        Text t -> do
          let Just v = decode t :: Maybe Value
              Just payload = v ^? key "payload"
              eventType = v ^? key "type" . _String
          case eventType of
            Just "echo" -> sendTextData conn (mkPayload "echo" payload)
            Just "broadcast" -> do
              broadcast bc (mkPayload "broadcast" payload)
              sendTextData conn (mkPayload "broadcastResult" payload)
            _ -> error "client violated protocol!"
        _ -> error "client sent binary message!"

wsApp :: Broadcast ByteString -> ServerApp
wsApp bc pending = do
  conn <- acceptRequest pending
  bidiHandler bc conn

main :: IO ()
main = do
  bc <- newBroadcast
  runServer "127.0.0.1" 3000 (wsApp bc)

-- Broadcast implementations

data Tail a
   = Tail (MVar (Maybe (Tail a, a)))

newtype Broadcast a
      = Broadcast (MVar (Tail a))

newtype BroadcastListener a
      = BroadcastListener (MVar (Tail a))

data BroadcastTerminatedException = BroadcastTerminatedException
  deriving (Show, Typeable)

instance Exception BroadcastTerminatedException

newBroadcast :: IO (Broadcast a)
newBroadcast  = Broadcast <$> ( newMVar =<< Tail <$> newEmptyMVar )

broadcast :: Broadcast a -> a -> IO ()
broadcast (Broadcast mt) a = modifyMVar_ mt $ \(Tail currentTail)-> do
    newTail <- Tail <$> newEmptyMVar          -- a new (unresolved) tail
    putMVar currentTail (Just (newTail, a))   -- resolve the current head
    pure newTail                              -- new tail replaces current

terminate :: Broadcast a -> IO ()
terminate (Broadcast mt) = modifyMVar_ mt $ \(Tail currentTail)-> do
    putMVar currentTail Nothing               -- resolve the current tail
    pure (Tail currentTail)                   -- new tail replaces current

listen :: Broadcast a -> IO (BroadcastListener a)
listen (Broadcast mt) = BroadcastListener <$> ( newMVar =<< readMVar mt )

tryAccept ::  BroadcastListener a -> IO (Maybe a)
tryAccept (BroadcastListener mt) = modifyMVar mt $ \(Tail oldTail)-> do
  mat <- readMVar oldTail
  case mat of
    Nothing           -> pure (Tail oldTail, Nothing)
    Just (newTail, a) -> pure (newTail, Just a)

accept :: BroadcastListener a -> IO a
accept bl = do
  ma <- tryAccept bl
  case ma of
    Nothing -> throwIO BroadcastTerminatedException
    Just a  -> pure a
