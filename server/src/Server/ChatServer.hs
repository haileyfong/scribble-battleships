{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Server.ChatServer (server) where

import Prelude
import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import           Data.Text.Encoding        (encodeUtf8)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import           Data.Aeson          (withObject, encode, decode, decodeStrict, toJSON, parseJSON, tagSingleConstructors, defaultOptions, sumEncoding, (.=), (.:), genericParseJSON, object, SumEncoding(..), ToJSON, FromJSON)
import           GHC.Generics        (Generic)
import           Data.String         (unlines)
import           Data.String.Conversions (convertString)
import           Data.Maybe          (fromJust)
import           Data.Text.Lazy      (fromStrict)
import           Data.Aeson.Lens     (key, _String)
import           Control.Lens        ((^?), (&), Prism')

argonautOptions = defaultOptions {tagSingleConstructors = True, sumEncoding = TaggedObject "tag" "values"}


server :: IO ()
server = do
  state <- Concurrent.newMVar []
  Warp.run 9160 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

data Messages = Messages String
  deriving (Generic, Show)
instance ToJSON Messages where
  toJSON (Messages res) =
      object ["tag" .= ("Messages" :: Text.Text), "values" .= [res]]

data Message = Message {values :: [String], tag :: String}
  deriving (Generic, Show)
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> 
		Message <$> v .: "values" <*> v .: "tag"

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

something :: [String] -> WS.Connection -> ClientId -> Concurrent.MVar State -> IO()
something messages conn clientId stateRef = do
	msg <- WS.receiveData conn :: IO Text.Text
	case msg ^? key "tag" . _String :: Maybe Text.Text of
	  Just tag | tag == "Message" -> do
                     newMesg <- spread $ fromJust ((decode $ convertString msg) :: Maybe Message)
                     putStrLn(newMesg !! 0)
                     something (messages++newMesg) conn clientId stateRef
	  		     | tag == "RcvMessage" -> do
					           putStrLn  ("received") >> broadcast clientId stateRef messages
					           something messages conn clientId stateRef

    where
		spread (Message msg tag) = do
			broadcast clientId stateRef (messages++msg)
			return msg :: IO [String]

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> [String] -> IO ()
listen conn clientId stateRef messages = (WS.receiveData conn :: IO Text.Text) >> (Monad.forever $ do
  something messages conn clientId stateRef)
--   WS.receiveData conn >>= broadcast clientId stateRef messages)

-- send :: Message -> WS.Connection -> IO()
-- send (Message msg tag) conn = do 
-- 	_ <- Messages
-- 	WS.sendTextData conn $ encode $ Messages $ unlines(msg)

broadcast :: ClientId -> Concurrent.MVar State -> [String] -> IO ()
broadcast clientId stateRef msgs = do
--   putStrLn(Text.unpack(msg))
  clients <- Concurrent.readMVar stateRef
  Monad.forM_ clients $ \(_, conn) ->
    WS.sendTextData conn $ encode $ Messages $ unlines(msgs)

-- broadcast :: ClientId -> Concurrent.MVar State -> [String] -> Text.Text -> IO ()
-- broadcast clientId stateRef msg = do
--   putStrLn(Text.unpack(msg))
--   clients <- Concurrent.readMVar stateRef
--   case msg ^? key "tag" . _String :: Maybe Text of
-- 	  Just tag | tag == "Message" -> add $ fromJust (decode $ convertString action)
-- 	  		   | tag == "RcvMessage" -> 
--   Monad.forM_ clients $ \(_, conn) ->
--     send (fromJust (decode(convertString(msg)) :: Maybe Message)) conn

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef [])
    (disconnectClient clientId stateRef)
