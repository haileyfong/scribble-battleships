module Scribble.Protocol.ChatServer where

import Scribble.FSM

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)

data ConnectUser = ConnectUser String
derive instance genericConnectUser :: Generic ConnectUser _
instance encodeJsonConnectUser :: EncodeJson ConnectUser where
  encodeJson = genericEncodeJson
instance decodeJsonConnectUser :: DecodeJson ConnectUser where
  decodeJson = genericDecodeJson

data Message = Message String
derive instance genericMessage :: Generic Message _
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson

data RcvMessage = RcvMessage
derive instance genericRcvMessage :: Generic RcvMessage _
instance encodeJsonRcvMessage :: EncodeJson RcvMessage where
  encodeJson = genericEncodeJson
instance decodeJsonRcvMessage :: DecodeJson RcvMessage where
  decodeJson = genericDecodeJson

data Messages = Messages String
derive instance genericMessages :: Generic Messages _
instance encodeJsonMessages :: EncodeJson Messages where
  encodeJson = genericEncodeJson
instance decodeJsonMessages :: DecodeJson Messages where
  decodeJson = genericDecodeJson

data Quit = Quit
derive instance genericQuit :: Generic Quit _
instance encodeJsonQuit :: EncodeJson Quit where
  encodeJson = genericEncodeJson
instance decodeJsonQuit :: DecodeJson Quit where
  decodeJson = genericDecodeJson

foreign import data ChatServer :: Protocol

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S11 :: Type
foreign import data S11Connected :: Type
foreign import data S13 :: Type
foreign import data S13Send :: Type
foreign import data S13Receive :: Type
foreign import data S13Quit :: Type
foreign import data S14 :: Type
foreign import data S15 :: Type
foreign import data S16 :: Type
foreign import data S12 :: Type
foreign import data S17 :: Type

instance initialClient :: Initial Client S11
instance terminalClient :: Terminal Client S17
instance connectS11 :: Connect Client Server S11 S11Connected
instance sendS11 :: Send Server S11Connected S13 ConnectUser
instance sendS13Send :: Send Server S13Send S14 Message
instance sendS13Receive :: Send Server S13Receive S15 RcvMessage
instance sendS13Quit :: Send Server S13Quit S16 Quit
instance selectS13 :: Select Server S13 ("send" :: S13Send ,"receive" :: S13Receive ,"quit" :: S13Quit)
instance receiveS14 :: Receive Server S14 S13 Messages
instance receiveS15 :: Receive Server S15 S13 Messages
instance disconnectS16 :: Disconnect Client Server S16 S17

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S26 :: Type
foreign import data S28 :: Type
foreign import data S28Send :: Type
foreign import data S28Receive :: Type
foreign import data S28Quit :: Type
foreign import data S29 :: Type
foreign import data S30 :: Type
foreign import data S31 :: Type
foreign import data S27 :: Type

instance initialServer :: Initial Server S26
instance terminalServer :: Terminal Server S27
instance acceptS26 :: Accept Server Client S26 S28
instance receiveS28Send :: Receive Client S28Send S29 Message
instance receiveS28Receive :: Receive Client S28Receive S30 RcvMessage
instance receiveS28Quit :: Receive Client S28Quit S31 Quit
instance branchS28 :: Branch Server Client S28 ("send" :: S28Send ,"receive" :: S28Receive ,"connect" :: S26 ,"quit" :: S28Quit)
instance sendS29 :: Send Client S29 S28 Message
instance sendS30 :: Send Client S30 S28 Messages
instance disconnectS31 :: Disconnect Server Client S31 S27


