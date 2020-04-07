module Scribble.Protocol.ChatServer.ChatServer where

import Scribble.FSM
import Prim.TypeError (class Fail, Text)
import Data.Void (Void)
import Data.Tuple (Tuple)

-- From purescript-argonaut-codecs
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json) -- From purescript-argonaut-core
import Data.Generic.Rep (class Generic) -- From purescript-generics-rep
-- From purescript-argonaut-generic
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJsonWith)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic.Rep (Encoding)


jsonEncoding :: Encoding
jsonEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: true
  }

data Recv = Recv
derive instance genericRecv :: Generic Recv _
instance encodeJsonRecv :: EncodeJson Recv where
  encodeJson = genericEncodeJsonWith jsonEncoding
instance decodeJsonRecv :: DecodeJson Recv where
  decodeJson = genericDecodeJsonWith jsonEncoding
data Messages = Messages String
derive instance genericMessages :: Generic Messages _
instance encodeJsonMessages :: EncodeJson Messages where
  encodeJson = genericEncodeJsonWith jsonEncoding
instance decodeJsonMessages :: DecodeJson Messages where
  decodeJson = genericDecodeJsonWith jsonEncoding
data Message = Message String
derive instance genericMessage :: Generic Message _
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJsonWith jsonEncoding
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJsonWith jsonEncoding
data Connect = Connect
derive instance genericConnect :: Generic Connect _
instance encodeJsonConnect :: EncodeJson Connect where
  encodeJson = genericEncodeJsonWith jsonEncoding
instance decodeJsonConnect :: DecodeJson Connect where
  decodeJson = genericDecodeJsonWith jsonEncoding
data Quit = Quit
derive instance genericQuit :: Generic Quit _
instance encodeJsonQuit :: EncodeJson Quit where
  encodeJson = genericEncodeJsonWith jsonEncoding
instance decodeJsonQuit :: DecodeJson Quit where
  decodeJson = genericDecodeJsonWith jsonEncoding

foreign import data ChatServer :: Protocol

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S11 :: Type
foreign import data S11Connected :: Type
foreign import data S13 :: Type
foreign import data S13Message :: Type
foreign import data S13Recv :: Type
foreign import data S13Quit :: Type
foreign import data S14 :: Type
foreign import data S15 :: Type
foreign import data S16 :: Type
foreign import data S12 :: Type

instance initialClient :: Initial Client S11
instance terminalClient :: Terminal Client S12
instance connectS11 :: Connect Client Server S11 S11Connected
instance sendS11 :: Send Server S11Connected S13 Connect
-- else instance sendS11Fail1 :: Fail (Text "Sending to Server from S11 to S13 is invalid") => Send  Server S11 S13 Connect
else instance sendS11Fail2 :: Fail (Text "Sending to Server from S13 to S13 is invalid") => Send  Server S13 S13 Connect
-- else instance sendS11Fail3 :: Fail (Text "Sending to Server from S13Message to S13 is invalid") => Send  Server S13Message S13 Connect
-- else instance sendS11Fail4 :: Fail (Text "Sending to Server from S13Recv to S13 is invalid") => Send  Server S13Recv S13 Connect
-- else instance sendS11Fail5 :: Fail (Text "Sending to Server from S13Quit to S13 is invalid") => Send  Server S13Quit S13 Connect
-- else instance sendS11Fail6 :: Fail (Text "Sending to Server from S14 to S13 is invalid") => Send  Server S14 S13 Connect
-- else instance sendS11Fail7 :: Fail (Text "Sending to Server from S15 to S13 is invalid") => Send  Server S15 S13 Connect
-- else instance sendS11Fail8 :: Fail (Text "Sending to Server from S16 to S13 is invalid") => Send  Server S16 S13 Connect
-- else instance sendS11Fail9 :: Fail (Text "Sending to Server from S12 to S13 is invalid") => Send  Server S12 S13 Connect
instance sendS13Message :: Send Server S13Message S14 Message
-- else instance sendS13MessageFail1 :: Fail (Text "Sending to Server from S11 to S14 is invalid") => Send  Server S11 S14 Message
-- else instance sendS13MessageFail2 :: Fail (Text "Sending to Server from S11Connected to S14 is invalid") => Send  Server S11Connected S14 Message
-- else instance sendS13MessageFail3 :: Fail (Text "Sending to Server from S13 to S14 is invalid") => Send  Server S13 S14 Message
-- else instance sendS13MessageFail4 :: Fail (Text "Sending to Server from S13Recv to S14 is invalid") => Send  Server S13Recv S14 Message
-- else instance sendS13MessageFail5 :: Fail (Text "Sending to Server from S13Quit to S14 is invalid") => Send  Server S13Quit S14 Message
else instance sendS13MessageFail6 :: Fail (Text "Sending to Server from S14 to S14 is invalid") => Send  Server S14 S14 Message
-- else instance sendS13MessageFail7 :: Fail (Text "Sending to Server from S15 to S14 is invalid") => Send  Server S15 S14 Message
-- else instance sendS13MessageFail8 :: Fail (Text "Sending to Server from S16 to S14 is invalid") => Send  Server S16 S14 Message
-- else instance sendS13MessageFail9 :: Fail (Text "Sending to Server from S12 to S14 is invalid") => Send  Server S12 S14 Message
instance sendS13Recv :: Send Server S13Recv S15 Recv
-- else instance sendS13RecvFail1 :: Fail (Text "Sending to Server from S11 to S15 is invalid") => Send  Server S11 S15 Recv
-- else instance sendS13RecvFail2 :: Fail (Text "Sending to Server from S11Connected to S15 is invalid") => Send  Server S11Connected S15 Recv
-- else instance sendS13RecvFail3 :: Fail (Text "Sending to Server from S13 to S15 is invalid") => Send  Server S13 S15 Recv
-- else instance sendS13RecvFail4 :: Fail (Text "Sending to Server from S13Message to S15 is invalid") => Send  Server S13Message S15 Recv
-- else instance sendS13RecvFail5 :: Fail (Text "Sending to Server from S13Quit to S15 is invalid") => Send  Server S13Quit S15 Recv
-- else instance sendS13RecvFail6 :: Fail (Text "Sending to Server from S14 to S15 is invalid") => Send  Server S14 S15 Recv
else instance sendS13RecvFail7 :: Fail (Text "Sending to Server from S15 to S15 is invalid") => Send  Server S15 S15 Recv
-- else instance sendS13RecvFail8 :: Fail (Text "Sending to Server from S16 to S15 is invalid") => Send  Server S16 S15 Recv
-- else instance sendS13RecvFail9 :: Fail (Text "Sending to Server from S12 to S15 is invalid") => Send  Server S12 S15 Recv
instance sendS13Quit :: Send Server S13Quit S16 Quit
-- else instance sendS13QuitFail1 :: Fail (Text "Sending to Server from S11 to S16 is invalid") => Send  Server S11 S16 Quit
-- else instance sendS13QuitFail2 :: Fail (Text "Sending to Server from S11Connected to S16 is invalid") => Send  Server S11Connected S16 Quit
-- else instance sendS13QuitFail3 :: Fail (Text "Sending to Server from S13 to S16 is invalid") => Send  Server S13 S16 Quit
-- else instance sendS13QuitFail4 :: Fail (Text "Sending to Server from S13Message to S16 is invalid") => Send  Server S13Message S16 Quit
-- else instance sendS13QuitFail5 :: Fail (Text "Sending to Server from S13Recv to S16 is invalid") => Send  Server S13Recv S16 Quit
-- else instance sendS13QuitFail6 :: Fail (Text "Sending to Server from S14 to S16 is invalid") => Send  Server S14 S16 Quit
-- else instance sendS13QuitFail7 :: Fail (Text "Sending to Server from S15 to S16 is invalid") => Send  Server S15 S16 Quit
else instance sendS13QuitFail8 :: Fail (Text "Sending to Server from S16 to S16 is invalid") => Send  Server S16 S16 Quit
-- else instance sendS13QuitFail9 :: Fail (Text "Sending to Server from S12 to S16 is invalid") => Send  Server S12 S16 Quit
instance selectS13 :: Select Server S13 ("quit" :: S13Quit, "message" :: S13Message, "receive" :: S13Recv)
instance receiveS14 :: Receive Server S14 S13 Messages
-- else instance receiveS14Fail1 :: Fail (Text "Receiving from Server from S11 to S13 is invalid") => Receive  Server S11 S13 Messages
-- else instance receiveS14Fail2 :: Fail (Text "Receiving from Server from S11Connected to S13 is invalid") => Receive  Server S11Connected S13 Messages
else instance receiveS14Fail3 :: Fail (Text "Receiving from Server from S13 to S13 is invalid") => Receive  Server S13 S13 Messages
-- else instance receiveS14Fail4 :: Fail (Text "Receiving from Server from S13Message to S13 is invalid") => Receive  Server S13Message S13 Messages
-- else instance receiveS14Fail5 :: Fail (Text "Receiving from Server from S13Recv to S13 is invalid") => Receive  Server S13Recv S13 Messages
-- else instance receiveS14Fail6 :: Fail (Text "Receiving from Server from S13Quit to S13 is invalid") => Receive  Server S13Quit S13 Messages
-- else instance receiveS14Fail7 :: Fail (Text "Receiving from Server from S15 to S13 is invalid") => Receive  Server S15 S13 Messages
-- else instance receiveS14Fail8 :: Fail (Text "Receiving from Server from S16 to S13 is invalid") => Receive  Server S16 S13 Messages
-- else instance receiveS14Fail9 :: Fail (Text "Receiving from Server from S12 to S13 is invalid") => Receive  Server S12 S13 Messages
instance receiveS15 :: Receive Server S15 S13 Messages
-- else instance receiveS15Fail1 :: Fail (Text "Receiving from Server from S11 to S13 is invalid") => Receive  Server S11 S13 Messages
-- else instance receiveS15Fail2 :: Fail (Text "Receiving from Server from S11Connected to S13 is invalid") => Receive  Server S11Connected S13 Messages
-- else instance receiveS15Fail3 :: Fail (Text "Receiving from Server from S13 to S13 is invalid") => Receive  Server S13 S13 Messages
-- else instance receiveS15Fail4 :: Fail (Text "Receiving from Server from S13Message to S13 is invalid") => Receive  Server S13Message S13 Messages
-- else instance receiveS15Fail5 :: Fail (Text "Receiving from Server from S13Recv to S13 is invalid") => Receive  Server S13Recv S13 Messages
-- else instance receiveS15Fail6 :: Fail (Text "Receiving from Server from S13Quit to S13 is invalid") => Receive  Server S13Quit S13 Messages
-- else instance receiveS15Fail7 :: Fail (Text "Receiving from Server from S14 to S13 is invalid") => Receive  Server S14 S13 Messages
-- else instance receiveS15Fail8 :: Fail (Text "Receiving from Server from S16 to S13 is invalid") => Receive  Server S16 S13 Messages
-- else instance receiveS15Fail9 :: Fail (Text "Receiving from Server from S12 to S13 is invalid") => Receive  Server S12 S13 Messages
instance disconnectS16 :: Disconnect Client Server S16 S12

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S26 :: Type
foreign import data S28 :: Type
foreign import data S28Message :: Type
foreign import data S28Recv :: Type
foreign import data S28Quit :: Type
foreign import data S29 :: Type
foreign import data S30 :: Type
foreign import data S31 :: Type
foreign import data S27 :: Type

instance initialServer :: Initial Server S26
instance terminalServer :: Terminal Server S27
instance acceptS26 :: Accept Server Client S26 S28
instance receiveS28Message :: Receive Client S28Message S29 Message
-- else instance receiveS28MessageFail1 :: Fail (Text "Receiving from Client from S26 to S29 is invalid") => Receive  Client S26 S29 Message
-- else instance receiveS28MessageFail2 :: Fail (Text "Receiving from Client from S28 to S29 is invalid") => Receive  Client S28 S29 Message
-- else instance receiveS28MessageFail3 :: Fail (Text "Receiving from Client from S28Recv to S29 is invalid") => Receive  Client S28Recv S29 Message
-- else instance receiveS28MessageFail4 :: Fail (Text "Receiving from Client from S28Quit to S29 is invalid") => Receive  Client S28Quit S29 Message
-- else instance receiveS28MessageFail5 :: Fail (Text "Receiving from Client from S29 to S29 is invalid") => Receive  Client S29 S29 Message
-- else instance receiveS28MessageFail6 :: Fail (Text "Receiving from Client from S30 to S29 is invalid") => Receive  Client S30 S29 Message
-- else instance receiveS28MessageFail7 :: Fail (Text "Receiving from Client from S31 to S29 is invalid") => Receive  Client S31 S29 Message
-- else instance receiveS28MessageFail8 :: Fail (Text "Receiving from Client from S27 to S29 is invalid") => Receive  Client S27 S29 Message
instance receiveS28Recv :: Receive Client S28Recv S30 Recv
-- else instance receiveS28RecvFail1 :: Fail (Text "Receiving from Client from S26 to S30 is invalid") => Receive  Client S26 S30 Recv
-- else instance receiveS28RecvFail2 :: Fail (Text "Receiving from Client from S28 to S30 is invalid") => Receive  Client S28 S30 Recv
-- else instance receiveS28RecvFail3 :: Fail (Text "Receiving from Client from S28Message to S30 is invalid") => Receive  Client S28Message S30 Recv
-- else instance receiveS28RecvFail4 :: Fail (Text "Receiving from Client from S28Quit to S30 is invalid") => Receive  Client S28Quit S30 Recv
-- else instance receiveS28RecvFail5 :: Fail (Text "Receiving from Client from S29 to S30 is invalid") => Receive  Client S29 S30 Recv
-- else instance receiveS28RecvFail6 :: Fail (Text "Receiving from Client from S30 to S30 is invalid") => Receive  Client S30 S30 Recv
-- else instance receiveS28RecvFail7 :: Fail (Text "Receiving from Client from S31 to S30 is invalid") => Receive  Client S31 S30 Recv
-- else instance receiveS28RecvFail8 :: Fail (Text "Receiving from Client from S27 to S30 is invalid") => Receive  Client S27 S30 Recv
instance receiveS28Quit :: Receive Client S28Quit S31 Quit
-- else instance receiveS28QuitFail1 :: Fail (Text "Receiving from Client from S26 to S31 is invalid") => Receive  Client S26 S31 Quit
-- else instance receiveS28QuitFail2 :: Fail (Text "Receiving from Client from S28 to S31 is invalid") => Receive  Client S28 S31 Quit
-- else instance receiveS28QuitFail3 :: Fail (Text "Receiving from Client from S28Message to S31 is invalid") => Receive  Client S28Message S31 Quit
-- else instance receiveS28QuitFail4 :: Fail (Text "Receiving from Client from S28Recv to S31 is invalid") => Receive  Client S28Recv S31 Quit
-- else instance receiveS28QuitFail5 :: Fail (Text "Receiving from Client from S29 to S31 is invalid") => Receive  Client S29 S31 Quit
-- else instance receiveS28QuitFail6 :: Fail (Text "Receiving from Client from S30 to S31 is invalid") => Receive  Client S30 S31 Quit
-- else instance receiveS28QuitFail7 :: Fail (Text "Receiving from Client from S31 to S31 is invalid") => Receive  Client S31 S31 Quit
-- else instance receiveS28QuitFail8 :: Fail (Text "Receiving from Client from S27 to S31 is invalid") => Receive  Client S27 S31 Quit
instance branchS28 :: Branch Server Client S28 ("message" :: S28Message, "quit" :: S28Quit, "receive" :: S28Recv)
instance sendS29 :: Send Client S29 S28 Messages
-- else instance sendS29Fail1 :: Fail (Text "Sending to Client from S26 to S28 is invalid") => Send  Client S26 S28 Messages
-- else instance sendS29Fail2 :: Fail (Text "Sending to Client from S28 to S28 is invalid") => Send  Client S28 S28 Messages
-- else instance sendS29Fail3 :: Fail (Text "Sending to Client from S28Message to S28 is invalid") => Send  Client S28Message S28 Messages
-- else instance sendS29Fail4 :: Fail (Text "Sending to Client from S28Recv to S28 is invalid") => Send  Client S28Recv S28 Messages
-- else instance sendS29Fail5 :: Fail (Text "Sending to Client from S28Quit to S28 is invalid") => Send  Client S28Quit S28 Messages
-- else instance sendS29Fail6 :: Fail (Text "Sending to Client from S30 to S28 is invalid") => Send  Client S30 S28 Messages
-- else instance sendS29Fail7 :: Fail (Text "Sending to Client from S31 to S28 is invalid") => Send  Client S31 S28 Messages
-- else instance sendS29Fail8 :: Fail (Text "Sending to Client from S27 to S28 is invalid") => Send  Client S27 S28 Messages
instance sendS30 :: Send Client S30 S28 Messages
-- else instance sendS30Fail1 :: Fail (Text "Sending to Client from S26 to S28 is invalid") => Send  Client S26 S28 Messages
-- else instance sendS30Fail2 :: Fail (Text "Sending to Client from S28 to S28 is invalid") => Send  Client S28 S28 Messages
-- else instance sendS30Fail3 :: Fail (Text "Sending to Client from S28Message to S28 is invalid") => Send  Client S28Message S28 Messages
-- else instance sendS30Fail4 :: Fail (Text "Sending to Client from S28Recv to S28 is invalid") => Send  Client S28Recv S28 Messages
-- else instance sendS30Fail5 :: Fail (Text "Sending to Client from S28Quit to S28 is invalid") => Send  Client S28Quit S28 Messages
-- else instance sendS30Fail6 :: Fail (Text "Sending to Client from S29 to S28 is invalid") => Send  Client S29 S28 Messages
-- else instance sendS30Fail7 :: Fail (Text "Sending to Client from S31 to S28 is invalid") => Send  Client S31 S28 Messages
-- else instance sendS30Fail8 :: Fail (Text "Sending to Client from S27 to S28 is invalid") => Send  Client S27 S28 Messages
instance disconnectS31 :: Disconnect Server Client S31 S27

