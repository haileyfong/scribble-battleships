module Main where

import Prelude
import Prelude hiding (div)
import Concur.Core (Widget)
import Concur.Core.Patterns (loopState)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputWithButton)
import Control.Monad.State (StateT, evalStateT, get)
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Tuple (Tuple(..))
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Effect.Aff  (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Data.Time.Duration (Milliseconds(..))
import Effect.Console (log)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (negateDuration)
import Scribble.Protocol.ChatServer.ChatServer as CS
import Scribble.FSM (Role(..))
import Scribble.Session (Session, session, connect, send, receive, lift, whileWaiting, select, choice, disconnect)
import Scribble.Transport.WebSocket (WebSocket, URL(..))
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(..))
import Control.Bind.Indexed (ibind)
import Control.Applicative.Indexed (ipure)
import Unsafe.Coerce (unsafeCoerce)


type ChatBoxState = String
type CurrentState = String

chatWidget :: forall a. Widget HTML String
chatWidget = do
    message <- D.div' [ textInputWithButton "" "Enter" [P.placeholder "Type Your Message"] [] ]
    pure message

timersWidget :: forall a. Widget HTML a
timersWidget = D.div' (map timerWidget [1,2,3,4,5])

timerWidget :: forall a. Int -> Widget HTML a
timerWidget idx = D.div'
  [ D.h4' [D.text ("Timer " <> show idx)]
  , timer Nothing
  ]
  where
    timer prevTimeResult = do
      D.div'[ maybe empty (\t -> D.div' [D.text ("Previous time - " <> show t)]) prevTimeResult
          , D.button [P.onClick] [D.text "Start timer"] >>= \_ -> getNewTime
          ] >>= Just >>> timer
    getNewTime = do
      startTime <- liftEffect now
      liftEffect $ log $ "Started Timer " <> show idx <> " at time " <> show startTime
      liftAff (delay (Milliseconds 3000.0)) <|> D.button [unit <$ P.onClick] [D.text "Cancel"]
      stopTime <- liftEffect now
      liftEffect $ log $ "Stopped Timer " <> show idx <> " at time " <> show stopTime
      pure $ unInstant stopTime <> negateDuration (unInstant startTime)

updateStates :: forall a. String -> ChatBoxState -> Session (Widget HTML) WebSocket CS.S13 CS.S13 String
updateStates msg st = do
  case msg of
    "" -> pure st
    _ -> pure msg

data ChatFormAction
  = Input String
  | Submit
  | SubmitStr String

chatFormWidget :: Maybe String -> CurrentState -> Widget HTML ChatFormAction
chatFormWidget input st = do
  res <- D.div'
    [ D.button [Submit <$ P.onClick, P.value "Enter", P.disabled (not $ isJust input)] [D.text "Enter"]
    , Input <$> textInputEnter (maybe st show input) "" false
    ]
  case res of
    Input s ->
      case s of
        "" -> chatFormWidget input st
        str -> pure (Input str)
        -- str  -> chatFormWidget (Just str) str
    Submit ->
      case input of
        Nothing -> chatFormWidget Nothing st
        Just i  -> pure (SubmitStr i)
    _ -> chatFormWidget input st

textInputEnter :: String -> String -> Boolean -> Widget HTML String
textInputEnter value hint reset = do
    e <- D.input [P.defaultValue value, P.onChange, P.placeholder hint]
    new <- pure $ unsafeGetVal e
    pure new
  where
    unsafeGetVal e = (unsafeCoerce e).target.value

receiveChatSession :: forall a. ChatBoxState -> Session (Widget HTML) WebSocket CS.S13 CS.S13 Unit
receiveChatSession st = do
  select (SProxy :: SProxy "receive")
  send (CS.Recv)
  (CS.Messages messages) <- receive
  newSt <- updateStates messages st
  -- _ <- lift $ D.div' [D.text ("Silly Chatting"),true <$ textInputWithButton "" "Enter" [P.placeholder "Type Your Message"] [], D.p' [D.text newSt], false <$ liftAff (delay (Milliseconds 6.0))]
  lift $ D.div' [D.text ("Silly Chatting"), D.p' [D.text newSt]] <|> liftAff (delay(Milliseconds 100.0))
  receiveChatSession newSt
    where
    bind = ibind
    pure = ipure
    discard = bind


continueUntilSentSession :: forall a. CurrentState -> Session (Widget HTML) WebSocket CS.S13 CS.S13 Unit
continueUntilSentSession st = do
  newSt <- lift $ chatFormWidget (Just st) st
  case newSt of
    Input s -> continueUntilSentSession s
    SubmitStr s -> sendChatSession s
    _ -> continueUntilSentSession st

sendChatSession :: forall a. String -> Session (Widget HTML) WebSocket CS.S13 CS.S13 Unit
sendChatSession msg = do
  select (SProxy :: SProxy "message")
  sendMessage <- lift $ D.div' [ textInputWithButton "" "Enter" [P.placeholder "Type Your Message"] [] ]
  -- sendMessage <- lift $ chatFormWidget Nothing st
  send (CS.Message msg)
  (CS.Messages messages) <- receive
  continueUntilSentSession ""
    where
    bind = ibind
    pure = ipure
    discard = bind


sessionReceiveWidget :: forall a. Int -> ChatBoxState -> Widget HTML Int
sessionReceiveWidget port st = session
  (Proxy :: Proxy WebSocket)
  (Role :: Role CS.Client) $ do
  -- lift $ D.button [P.onClick] [D.text "Connect to Chat Server"]
  connect (Role :: Role CS.Server) (URL $ "ws://localhost:" <> show port)
  send (CS.Connect)
  receiveChatSession st
  select (SProxy :: SProxy "quit")
  send CS.Quit
  disconnect (Role :: Role CS.Server)
  pure 1
    where
    bind = ibind
    pure = ipure
    discard = bind

sessionSendWidget :: forall a. Int -> CurrentState -> Widget HTML Int
sessionSendWidget port st = session
  (Proxy :: Proxy WebSocket)
  (Role :: Role CS.Client) $ do
  connect (Role :: Role CS.Server) (URL $ "ws://localhost:" <> show port)
  send (CS.Connect)
  send (CS.Connect)
  sendChatSession st
  select (SProxy :: SProxy "quit")
  send CS.Quit
  disconnect (Role :: Role CS.Server)
  pure 1
    where
    bind = ibind
    pure = ipure
    discard = bind

main :: Effect Unit
main = runWidgetInDom "root" $ (sessionReceiveWidget 9160 ""  <> sessionSendWidget 9160 "")