-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Adaptor
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
----------------------------------------------------------------------------
module DAP.Adaptor
  ( -- * Message Construction
    setBody
  , setField
    -- * Response
  , sendSuccesfulEmptyResponse
  , sendSuccesfulResponse
  , sendErrorResponse
  -- * Events
  , sendSuccesfulEvent
  -- * Server
  , getServerCapabilities
  , withConnectionLock
  -- * Request Arguments
  , getArguments
  , getRequestSeqNum
  -- * Debug Session
  , registerNewDebugSession
  , updateDebugSession
  , getDebugSession
  , getDebugSessionId
  , destroyDebugSession
  -- * Error handling
  , sendError
  -- * Logging
  , logWarn
  , logError
  , logInfo
  , logger
  , debugMessage
  -- * Internal use
  , send
  , sendRaw
  -- * Internal function used to execute actions on behalf of the DAP server
  -- from child threads (useful for handling asynchronous debugger events).
  , runAdaptorWith
  , runAdaptor
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent.Lifted  ( fork, killThread )
import           Control.Exception          ( throwIO )
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when, unless )
import           Control.Monad.Except       ( runExceptT, throwError )
import           Control.Monad.State        ( runStateT, gets, gets, modify' )
import Control.Monad.Reader
import           Data.Aeson                 ( FromJSON, Result (..), fromJSON )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.Aeson.Types           ( object, Key, KeyValue((.=)), ToJSON )
import           Data.Text                  ( unpack, pack )
import           Network.Socket             ( SockAddr )
import           System.IO                  ( Handle )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as H
import Data.IORef
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Utils
import           DAP.Internal
----------------------------------------------------------------------------
logWarn :: BL8.ByteString -> Adaptor app r ()
logWarn msg = logWithAddr WARN Nothing (withBraces msg)
----------------------------------------------------------------------------
logError :: BL8.ByteString -> Adaptor app r ()
logError msg = logWithAddr ERROR Nothing (withBraces msg)
----------------------------------------------------------------------------
logInfo :: BL8.ByteString -> Adaptor app r ()
logInfo msg = logWithAddr INFO Nothing (withBraces msg)
----------------------------------------------------------------------------
-- | Meant for internal consumption, used to signify a message has been
-- SENT from the server
debugMessage :: BL8.ByteString -> Adaptor app r ()
debugMessage msg = do
  shouldLog <- getDebugLogging
  addr <- getAddress
  liftIO
    $ when shouldLog
    $ logger DEBUG addr (Just SENT) msg
----------------------------------------------------------------------------
-- | Meant for external consumption
logWithAddr :: Level -> Maybe DebugStatus -> BL8.ByteString -> Adaptor app r ()
logWithAddr level status msg = do
  addr <- getAddress
  liftIO (logger level addr status msg)
----------------------------------------------------------------------------
-- | Meant for external consumption
logger :: Level -> SockAddr -> Maybe DebugStatus -> BL8.ByteString -> IO ()
logger level addr maybeDebug msg = do
  liftIO
    $ withGlobalLock
    $ BL8.putStrLn formatted
  where
    formatted
      = BL8.concat
      [ withBraces $ BL8.pack (show addr)
      , withBraces $ BL8.pack (show level)
      , maybe mempty (withBraces . BL8.pack . show) maybeDebug
      , msg
      ]
----------------------------------------------------------------------------
getDebugLogging :: Adaptor app r Bool
getDebugLogging = asks (debugLogging . serverConfig)
----------------------------------------------------------------------------
getServerCapabilities :: Adaptor app r Capabilities
getServerCapabilities = asks (serverCapabilities . serverConfig)
----------------------------------------------------------------------------
getAddress :: Adaptor app r SockAddr
getAddress = asks address
----------------------------------------------------------------------------
getHandle :: Adaptor app r Handle
getHandle = asks handle
----------------------------------------------------------------------------
getRequestSeqNum :: Adaptor app Request Seq
getRequestSeqNum = asks (requestSeqNum . request)
----------------------------------------------------------------------------
getDebugSessionId :: Adaptor app r SessionId
getDebugSessionId = do
  var <- asks (sessionId)
  res <- liftIO $ readIORef var
  case res of
    Nothing -> sessionNotFound
    Just sessionId -> pure sessionId
  where
    sessionNotFound = do
      let err = "No Debug Session has started"
      sendError (ErrorMessage (pack err)) Nothing
----------------------------------------------------------------------------
setDebugSessionId :: SessionId -> Adaptor app r ()
setDebugSessionId session = do
  var <- asks sessionId
  liftIO $ writeIORef var (Just session)
----------------------------------------------------------------------------
registerNewDebugSession
  :: SessionId
  -> app
  -> [(Adaptor app () () -> IO ()) -> IO ()]
  -- ^ Actions to run debugger (operates in a forked thread that gets killed when disconnect is set)
  -- Long running operation, meant to be used as a sink for
  -- the debugger to emit events and for the adaptor to forward to the editor
  -- This function should be in a 'forever' loop waiting on the read end of
  -- a debugger channel.
  --
  -- This event handler thread also takes an argument that allows any child thread to execute
  -- events on behalf of the DAP server (in 'Adaptor app ()'). This function should always be
  -- used when sending events to the editor from the debugger (or from any forked thread).
  --
  -- >
  -- > registerNewDebugSession sessionId appState $ loadDebugger : [\withAdaptor ->
  -- >   forever $ getDebuggerOutput >>= \output -> do
  -- >     withAdaptor $ sendOutputEvent defaultOutputEvent { outputEventOutput = output }
  -- >   ]
  --
  -> Adaptor app r ()
registerNewDebugSession k v debuggerConcurrentActions = do
  store <- asks appStore
  lcl <- ask
  let lcl' = lcl { request = () }
  let emptyState = AdaptorState MessageTypeEvent []
  debuggerThreadState <- liftIO $
    DebuggerThreadState
      <$> sequence [fork $ action (runAdaptorWith lcl' emptyState "s") | action <- debuggerConcurrentActions]
  liftIO . atomically $ modifyTVar' store (H.insert k (debuggerThreadState, v))
  logInfo $ BL8.pack $ "Registered new debug session: " <> unpack k
  setDebugSessionId k

----------------------------------------------------------------------------
updateDebugSession :: (app -> app) -> Adaptor app r ()
updateDebugSession updateFun = do
  sessionId <- getDebugSessionId
  store <- asks appStore
  liftIO . atomically $ modifyTVar' store (H.adjust (fmap updateFun) sessionId)
----------------------------------------------------------------------------
getDebugSession :: Adaptor a r a
getDebugSession = do
  (_, _, app) <- getDebugSessionWithThreadIdAndSessionId
  pure app
----------------------------------------------------------------------------
getDebugSessionWithThreadIdAndSessionId :: Adaptor app r (SessionId, DebuggerThreadState, app)
getDebugSessionWithThreadIdAndSessionId = do
  sessionId <- getDebugSessionId
  appStore <- liftIO . readTVarIO =<< getAppStore
  case H.lookup sessionId appStore of
    Nothing -> do
      appNotFound sessionId
    Just (tid, app) ->
      pure (sessionId, tid, app)
  where
    appNotFound sessionId = do
      let err = concat
            [ "SessionID: " <> unpack sessionId
            , "has no corresponding Debugger registered"
            ]
      sendError (ErrorMessage (pack err)) Nothing
----------------------------------------------------------------------------
-- | Whenever a debug Session ends (cleanly or otherwise) this function
-- will remove the local debugger communication state from the global state
----------------------------------------------------------------------------
destroyDebugSession :: Adaptor app r ()
destroyDebugSession = do
  (sessionId, DebuggerThreadState {..}, _) <- getDebugSessionWithThreadIdAndSessionId
  store <- getAppStore
  liftIO $ do
    mapM_ killThread debuggerThreads
    atomically $ modifyTVar' store (H.delete sessionId)
  logInfo $ BL8.pack $ "SessionId " <> unpack sessionId <> " ended"
----------------------------------------------------------------------------
getAppStore :: Adaptor app r (AppStore app)
getAppStore = asks appStore
----------------------------------------------------------------------------
getCommand :: Adaptor app Request Command
getCommand = command <$> asks request
----------------------------------------------------------------------------
-- | 'sendRaw' (internal use only)
-- Sends a raw JSON payload to the editor. No "seq", "type" or "command" fields are set.
-- The message is still encoded with the ProtocolMessage Header, byte count, and CRLF.
--
sendRaw :: ToJSON value => value -> Adaptor app r ()
sendRaw value = do
  handle        <- getHandle
  address       <- getAddress
  writeToHandle address handle value
----------------------------------------------------------------------------
-- | Function for constructing a payload and writing bytes to a socket.
-- This function takes care of incrementing sequence numbers
-- and setting fields automatically that are required for 'response' messages.
-- i.e. "request_seq" and "command".
-- We also have to be sure to reset the message payload
----------------------------------------------------------------------------
send :: Adaptor app Request () -> Adaptor app Request ()
send action = do
  ()            <- action
  cmd           <- getCommand
  handle        <- getHandle
  messageType   <- gets messageType
  address       <- getAddress
  requestSeqNum <- getRequestSeqNum
  let seqNum    =  requestSeqNum + 1

  -- Additional fields are required to be set for 'response' or 'reverse_request' messages.
  when (messageType == MessageTypeResponse) (setField "request_seq" requestSeqNum)
  when (messageType `elem` [MessageTypeResponse, MessageTypeRequest]) (setField "command" cmd)

  -- "seq" and "type" must be set for all protocol messages
  setField "type" messageType
  unless (messageType == MessageTypeEvent) $
    setField "seq" seqNum

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets payload

  -- Send payload to client from debug adaptor
  writeToHandle address handle payload
  resetAdaptorStatePayload

sendEvent :: Adaptor app r () -> Adaptor app r ()
sendEvent action = do
  () <- action
  handle        <- getHandle
  messageType   <- gets messageType
  address       <- getAddress
  case messageType of
    MessageTypeResponse -> error "use send"
    MessageTypeRequest -> error "use send"
    MessageTypeEvent -> do
      address       <- getAddress
      setField "type" messageType

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets payload
  -- Send payload to client from debug adaptor
  writeToHandle address handle payload
  resetAdaptorStatePayload


----------------------------------------------------------------------------
-- | Writes payload to the given 'Handle' using the local connection lock
----------------------------------------------------------------------------
writeToHandle
  :: ToJSON event
  => SockAddr
  -> Handle
  -> event
  -> Adaptor app r ()
writeToHandle _ handle evt = do
  let msg = encodeBaseProtocolMessage evt
  debugMessage ("\n" <> encodePretty evt)
  withConnectionLock (BS.hPutStr handle msg)
----------------------------------------------------------------------------
-- | Resets Adaptor's payload
----------------------------------------------------------------------------
resetAdaptorStatePayload :: Adaptor app r ()
resetAdaptorStatePayload = modify' $ \s -> s { payload = [] }
----------------------------------------------------------------------------
sendSuccesfulResponse :: Adaptor app Request () -> Adaptor app Request ()
sendSuccesfulResponse action = do
 send $ do
    setType MessageTypeResponse
    setSuccess True
    action
----------------------------------------------------------------------------
sendSuccesfulEmptyResponse :: Adaptor app Request ()
sendSuccesfulEmptyResponse = sendSuccesfulResponse (pure ())
----------------------------------------------------------------------------
-- | Sends successful event
sendSuccesfulEvent :: EventType -> Adaptor app r () -> Adaptor app r ()
sendSuccesfulEvent event action = do
  sendEvent $ do
    setEvent event
    setType MessageTypeEvent
    action
----------------------------------------------------------------------------
-- | Raises an error
-- Meant abort the current reqeust / response cycle, prematurely sending an 'ErrorResponse'
-- <https://microsoft.github.io/debug-adapter-protocol/specification#Base_Protocol_ErrorResponse>
--
sendError
  :: ErrorMessage
  -> Maybe Message
  -> Adaptor app r a
sendError errorMessage maybeMessage = do
  throwError (errorMessage, maybeMessage)
----------------------------------------------------------------------------
-- | Sends unsuccessful response
-- Only used internally within the Server module
sendErrorResponse
  :: ErrorMessage
  -> Maybe Message
  -> Adaptor app Request ()
sendErrorResponse errorMessage maybeMessage = do
  send $ do
    setType MessageTypeResponse
    setSuccess False
    setErrorMessage errorMessage
    setBody (ErrorResponse maybeMessage)
----------------------------------------------------------------------------
setErrorMessage
  :: ErrorMessage
  -> Adaptor app r ()
setErrorMessage v = setField "message" v
----------------------------------------------------------------------------
-- | Sends successful event
setSuccess
  :: Bool
  -> Adaptor app r ()
setSuccess = setField "success"
----------------------------------------------------------------------------
setBody
  :: ToJSON value
  => value
  -> Adaptor app r ()
setBody value = setField "body" value
----------------------------------------------------------------------------
setType
  :: MessageType
  -> Adaptor app r ()
setType messageType = do
  modify' $ \adaptorState ->
    adaptorState
    { messageType = messageType
    }
----------------------------------------------------------------------------
setEvent
  :: EventType
  -> Adaptor app r ()
setEvent = setField "event"
----------------------------------------------------------------------------
setField
  :: ToJSON value
  => Key
  -> value
  -> Adaptor app r ()
setField key value = do
  currentPayload <- gets payload
  modify' $ \adaptorState ->
    adaptorState
    { payload = (key .= value) : currentPayload
    }
----------------------------------------------------------------------------
withConnectionLock
  :: IO ()
  -> Adaptor app r ()
withConnectionLock action = do
  lock <- asks handleLock
  liftIO (withLock lock action)
----------------------------------------------------------------------------
-- | Attempt to parse arguments from the Request
----------------------------------------------------------------------------
getArguments
  :: (Show value, FromJSON value)
  => Adaptor app Request value
getArguments = do
  maybeArgs <- asks (args . request)
  let msg = "No args found for this message"
  case maybeArgs of
    Nothing -> do
      logError (BL8.pack msg)
      liftIO $ throwIO (ExpectedArguments msg)
    Just val ->
      case fromJSON val of
        Success r -> pure r
        x -> do
          logError (BL8.pack (show x))
          liftIO $ throwIO (ParseException (show x))

----------------------------------------------------------------------------
-- | Evaluates Adaptor action by using and updating the state in the MVar
runAdaptorWith :: AdaptorLocal app r -> AdaptorState -> String -> Adaptor app r () -> IO ()
runAdaptorWith lcl st s (Adaptor action) = do
  runStateT (runReaderT (runExceptT action) lcl) st
  return ()

----------------------------------------------------------------------------
-- | Utility for evaluating a monad transformer stack
runAdaptor :: AdaptorLocal app Request -> AdaptorState -> Adaptor app Request () -> IO ()
runAdaptor lcl s (Adaptor client) =
  runStateT (runReaderT (runExceptT client) lcl) s >>= \case
    (Left (errorMessage, maybeMessage), s') ->
      runAdaptor lcl s' (sendErrorResponse errorMessage maybeMessage)
    (Right (), s') -> pure ()
