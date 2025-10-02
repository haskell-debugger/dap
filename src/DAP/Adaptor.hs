-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Adaptor
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  -- * Reverse Requests
  , sendReverseRequest
  , sendRunInTerminalReverseRequest
  -- * Server
  , getServerCapabilities
  , withConnectionLock
  -- * Request Arguments
  , getArguments
  , getRequestSeqNum
  , getReverseRequestResponseBody
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
  , withRequest
  , getHandle
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent.Lifted  ( fork, killThread )
import           Control.Exception          ( throwIO )
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when, unless )
import           Control.Monad.Except       ( runExceptT, throwError, mapExceptT )
import           Control.Monad.State        ( runStateT, gets, gets, modify' )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Reader       ( asks, ask, runReaderT, withReaderT )
import           Data.Aeson                 ( FromJSON, Result (..), fromJSON )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.Aeson.Types           ( object, Key, KeyValue((.=)), ToJSON )
import           Data.IORef                 ( readIORef, writeIORef )
import           Data.Text                  ( unpack, pack )
import           Network.Socket             ( SockAddr )
import           System.IO                  ( Handle )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Utils
import           DAP.Log
import           DAP.Internal
----------------------------------------------------------------------------
logWarn :: T.Text -> Adaptor app request ()
logWarn msg = logWithAddr WARN Nothing (withBraces msg)
----------------------------------------------------------------------------
logError :: T.Text -> Adaptor app request ()
logError msg = logWithAddr ERROR Nothing (withBraces msg)
----------------------------------------------------------------------------
logInfo :: T.Text -> Adaptor app request ()
logInfo msg = logWithAddr INFO Nothing (withBraces msg)
----------------------------------------------------------------------------
-- | Meant for internal consumption, used to signify a message has been
-- SENT from the server
debugMessage :: DebugStatus -> BL8.ByteString -> Adaptor app request ()
debugMessage dir msg = do
#if MIN_VERSION_text(2,0,0)
  logWithAddr DEBUG (Just dir) (TE.decodeUtf8Lenient (BL8.toStrict msg))
#else
  logWithAddr DEBUG (Just dir) (TE.decodeUtf8 (BL8.toStrict msg))
#endif
----------------------------------------------------------------------------
-- | Meant for external consumption
logWithAddr :: Level -> Maybe DebugStatus -> T.Text -> Adaptor app request ()
logWithAddr level status msg = do
  addr <- getAddress
  logAction <- getLogAction
  liftIO (logger logAction level addr status msg)
----------------------------------------------------------------------------
-- | Meant for external consumption
logger :: LogAction IO DAPLog -> Level -> SockAddr -> Maybe DebugStatus -> T.Text -> IO ()
logger logAction level addr maybeDebug msg =
  logAction <& DAPLog level maybeDebug addr msg
----------------------------------------------------------------------------
getServerCapabilities :: Adaptor app request Capabilities
getServerCapabilities = asks (serverCapabilities . serverConfig)
----------------------------------------------------------------------------
getAddress :: Adaptor app request SockAddr
getAddress = asks address
----------------------------------------------------------------------------
getLogAction :: Adaptor app request (LogAction IO DAPLog)
getLogAction = asks logAction
----------------------------------------------------------------------------
getHandle :: Adaptor app r Handle
getHandle = asks handle
----------------------------------------------------------------------------
getRequestSeqNum :: Adaptor app Request Seq
getRequestSeqNum = asks (requestSeqNum . request)
----------------------------------------------------------------------------
getDebugSessionId :: Adaptor app request SessionId
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
setDebugSessionId :: SessionId -> Adaptor app request ()
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
  -> Adaptor app request ()
registerNewDebugSession k v debuggerConcurrentActions = do
  store <- asks appStore
  lcl <- ask
  let lcl' = lcl { request = () }
  let emptyState = AdaptorState MessageTypeEvent []
  debuggerThreadState <- liftIO $
    DebuggerThreadState
      <$> sequence [fork $ action (runAdaptorWith lcl' emptyState) | action <- debuggerConcurrentActions]
  liftIO . atomically $ modifyTVar' store (H.insert k (debuggerThreadState, v))
  logInfo $ T.pack $ "Registered new debug session: " <> unpack k
  setDebugSessionId k

----------------------------------------------------------------------------
updateDebugSession :: (app -> app) -> Adaptor app request ()
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
getDebugSessionWithThreadIdAndSessionId :: Adaptor app request (SessionId, DebuggerThreadState, app)
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
destroyDebugSession :: Adaptor app request ()
destroyDebugSession = do
  (sessionId, DebuggerThreadState {..}, _) <- getDebugSessionWithThreadIdAndSessionId
  store <- getAppStore
  liftIO $ do
    mapM_ killThread debuggerThreads
    atomically $ modifyTVar' store (H.delete sessionId)
  logInfo $ T.pack $ "SessionId " <> unpack sessionId <> " ended"
----------------------------------------------------------------------------
getAppStore :: Adaptor app request (AppStore app)
getAppStore = asks appStore
----------------------------------------------------------------------------
getCommand :: Adaptor app Request Command
getCommand = command <$> asks request
----------------------------------------------------------------------------
-- | 'sendRaw' (internal use only)
-- Sends a raw JSON payload to the editor. No "seq", "type" or "command" fields are set.
-- The message is still encoded with the ProtocolMessage Header, byte count, and CRLF.
--
sendRaw :: ToJSON value => value -> Adaptor app request ()
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
  unless (messageType == MessageTypeEvent) (setField "seq" seqNum)

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets payload

  -- Send payload to client from debug adaptor
  writeToHandle address handle payload
  resetAdaptorStatePayload
----------------------------------------------------------------------------
-- | Write event to Handle
sendEvent
  :: Adaptor app request ()
  -> Adaptor app request ()
sendEvent action = do
  ()            <- action
  handle        <- getHandle
  messageType   <- gets messageType
  address       <- getAddress
  let errorMsg =
        "Use 'send' function when responding to a DAP request, "
        <> "'sendEvent' is for responding to events"
  case messageType of
    MessageTypeResponse ->
      sendError (ErrorMessage errorMsg) Nothing
    MessageTypeRequest ->
      sendError (ErrorMessage errorMsg) Nothing
    MessageTypeEvent ->
      setField "type" messageType

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets payload
  -- Send payload to client from debug adaptor
  writeToHandle address handle payload
  resetAdaptorStatePayload
----------------------------------------------------------------------------
-- | Write reverse request to Handle
sendReverseRequest
  :: ReverseCommand
  -> Adaptor app Request ()
sendReverseRequest rcmd = send $ do
  setField "type" MessageTypeRequest
  setField "command" rcmd
----------------------------------------------------------------------------
-- | Send runInTerminal reverse request
sendRunInTerminalReverseRequest :: RunInTerminalRequestArguments -> Adaptor app Request ()
sendRunInTerminalReverseRequest args = do
  setField "arguments" args
  sendReverseRequest ReverseCommandRunInTerminal

----------------------------------------------------------------------------
-- | Writes payload to the given 'Handle' using the local connection lock
----------------------------------------------------------------------------
writeToHandle
  :: ToJSON event
  => SockAddr
  -> Handle
  -> event
  -> Adaptor app request ()
writeToHandle _ handle evt = do
  let msg = encodeBaseProtocolMessage evt
  debugMessage SENT ("\n" <> encodePretty evt)
  withConnectionLock (BS.hPutStr handle msg)
----------------------------------------------------------------------------
-- | Resets Adaptor's payload
----------------------------------------------------------------------------
resetAdaptorStatePayload :: Adaptor app request ()
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
sendSuccesfulEvent
  :: EventType
  -> Adaptor app request ()
  -> Adaptor app request ()
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
  -> Adaptor app request a
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
  -> Adaptor app request ()
setErrorMessage v = setField "message" v
----------------------------------------------------------------------------
-- | Sends successful event
setSuccess
  :: Bool
  -> Adaptor app request ()
setSuccess = setField "success"
----------------------------------------------------------------------------
setBody
  :: ToJSON value
  => value
  -> Adaptor app request ()
setBody value = setField "body" value
----------------------------------------------------------------------------
setType
  :: MessageType
  -> Adaptor app request ()
setType messageType = do
  modify' $ \adaptorState ->
    adaptorState
    { messageType = messageType
    }
----------------------------------------------------------------------------
setEvent
  :: EventType
  -> Adaptor app request ()
setEvent = setField "event"
----------------------------------------------------------------------------
setField
  :: ToJSON value
  => Key
  -> value
  -> Adaptor app request ()
setField key value = do
  currentPayload <- gets payload
  modify' $ \adaptorState ->
    adaptorState
    { payload = (key .= value) : currentPayload
    }
----------------------------------------------------------------------------
withConnectionLock
  :: IO ()
  -> Adaptor app request ()
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
      logError msg
      liftIO $ throwIO (ExpectedArguments msg)
    Just val ->
      case fromJSON val of
        Success r -> pure r
        Error reason -> do
          logError (T.pack reason)
          liftIO $ throwIO (ParseException reason)
----------------------------------------------------------------------------
-- | Attempt to parse arguments from a ReverseRequestResponse (not in env)
----------------------------------------------------------------------------
getReverseRequestResponseBody
  :: (Show value, FromJSON value)
  => ReverseRequestResponse -> Adaptor app r value
getReverseRequestResponseBody resp = do
  let maybeArgs = body resp
  let msg = "No args found for this message"
  case maybeArgs of
    Nothing -> do
      logError msg
      liftIO $ throwIO (ExpectedArguments msg)
    Just val ->
      case fromJSON val of
        Success r -> pure r
        Error reason -> do
          logError (T.pack reason)
          liftIO $ throwIO (ParseException reason)
----------------------------------------------------------------------------
-- | Evaluates Adaptor action by using and updating the state in the MVar
runAdaptorWith :: AdaptorLocal app request -> AdaptorState -> Adaptor app request () -> IO ()
runAdaptorWith lcl st (Adaptor action) = do
  (es,final_st) <- runStateT (runReaderT (runExceptT action) lcl) st
  case es of
    Left err -> error ("runAdaptorWith, unhandled exception:" <> show err)
    Right () -> case final_st of
      AdaptorState _ p ->
        if null p
          then return ()
          else error $ "runAdaptorWith, unexpected payload:" <> show p
----------------------------------------------------------------------------
-- | Utility for evaluating a monad transformer stack
runAdaptor :: AdaptorLocal app Request -> AdaptorState -> Adaptor app Request () -> IO ()
runAdaptor lcl s (Adaptor client) =
  runStateT (runReaderT (runExceptT client) lcl) s >>= \case
    (Left (errorMessage, maybeMessage), s') ->
      runAdaptor lcl s' (sendErrorResponse errorMessage maybeMessage)
    (Right (), _) -> pure ()
----------------------------------------------------------------------------

withRequest :: Request -> Adaptor app Request a -> Adaptor app r a
withRequest r (Adaptor client) = Adaptor (mapExceptT (withReaderT (\lcl -> lcl { request = r })) client)
