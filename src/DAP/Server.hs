-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Server
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
----------------------------------------------------------------------------
module DAP.Server
  ( runDAPServer
  , runDAPServerWithLogger
  , readPayload
  , TerminateServer(..)
  ) where
----------------------------------------------------------------------------
import           Control.Monad              ( when, forever )
import           Control.Concurrent         ( ThreadId, myThreadId, throwTo )
import           Control.Concurrent.MVar    ( newMVar )
import           Control.Concurrent.STM     ( newTVarIO )
import           Control.Exception          ( Exception
                                            , SomeAsyncException(..)
                                            , SomeException
                                            , IOException
                                            , catch
                                            , fromException
                                            , toException
                                            , throwIO )
import           Control.Monad              ( void )
import           Data.Aeson                 ( decodeStrict, eitherDecode, Value, FromJSON )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.ByteString            ( ByteString )
import           Data.Char                  ( isDigit )
import           Data.IORef                 ( newIORef )
import           Network.Simple.TCP         ( serve, HostPreference(Host) )
import           Network.Socket             ( socketToHandle, withSocketsDo, SockAddr )
import           System.IO                  ( hClose, hSetNewlineMode, Handle, Newline(CRLF)
                                            , NewlineMode(NewlineMode, outputNL, inputNL)
                                            , IOMode(ReadWriteMode), stderr, hPrint)
import           System.IO.Error            ( isEOFError )
import           System.Exit                ( exitWith, ExitCode(ExitSuccess) )
import           Text.Read                  ( readMaybe )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Internal
import           DAP.Utils
import           DAP.Adaptor
import           DAP.Log
----------------------------------------------------------------------------

stdoutLogger :: IO (LogAction IO T.Text)
stdoutLogger = do
  handleLock               <- newMVar ()
  return $ LogAction $ \msg -> do
    withLock handleLock $ do
      T.putStrLn msg

-- | An exception to throw if you want to stop the server execution from a
-- client. This is useful if you launch a new server per debugging session and
-- want to terminate it at the end.
data TerminateServer = TerminateServer
  deriving (Show, Exception)

-- | Simpler version of 'runDAPServerWithLogger'.
--
-- If you don't need a custom logger or to observe reverse request responses.
runDAPServer :: ServerConfig -> (Command -> Adaptor app Request ()) -> IO ()
runDAPServer config communicate = do
  l <- stdoutLogger
  runDAPServerWithLogger (cmap renderDAPLog l) config communicate (const (pure ()))

runDAPServerWithLogger
  :: LogAction IO DAPLog
  -> ServerConfig
  -- ^ Top-level Server configuration, global across all debug sessions
  -> (Command -> Adaptor app Request ())
  -- ^ A function to facilitate communication between DAP clients, debug adaptors and debuggers
  -> (ReverseRequestResponse -> Adaptor app () ())
  -- ^ A function to receive reverse-request-responses from DAP clients
  -> IO ()
runDAPServerWithLogger rawLogAction serverConfig@ServerConfig {..} communicate ackResp = withSocketsDo $ do
  let logAction = cfilter (\msg -> if debugLogging then True else severity msg /= DEBUG) rawLogAction
  logAction <& (mkDebugMessage $ (T.pack ("Running DAP server on " <> show port <> "...")))
  appStore <- newTVarIO mempty
  mainThread <- myThreadId
  let
    server = serve (Host host) (show port) $ \(socket, address) -> do
      logAction <& mkDebugMessage (T.pack ("TCP connection established from " ++ show address))
      handle <- socketToHandle socket ReadWriteMode
      hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
      adaptorStateMVar <- initAdaptorState logAction handle address appStore serverConfig
      serviceClient communicate ackResp adaptorStateMVar
        `catch` exceptionHandler logAction handle address debugLogging mainThread
  server `catch` \(SomeAsyncException e) ->
    case fromException $ toException e of
      Just TerminateServer -> exitWith ExitSuccess
      _                    -> throwIO e

-- | Initializes the Adaptor
--
initAdaptorState
  :: LogAction IO DAPLog
  -> Handle
  -> SockAddr
  -> AppStore app
  -> ServerConfig
  -> IO (AdaptorLocal app ())
initAdaptorState logAction handle address appStore serverConfig = do
  handleLock               <- newMVar ()
  sessionId                <- newIORef Nothing
  let request = ()
  pure AdaptorLocal
    { ..
    }
----------------------------------------------------------------------------
-- | Communication loop between editor and adaptor
-- Evaluates the current 'Request' located in the 'AdaptorState'
-- Fetches, updates and recurses on the next 'Request'
--
-- Similarly, if the client responded to a reverse request then we execute the
-- acknowledge action (which, notably, is not an @'Adaptor' _ 'Request'@
-- because there's no 'Request' to reply to)
serviceClient
  :: (Command -> Adaptor app Request ())
  -> (ReverseRequestResponse -> Adaptor app r ())
  -> AdaptorLocal app r
  -> IO ()
serviceClient communicate ackResp lcl = forever $ runAdaptorWith lcl st $ do
    either_nextRequest <- getRequest
    case either_nextRequest of
      Right nextRequest ->
        withRequest nextRequest (communicate (command nextRequest))
      Left rrr -> ackResp rrr
  where
    st = AdaptorState MessageTypeResponse []
----------------------------------------------------------------------------
-- | Handle exceptions from client threads, parse and log accordingly.
-- Detects if client failed with `TerminateServer` and kills the server accordingly by sending an exception to the main thread.
exceptionHandler :: LogAction IO DAPLog -> Handle -> SockAddr -> Bool -> ThreadId -> SomeException -> IO ()
exceptionHandler logAction handle address shouldLog serverThread (e :: SomeException) = do
  let
    dumpError
      | Just TerminateServer      <- fromException e
          = do
            logger logAction ERROR address Nothing
              $ withBraces
              $ T.pack ("Server terminated!")
            throwTo serverThread (SomeAsyncException TerminateServer)
      | Just (ParseException msg) <- fromException e
          = logger logAction ERROR address Nothing
            $ withBraces
            $ T.pack ("Parse Exception encountered: " <> msg)
      | Just (err :: IOException) <- fromException e, isEOFError err
          = logger logAction INFO address (Just SENT)
            $ withBraces "Client has ended its connection"
      | otherwise
          = logger logAction ERROR address Nothing
            $ withBraces
            $ T.pack ("Unknown Exception: " <> show e)
  hPrint stderr ("Handling" <> show e)
  when shouldLog $ do
    dumpError
    logger logAction INFO address (Just SENT) (withBraces "Closing Connection")
  hClose handle
----------------------------------------------------------------------------
-- | Internal function for parsing a 'ProtocolMessage' header
-- This function also dispatches on 'talk'
--
-- 'parseHeader' Attempts to parse 'Content-Length: <byte-count>'
-- Helper function for parsing message headers
-- e.g. ("Content-Length: 11\r\n")
getRequest :: Adaptor app r (Either ReverseRequestResponse Request)
getRequest = do
  handle <- getHandle
  header <- liftIO $ getHeaderHandle handle
  case header of
    Left errorMessage -> do
      logError (T.pack errorMessage)
      liftIO $ throwIO (ParseException errorMessage)
    Right count -> do
      body <- liftIO $ BS.hGet handle count
      debugMessage RECEIVED
          ("\n" <> encodePretty (decodeStrict body :: Maybe Value))
      case eitherDecode (BL8.fromStrict body) of
        Left couldn'tDecodeBody -> do
          -- As a fallback, try to parse a reverse request response
          case eitherDecode (BL8.fromStrict body) of
            Right rrr -> pure (Left rrr)
            Left _ -> do
              -- No luck, report fail to parse command:
              logError (T.pack couldn'tDecodeBody)
              liftIO $ throwIO (ParseException couldn'tDecodeBody)
        Right request ->
          pure (Right request)

getHeaderHandle :: Handle -> IO (Either String PayloadSize)
getHeaderHandle handle = do
  headerBytes <- BS.hGetLine handle
  void (BS.hGetLine handle)
  pure $ parseHeader headerBytes


----------------------------------------------------------------------------
-- | Parses the HeaderPart of all ProtocolMessages
parseHeader :: ByteString -> Either String PayloadSize
parseHeader bytes =
  let byteSize = BS.takeWhile isDigit (BS.drop (BS.length "Content-Length: ") bytes)
  in case readMaybe (BS.unpack byteSize) of
        Just contentLength ->
          Right contentLength
        Nothing ->
          Left ("Invalid payload: " <> BS.unpack bytes)
----------------------------------------------------------------------------
-- | Helper function to parse a 'ProtocolMessage', extracting it's body.
-- used for testing.
--
readPayload :: FromJSON json => Handle -> IO (Either String json)
readPayload handle = do
  headerBytes <- BS.hGetLine handle
  void (BS.hGetLine handle)
  case parseHeader headerBytes of
    Left e -> pure (Left e)
    Right count -> do
      body <- BS.hGet handle count
      pure $ eitherDecode (BL8.fromStrict body)
----------------------------------------------------------------------------
