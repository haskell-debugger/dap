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
----------------------------------------------------------------------------
module DAP.Server
  ( runDAPServer
  , runDAPServerWithLogger
  , readPayload
  ) where
----------------------------------------------------------------------------
import           Control.Monad              ( when, forever )
import           Control.Concurrent.MVar    ( newMVar )
import           Control.Concurrent.STM     ( newTVarIO )
import           Control.Exception          ( SomeException
                                            , IOException
                                            , catch
                                            , fromException
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



runDAPServer :: ServerConfig -> (Command -> Adaptor app Request ()) -> IO ()
runDAPServer config communicate = do
  l <- stdoutLogger
  runDAPServerWithLogger (cmap renderDAPLog l) config communicate

runDAPServerWithLogger
  :: LogAction IO DAPLog
  -> ServerConfig
  -- ^ Top-level Server configuration, global across all debug sessions
  -> (Command -> Adaptor app Request ())
  -- ^ A function to facilitate communication between DAP clients, debug adaptors and debuggers
  -> IO ()
runDAPServerWithLogger rawLogAction serverConfig@ServerConfig {..} communicate = withSocketsDo $ do
  let logAction = cfilter (\msg -> if debugLogging then True else severity msg /= DEBUG) rawLogAction
  logAction <& (mkDebugMessage $ (T.pack ("Running DAP server on " <> show port <> "...")))
  appStore <- newTVarIO mempty
  serve (Host host) (show port) $ \(socket, address) -> do
    logAction <& mkDebugMessage (T.pack ("TCP connection established from " ++ show address))
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
    adaptorStateMVar <- initAdaptorState logAction handle address appStore serverConfig
    serviceClient communicate adaptorStateMVar
      `catch` exceptionHandler logAction handle address debugLogging

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
serviceClient
  :: (Command -> Adaptor app Request ())
  -> AdaptorLocal app r
  -> IO ()
serviceClient communicate lcl = forever $ runAdaptorWith lcl st $ do
    nextRequest <- getRequest
    withRequest nextRequest (communicate (command nextRequest))
  where
    st = AdaptorState MessageTypeResponse []
----------------------------------------------------------------------------
-- | Handle exceptions from client threads, parse and log accordingly
exceptionHandler :: LogAction IO DAPLog -> Handle -> SockAddr -> Bool -> SomeException -> IO ()
exceptionHandler logAction handle address shouldLog (e :: SomeException) = do
  let
    dumpError
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
getRequest :: Adaptor app r Request
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
          logError (T.pack couldn'tDecodeBody)
          liftIO $ throwIO (ParseException couldn'tDecodeBody)
        Right request ->
          pure request

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
