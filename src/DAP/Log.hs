module DAP.Log (
    DebugStatus (..)
  , DAPLog(..)
  , LogAction(..)
  , Level(..)
  , (<&)
  , cmap
  , cfilter
  , mkDebugMessage
  , renderDAPLog
) where

import Data.Text (Text)
import           Network.Socket                  ( SockAddr )
import Colog.Core
import qualified Data.Text as T
import DAP.Utils

----------------------------------------------------------------------------
data Level = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq)
----------------------------------------------------------------------------
data DebugStatus = SENT | RECEIVED
  deriving (Show, Eq)

data DAPLog =
  DAPLog {
      severity :: Level
    , mDebugStatus :: Maybe DebugStatus
    , addr     :: SockAddr
    , message  :: Text
    }
  | GenericMessage { severity :: Level, message :: Text }

mkDebugMessage :: Text -> DAPLog
mkDebugMessage  = GenericMessage DEBUG

renderDAPLog :: DAPLog -> Text
renderDAPLog (GenericMessage _ t) = t
renderDAPLog (DAPLog level maybeDebug log_addr msg) = T.concat
      [ withBraces $ T.pack (show log_addr)
      , withBraces $ T.pack (show level)
      , maybe mempty (withBraces . T.pack . show) maybeDebug
      , msg
      ]

