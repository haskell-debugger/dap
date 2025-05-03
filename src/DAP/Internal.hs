----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Internal
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
-- Description :  Internal functions for consumption by other modules like Server.hs
----------------------------------------------------------------------------
module DAP.Internal
  ( withLock
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent
----------------------------------------------------------------------------
-- | Used for performing actions (e.g. printing debug logs to stdout)
-- Also used for writing to each connections Handle.
-- Ensures operations occur one thread at a time.
--
-- Used internally only
--
withLock :: MVar () -> IO () -> IO ()
withLock mvar action = modifyMVar_ mvar $ \x -> x <$ action
----------------------------------------------------------------------------
