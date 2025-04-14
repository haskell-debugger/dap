-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Event
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
----------------------------------------------------------------------------
module DAP.Event
  ( -- * Event message API
    sendBreakpointEvent
  , sendCapabilitiesEvent
  , sendContinuedEvent
  , sendExitedEvent
  , sendInitializedEvent
  , sendInvalidatedEvent
  , sendLoadedSourceEvent
  , sendMemoryEvent
  , sendModuleEvent
  , sendOutputEvent
  , sendProcessEvent
  , sendProgressEndEvent
  , sendProgressStartEvent
  , sendProgressUpdateEvent
  , sendStoppedEvent
  , sendTerminatedEvent
  , sendThreadEvent
  -- * Defaults
  , defaultContinuedEvent
  , defaultExitedEvent
  , defaultInvalidatedEvent
  , defaultMemoryEvent
  , defaultOutputEvent
  , defaultProcessEvent
  , defaultProgressEndEvent
  , defaultProgressStartEvent
  , defaultProgressUpdateEvent
  , defaultStoppedEvent
  , defaultTerminatedEvent
  , defaultThreadEvent
  ) where
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Adaptor
----------------------------------------------------------------------------
sendBreakpointEvent :: BreakpointEvent -> Adaptor app Request ()
sendBreakpointEvent = sendSuccesfulEvent EventTypeBreakpoint . setBody
----------------------------------------------------------------------------
sendCapabilitiesEvent :: CapabilitiesEvent -> Adaptor app Request ()
sendCapabilitiesEvent = sendSuccesfulEvent EventTypeCapabilities . setBody
----------------------------------------------------------------------------
sendContinuedEvent :: ContinuedEvent -> Adaptor app Request ()
sendContinuedEvent = sendSuccesfulEvent EventTypeContinued . setBody
----------------------------------------------------------------------------
defaultContinuedEvent :: ContinuedEvent
defaultContinuedEvent
  = ContinuedEvent
  { continuedEventThreadId            = 0
  , continuedEventAllThreadsContinued = False
  }
----------------------------------------------------------------------------
sendExitedEvent :: ExitedEvent -> Adaptor app Request ()
sendExitedEvent = sendSuccesfulEvent EventTypeExited . setBody
----------------------------------------------------------------------------
defaultExitedEvent :: ExitedEvent
defaultExitedEvent
  = ExitedEvent
  { exitedEventExitCode = 0
  }
----------------------------------------------------------------------------
sendInitializedEvent :: Adaptor app Request ()
sendInitializedEvent = sendSuccesfulEvent EventTypeInitialized (pure ())
----------------------------------------------------------------------------
sendInvalidatedEvent :: InvalidatedEvent -> Adaptor app Request ()
sendInvalidatedEvent = sendSuccesfulEvent EventTypeInvalidated . setBody
----------------------------------------------------------------------------
defaultInvalidatedEvent :: InvalidatedEvent
defaultInvalidatedEvent
  = InvalidatedEvent
  { invalidatedEventAreas         = []
  , invalidatedEventThreadId      = Nothing
  , invalidatedEventStackFrameId  = Nothing
  }

----------------------------------------------------------------------------
sendLoadedSourceEvent :: LoadedSourceEvent -> Adaptor app Request ()
sendLoadedSourceEvent = sendSuccesfulEvent EventTypeLoadedSource . setBody
----------------------------------------------------------------------------
sendMemoryEvent :: MemoryEvent -> Adaptor app Request ()
sendMemoryEvent = sendSuccesfulEvent EventTypeMemory . setBody
----------------------------------------------------------------------------
defaultMemoryEvent :: MemoryEvent
defaultMemoryEvent
  = MemoryEvent
  { memoryEventMemoryReference  = mempty
  , memoryEventOffset           = 0
  , memoryEventCount            = 0
  }
----------------------------------------------------------------------------
sendModuleEvent :: ModuleEvent -> Adaptor app Request ()
sendModuleEvent = sendSuccesfulEvent EventTypeModule . setBody
----------------------------------------------------------------------------
sendOutputEvent :: OutputEvent -> Adaptor app r ()
sendOutputEvent = sendSuccesfulEvent EventTypeOutput . setBody
----------------------------------------------------------------------------
defaultOutputEvent :: OutputEvent
defaultOutputEvent
  = OutputEvent
  { outputEventCategory           = Nothing
  , outputEventOutput             = mempty
  , outputEventGroup              = Nothing
  , outputEventVariablesReference = Nothing
  , outputEventSource             = Nothing
  , outputEventLine               = Nothing
  , outputEventColumn             = Nothing
  , outputEventData               = Nothing
  }
----------------------------------------------------------------------------
sendProcessEvent :: ProcessEvent -> Adaptor app Request ()
sendProcessEvent = sendSuccesfulEvent EventTypeProcess . setBody
----------------------------------------------------------------------------
defaultProcessEvent :: ProcessEvent
defaultProcessEvent
  = ProcessEvent
  { processEventName            = mempty
  , processEventSystemProcessId = Nothing
  , processEventIsLocalProcess  = True
  , processEventStartMethod     = Nothing
  , processEventPointerSize     = Nothing
  }
----------------------------------------------------------------------------
sendProgressEndEvent :: ProgressEndEvent -> Adaptor app Request ()
sendProgressEndEvent = sendSuccesfulEvent EventTypeProgressEnd . setBody
----------------------------------------------------------------------------
defaultProgressEndEvent :: ProgressEndEvent
defaultProgressEndEvent
  = ProgressEndEvent
  { progressEndEventProgressId  = mempty
  , progressEndEventMessage     = Nothing
  }
----------------------------------------------------------------------------
sendProgressStartEvent :: ProgressStartEvent -> Adaptor app Request ()
sendProgressStartEvent = sendSuccesfulEvent EventTypeProgressStart . setBody
----------------------------------------------------------------------------
defaultProgressStartEvent :: ProgressStartEvent
defaultProgressStartEvent
  = ProgressStartEvent
  { progressStartEventProgressId  = mempty
  , progressStartEventTitle       = mempty
  , progressStartEventRequestId   = Nothing
  , progressStartEventCancellable = False
  , progressStartEventMessage     = Nothing
  , progressStartEventPercentage  = Nothing
  }
----------------------------------------------------------------------------
sendProgressUpdateEvent :: ProgressUpdateEvent -> Adaptor app Request ()
sendProgressUpdateEvent = sendSuccesfulEvent EventTypeProgressUpdate . setBody
----------------------------------------------------------------------------
defaultProgressUpdateEvent :: ProgressUpdateEvent
defaultProgressUpdateEvent
  = ProgressUpdateEvent
  { progressUpdateEventProgressId = mempty
  , progressUpdateEventMessage    = Nothing
  , progressUpdateEventPercentage = Nothing
  }
----------------------------------------------------------------------------
sendStoppedEvent :: StoppedEvent -> Adaptor app Request ()
sendStoppedEvent = sendSuccesfulEvent EventTypeStopped . setBody
----------------------------------------------------------------------------
defaultStoppedEvent :: StoppedEvent
defaultStoppedEvent
  = StoppedEvent
  { stoppedEventReason            = StoppedEventReasonStep
  , stoppedEventDescription       = Nothing
  , stoppedEventThreadId          = Just 0
  , stoppedEventPreserveFocusHint = False
  , stoppedEventText              = Nothing
  , stoppedEventAllThreadsStopped = False
  , stoppedEventHitBreakpointIds  = []
  }
----------------------------------------------------------------------------
sendTerminatedEvent :: TerminatedEvent -> Adaptor app Request ()
sendTerminatedEvent = sendSuccesfulEvent EventTypeTerminated . setBody
----------------------------------------------------------------------------
defaultTerminatedEvent :: TerminatedEvent
defaultTerminatedEvent
  = TerminatedEvent
  { terminatedEventRestart = False
  }
----------------------------------------------------------------------------
sendThreadEvent :: ThreadEvent -> Adaptor app Request ()
sendThreadEvent = sendSuccesfulEvent EventTypeThread . setBody
----------------------------------------------------------------------------
defaultThreadEvent :: ThreadEvent
defaultThreadEvent
  = ThreadEvent
  { threadEventReason   = ThreadEventReasonStarted
  , threadEventThreadId = 0
  }
----------------------------------------------------------------------------

