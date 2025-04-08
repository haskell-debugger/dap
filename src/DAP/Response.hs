-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Response
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
module DAP.Response
  ( -- * Response message API
    sendAttachResponse
  , sendBreakpointLocationsResponse
  , sendCompletionsResponse
  , sendConfigurationDoneResponse
  , sendContinueResponse
  , sendDataBreakpointInfoResponse
  , sendDisassembleResponse
  , sendDisconnectResponse
  , sendEvaluateResponse
  , sendExceptionInfoResponse
  , sendGotoResponse
  , sendGotoTargetsResponse
  , sendInitializeResponse
  , sendLaunchResponse
  , sendLoadedSourcesResponse
  , sendModulesResponse
  , sendNextResponse
  , sendPauseResponse
  , sendReadMemoryResponse
  , sendRestartResponse
  , sendRestartFrameResponse
  , sendReverseContinueResponse
  , sendScopesResponse
  , sendSetBreakpointsResponse
  , sendSetDataBreakpointsResponse
  , sendSetExceptionBreakpointsResponse
  , sendSetExpressionResponse
  , sendSetFunctionBreakpointsResponse
  , sendSetInstructionBreakpointsResponse
  , sendSetVariableResponse
  , sendSourceResponse
  , sendStackTraceResponse
  , sendStepBackResponse
  , sendStepInResponse
  , sendStepInTargetsResponse
  , sendStepOutResponse
  , sendTerminateResponse
  , sendTerminateThreadsResponse
  , sendThreadsResponse
  , sendVariablesResponse
  , sendWriteMemoryResponse
  , sendRunInTerminalResponse
  , sendStartDebuggingResponse
  ) where
----------------------------------------------------------------------------
import           DAP.Adaptor
import           DAP.Types
----------------------------------------------------------------------------
-- | AttachResponse has no body by default
sendAttachResponse :: Adaptor app Request ()
sendAttachResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | BreakpointLocationResponse has no body by default
sendBreakpointLocationsResponse
  :: [BreakpointLocation]
  -> Adaptor app Request ()
sendBreakpointLocationsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | 'SetDataBreakpointsResponse'
sendSetDataBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app Request ()
sendSetDataBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | BreakpointResponse has no body by default
sendSetBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app Request ()
sendSetBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetInstructionsBreakpointResponse has no body by default
sendSetInstructionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app Request ()
sendSetInstructionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetFunctionBreakpointResponse has no body by default
sendSetFunctionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app Request ()
sendSetFunctionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetExceptionBreakpointsResponse has no body by default
sendSetExceptionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app Request ()
sendSetExceptionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | ContinueResponse
sendContinueResponse
  :: ContinueResponse
  -> Adaptor app Request ()
sendContinueResponse continueResponse = do
  sendSuccesfulResponse (setBody continueResponse)
----------------------------------------------------------------------------
-- | ConfigurationDoneResponse
sendConfigurationDoneResponse
  :: Adaptor app Request ()
sendConfigurationDoneResponse = do
  sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | LaunchResponse
sendLaunchResponse
  :: Adaptor app Request ()
sendLaunchResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartResponse
sendRestartResponse
  :: Adaptor app Request ()
sendRestartResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | DisconnectResponse
sendDisconnectResponse
  :: Adaptor app Request ()
sendDisconnectResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateResponse
sendTerminateResponse
  :: Adaptor app Request ()
sendTerminateResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | NextResponse
sendNextResponse
  :: Adaptor app Request ()
sendNextResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepInResponse
sendStepInResponse
  :: Adaptor app Request ()
sendStepInResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepOutResponse
sendStepOutResponse
  :: Adaptor app Request ()
sendStepOutResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepBackResponse
sendStepBackResponse
  :: Adaptor app Request ()
sendStepBackResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | ReverseContinueResponse
sendReverseContinueResponse
  :: Adaptor app Request ()
sendReverseContinueResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartFrameResponse
sendRestartFrameResponse
  :: Adaptor app Request ()
sendRestartFrameResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | InitializeReponse
sendInitializeResponse
  :: Adaptor app Request ()
sendInitializeResponse = do
  capabilities <- getServerCapabilities
  sendSuccesfulResponse (setBody capabilities)
----------------------------------------------------------------------------
-- | GotoResponse
sendGotoResponse
  :: Adaptor app Request ()
sendGotoResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | GotoTargetsResponse
sendGotoTargetsResponse
  :: Adaptor app Request ()
sendGotoTargetsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | PauseResponse
sendPauseResponse
  :: Adaptor app Request ()
sendPauseResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateThreadsResponse
sendTerminateThreadsResponse
  :: Adaptor app Request ()
sendTerminateThreadsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
sendModulesResponse :: ModulesResponse -> Adaptor app Request ()
sendModulesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStackTraceResponse :: StackTraceResponse -> Adaptor app Request ()
sendStackTraceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSourceResponse :: SourceResponse -> Adaptor app Request ()
sendSourceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendThreadsResponse :: [Thread] -> Adaptor app Request ()
sendThreadsResponse = sendSuccesfulResponse . setBody . ThreadsResponse
----------------------------------------------------------------------------
sendLoadedSourcesResponse :: [Source] -> Adaptor app Request ()
sendLoadedSourcesResponse = sendSuccesfulResponse . setBody . LoadedSourcesResponse
----------------------------------------------------------------------------
sendWriteMemoryResponse :: WriteMemoryResponse -> Adaptor app Request ()
sendWriteMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendReadMemoryResponse :: ReadMemoryResponse -> Adaptor app Request ()
sendReadMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendCompletionsResponse :: CompletionsResponse -> Adaptor app Request ()
sendCompletionsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDataBreakpointInfoResponse :: DataBreakpointInfoResponse -> Adaptor app Request ()
sendDataBreakpointInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDisassembleResponse :: DisassembleResponse -> Adaptor app Request ()
sendDisassembleResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendEvaluateResponse :: EvaluateResponse -> Adaptor app Request ()
sendEvaluateResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendExceptionInfoResponse :: ExceptionInfoResponse -> Adaptor app Request ()
sendExceptionInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendScopesResponse :: ScopesResponse -> Adaptor app Request ()
sendScopesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetExpressionResponse :: SetExpressionResponse -> Adaptor app Request ()
sendSetExpressionResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetVariableResponse :: SetVariableResponse -> Adaptor app Request ()
sendSetVariableResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStepInTargetsResponse :: StepInTargetsResponse -> Adaptor app Request ()
sendStepInTargetsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendVariablesResponse :: VariablesResponse -> Adaptor app Request ()
sendVariablesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendRunInTerminalResponse :: RunInTerminalResponse -> Adaptor app Request ()
sendRunInTerminalResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStartDebuggingResponse :: Adaptor app Request ()
sendStartDebuggingResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
