{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Roboservant.Types.Config where

import Data.Dynamic
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Roboservant.Types.Internal (Provenance)
import Roboservant.Types.ReifiedApi (ApiOffset, InteractionError (..))

data Config
  = Config
      { seed :: [(Dynamic, Int)],
        maxRuntime :: Double, -- seconds to test for
        maxReps :: Integer,
        rngSeed :: Int,
        coverageThreshold :: Double,
        logInfo :: String -> IO (),
        databaseKey :: String,
        traceChecks :: [TraceCheck]
      }

data TraceResult
  = TraceSuccess (NonEmpty Dynamic)
  | TraceError InteractionError
  deriving (Show)

data CallOutcome
  = CallSucceeded [Text]
  | CallFailed InteractionError
  deriving (Show)

data CallSummary
  = CallSummary
      { csMethod :: Text,
        csStatus :: Maybe Int,
        csPathSegments :: [Text],
        csQueryItems :: [(Text, Text)],
        csHeaders :: [(Text, Text)],
        csBody :: [Text],
        csNotes :: [Text],
        csOutcome :: CallOutcome
      }
  deriving (Show)

data CallTrace
  = CallTrace
      { ctOffset :: ApiOffset,
        ctProvenance :: [Provenance],
        ctArguments :: [Dynamic],
        ctResult :: TraceResult,
        ctSummary :: CallSummary
      }
  deriving (Show)

data TraceCheck where
  TraceCheck ::
    (Typeable failure, Show failure) =>
    { traceCheckName :: String,
      traceCheck :: [CallTrace] -> IO (Maybe failure)
    } -> TraceCheck

emptySummary :: Text -> Maybe Int -> CallSummary
emptySummary method status =
  CallSummary
    { csMethod = method,
      csStatus = status,
      csPathSegments = [],
      csQueryItems = [],
      csHeaders = [],
      csBody = [],
      csNotes = [],
      csOutcome = CallSucceeded []
    }

appendPathSegment :: Text -> CallSummary -> CallSummary
appendPathSegment seg summary =
  summary {csPathSegments = csPathSegments summary <> [seg]}

appendQueryItem :: Text -> Text -> CallSummary -> CallSummary
appendQueryItem key value summary =
  summary {csQueryItems = csQueryItems summary <> [(key, value)]}

appendHeaderItem :: Text -> Text -> CallSummary -> CallSummary
appendHeaderItem key value summary =
  summary {csHeaders = csHeaders summary <> [(key, value)]}

appendBodyChunk :: Text -> CallSummary -> CallSummary
appendBodyChunk chunk summary =
  summary {csBody = csBody summary <> [chunk]}

appendNote :: Text -> CallSummary -> CallSummary
appendNote note summary =
  summary {csNotes = csNotes summary <> [note]}

setOutcome :: CallOutcome -> CallSummary -> CallSummary
setOutcome outcome summary = summary {csOutcome = outcome}

callSummaryLines :: CallSummary -> [Text]
callSummaryLines CallSummary {..} =
  firstLine : extras
  where
    pathTxt = "/" <> T.intercalate "/" csPathSegments
    queryTxt
      | null csQueryItems = ""
      | otherwise =
          let pairs = [key <> "=" <> value | (key, value) <- csQueryItems]
           in "?" <> T.intercalate "&" pairs
    baseLine = csMethod <> " " <> pathTxt <> queryTxt
    statusTxt = maybe "" (\code -> " " <> T.pack (show code)) csStatus
    (firstLine, responseLines) = case csOutcome of
      CallSucceeded vals ->
        (baseLine <> " ->" <> statusTxt <> " ok", map ("  response: " <>) vals)
      CallFailed err ->
        ( baseLine <> " -> ERROR " <> errorMessage err
            <> if fatalError err then " (fatal)" else "",
          []
        )
    headerLines = map (\(k, v) -> "  header " <> k <> ": " <> v) csHeaders
    bodyLines = map ("  body: " <>) csBody
    noteLines = map ("  note: " <>) csNotes
    extras = bodyLines <> responseLines <> headerLines <> noteLines

defaultConfig :: Config
defaultConfig =
  Config
    { seed = [],
      maxRuntime = 0.5,
      maxReps = 1000,
      rngSeed = 0,
      coverageThreshold = 0,
      logInfo = const (pure ()),
      databaseKey = "",
      traceChecks = []
    }

noisyConfig :: Config
noisyConfig = defaultConfig {logInfo = print}
