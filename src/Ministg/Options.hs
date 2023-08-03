-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Ministg.Options
-- Copyright   : (c) 2009-2012 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Command line option processing.
module Ministg.Options
  ( processOptions,
    programName,
    defaultMaxSteps,
    defaultEvalStyle,
    defaultTraceDir,
    Flag (..),
    EvalStyle (..),
    Dumped (..),
    probeFlags,
    probeFlagsFirst,
    existsFlag,
    getTraceDir,
    getMaxSteps,
    getEvalStyle,
  )
where

import Data.Char (isDigit, toLower)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

programName :: String
programName = "ministg"

-- This should really come from the cabal file somehow.
versionNumber :: String
versionNumber = "0.3"

versionInfo :: String
versionInfo = unwords [programName, "version", versionNumber]

processOptions :: [String] -> IO ([Flag], [String])
processOptions argv =
  case getOpt RequireOrder options argv of
    (_, [], []) -> do
      putStrLn $ usageInfo header options
      exitSuccess
    (flags, nonOpts, [])
      | existsFlag flags Help -> do
          putStrLn $ usageInfo header options
          exitSuccess
      | existsFlag flags Version -> do
          putStrLn versionInfo
          exitSuccess
      | otherwise -> return (flags, nonOpts)
    (_, _, errs) -> raiseError errs
  where
    header = "Usage: " ++ programName ++ " [OPTION...] file"
    failureMsg = programName ++ ": command line error.\n"
    raiseError errs = do
      hPutStrLn stderr $ failureMsg ++ concat errs ++ usageInfo header options
      exitFailure

probeFlags :: [Flag] -> (Flag -> Maybe a) -> [a]
probeFlags flags probe = mapMaybe probe flags

probeFlagsFirst :: [Flag] -> (Flag -> Maybe a) -> a -> a
probeFlagsFirst flags probe defaultValue
  | null probed = defaultValue
  | otherwise = head probed
  where
    probed = probeFlags flags probe

existsFlag :: [Flag] -> Flag -> Bool
existsFlag flags f =
  probeFlagsFirst flags probe False
  where
    probe someFlag = if f == someFlag then Just True else Nothing

data Flag
  = -- | Which evaluation rules to use (eval/apply or push enter)
    Style EvalStyle
  | -- | Turn tracing on.
    Trace
  | -- | Directory to save trace file.
    TraceDir String
  | -- | Maximum reduction steps to perform.
    MaxSteps Integer
  | -- | Include call stack in trace.
    CallStack
  | -- | Dump something out to debug the interpreter.
    Dump Dumped
  | -- | Do not automatically include the Prelude.
    NoPrelude
  | -- | Disable garbage collection.
    NoGC
  | -- | Print a help message and exit.
    Help
  | -- | Print the version number.
    Version
  | -- | Auto annotate the program with stack markers.
    Annotate
  deriving (Eq, Ord, Show)

data EvalStyle
  = EvalApply
  | PushEnter
  deriving (Eq, Ord, Show)

data Dumped
  = DumpAST
  | DumpParsed
  | DumpArity
  | DumpNothing
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
  [ Option ['s'] ["style"] (ReqArg mkStyle "STYLE") "evaluation STYLE to use (EA = eval apply, PE = push enter)",
    Option ['t'] ["trace"] (NoArg Trace) "record a trace of program evaluation",
    Option [] ["tracedir"] (ReqArg TraceDir "DIR") "directory (DIR) to store trace files",
    Option ['m'] ["maxsteps"] (ReqArg mkMaxSteps "STEPS") "maximum number of reduction STEPS to perform",
    Option ['c'] ["callstack"] (NoArg CallStack) "enable call stack tracing",
    Option [] ["nogc"] (NoArg NoGC) "disable garbage collector",
    Option ['d'] ["dump"] (ReqArg mkDumped "DUMPED") "output DUMPED for debugging purposes (ast, parsed, arity)",
    Option ['v'] ["version"] (NoArg Version) "show version number",
    Option ['h'] ["help"] (NoArg Help) "get help about using this program",
    Option ['a'] ["annotate"] (NoArg Annotate) "automatically annotate the program with stack markers"
  ]

defaultTraceDir :: String
defaultTraceDir = "trace"

defaultEvalStyle :: EvalStyle
defaultEvalStyle = PushEnter

mkStyle :: String -> Flag
mkStyle = normalMkStyle . map toLower
  where
    normalMkStyle "ea" = Style EvalApply
    normalMkStyle "pe" = Style PushEnter
    normalMkStyle other = Style defaultEvalStyle

mkDumped :: String -> Flag
mkDumped = normalMkDumped . map toLower
  where
    normalMkDumped "ast" = Dump DumpAST
    normalMkDumped "parsed" = Dump DumpParsed
    normalMkDumped "arity" = Dump DumpArity
    normalMkDumped other = Dump DumpNothing

defaultMaxSteps :: Integer
defaultMaxSteps = 1000

mkMaxSteps :: String -> Flag
mkMaxSteps [] = MaxSteps defaultMaxSteps
mkMaxSteps n
  | all isDigit n = MaxSteps $ read n
  | otherwise = MaxSteps defaultMaxSteps

getMaxSteps :: [Flag] -> Integer
getMaxSteps flags =
  probeFlagsFirst flags probe defaultMaxSteps
  where
    probe (MaxSteps i) = Just i
    probe other = Nothing

getTraceDir :: [Flag] -> String
getTraceDir flags =
  probeFlagsFirst flags probe defaultTraceDir
  where
    probe (TraceDir d) = Just d
    probe other = Nothing

getEvalStyle :: [Flag] -> EvalStyle
getEvalStyle flags =
  probeFlagsFirst flags probe defaultEvalStyle
  where
    probe :: Flag -> Maybe EvalStyle
    probe (Style style) = Just style
    probe other = Nothing
