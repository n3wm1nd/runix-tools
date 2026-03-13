{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- | Tools for Runix Code agent
--
-- Each tool is just a function in Sem r that the LLM can call.
-- universal-llm handles dispatching via ToolFunction instances.
--
-- Design principles:
-- - Each tool has a unique result type (for ToolFunction instance)
-- - Newtypes for semantic parameters (not just Text everywhere)
-- - Functions run in Sem r with required effects
-- - Keep it simple - tools are just functions
module Runix.Tools
  ( -- * File Operations
    readFile
  , writeFile
  , editFile
  , getCwd
  , mkdir
  , remove
  , glob
  , grep
  , diff
  , diffContentVsFile
  , sedPrint

    -- * Shell
  , bash
  , cmd

    -- * Build Tools
  , cabalBuild

    -- * Meta
  , todoWrite
  , todoRead
  , todoCheck
  , todoDelete
  , Todo (..)

    -- * Result Types
  , ReadFileResult (..)
  , WriteFileResult (..)
  , EditFileResult (..)
  , MkdirResult (..)
  , RemoveResult (..)
  , GlobResult (..)
  , GrepResult (..)
  , DiffResult (..)
  , BashResult (..)
  , CmdResult (..)
  , CabalBuildResult (..)
  , TodoWriteResult (..)
  , TodoReadResult (..)
  , TodoCheckResult (..)
  , TodoDeleteResult (..)

    -- * Parameter Types
  , FilePath (..)
  , FileContent (..)
  , OldString (..)
  , NewString (..)
  , CreateParents (..)
  , Recursive (..)
  , Pattern (..)
  , Command (..)
  , TodoText (..)
  , WorkingDirectory (..)
  , Offset (..)
  , Limit (..)
  , AfterContext (..)
  , BeforeContext (..)
  , FileGlob (..)
  , SedExpression (..)
  , SedPrintResult (..)
  , CmdArgs (..)
  , GrepPatterns (..)
  , HeadLines (..)
  , TailLines (..)
  , FilterStderr (..)

    -- * Utilities
  , truncateResponse
  , maxResponseChars
  , maxResponseLines
  ) where

import Prelude hiding (readFile, writeFile, FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Polysemy (Sem, Member, Members)
import Polysemy.State (State, modify, get, put)
import Polysemy.Fail (Fail)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Tools (ToolFunction(..), ToolParameter(..))
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import qualified Runix.FileSystem as FileSystem
import Runix.Grep (Grep)
import qualified Runix.Grep
import Runix.Bash (Bash)
import qualified Runix.Bash
import qualified Runix.Cmd

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A single todo item with completion status
data Todo = Todo
  { todoText :: Text
  , todoCompleted :: Bool
  } deriving stock (Show, Eq)

instance HasCodec Todo where
  codec = Autodocodec.object "Todo" $
    Todo
      <$> Autodocodec.requiredField "text" "todo item text" Autodocodec..= todoText
      <*> Autodocodec.requiredField "completed" "completion status" Autodocodec..= todoCompleted

instance ToolParameter Todo where
  paramName = "todo"
  paramDescription = "a todo item"

--------------------------------------------------------------------------------
-- Parameter Newtypes
--------------------------------------------------------------------------------

newtype FilePath = FilePath Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype FileContent = FileContent Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype OldString = OldString Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype NewString = NewString Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype CreateParents = CreateParents Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

newtype Recursive = Recursive Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

newtype Pattern = Pattern Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype Command = Command Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype TodoText = TodoText Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype WorkingDirectory = WorkingDirectory Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype Offset = Offset Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype Limit = Limit Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype AfterContext = AfterContext Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype BeforeContext = BeforeContext Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype FileGlob = FileGlob Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype SedExpression = SedExpression Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype CmdArgs = CmdArgs [Text]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Text]

newtype GrepPatterns = GrepPatterns [Text]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Text]

newtype HeadLines = HeadLines Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype TailLines = TailLines Int
  deriving stock (Show, Eq)
  deriving (HasCodec) via Int

newtype FilterStderr = FilterStderr Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

-- ToolParameter instances for parameters
instance ToolParameter GetCwdResult where
  paramName = "cwd"
  paramDescription = "the current working directory"

instance ToolParameter FilePath where
  paramName = "file_path"
  paramDescription = "absolute path to the file"

instance ToolParameter FileContent where
  paramName = "content"
  paramDescription = "content to write to the file"

instance ToolParameter OldString where
  paramName = "old_string"
  paramDescription = "exact string to find and replace"

instance ToolParameter NewString where
  paramName = "new_string"
  paramDescription = "string to replace with"

instance ToolParameter CreateParents where
  paramName = "create_parents"
  paramDescription = "whether to create parent directories if they don't exist"

instance ToolParameter Recursive where
  paramName = "recursive"
  paramDescription = "whether to remove directories recursively"

instance ToolParameter Pattern where
  paramName = "pattern"
  paramDescription = "glob or regex pattern to match"

instance ToolParameter Command where
  paramName = "command"
  paramDescription = "shell command to execute"

instance ToolParameter TodoText where
  paramName = "text"
  paramDescription = "text or prefix of the todo item"

instance ToolParameter WorkingDirectory where
  paramName = "working_directory"
  paramDescription = "directory to run the command in"

instance ToolParameter Offset where
  paramName = "offset"
  paramDescription = "1-based line number to start reading from (default: 1)"

instance ToolParameter Limit where
  paramName = "limit"
  paramDescription = "number of lines to return (default: all remaining lines)"

instance ToolParameter AfterContext where
  paramName = "after_context"
  paramDescription = "number of lines to show after each match"

instance ToolParameter BeforeContext where
  paramName = "before_context"
  paramDescription = "number of lines to show before each match"

instance ToolParameter FileGlob where
  paramName = "glob"
  paramDescription = "glob pattern to filter files (e.g., '*.hs', '*.{ts,tsx}')"

instance ToolParameter SedExpression where
  paramName = "expression"
  paramDescription = "sed-style expression: line numbers (5p), ranges (10,20p), relative (5,+10p), patterns (/text/p), combinations (5,/end/p, /start/,/end/p, /start/,+5p). Use // for all lines. Separate multiple with ; (5p;10,20p)"

instance ToolParameter CmdArgs where
  paramName = "args"
  paramDescription = "command line arguments"

instance ToolParameter GrepPatterns where
  paramName = "grep"
  paramDescription = "filter output to lines containing any of these patterns (empty list = no filtering)"

instance ToolParameter HeadLines where
  paramName = "head"
  paramDescription = "take only the first N lines of output"

instance ToolParameter TailLines where
  paramName = "tail"
  paramDescription = "take only the last N lines of output"

instance ToolParameter FilterStderr where
  paramName = "filter_stderr"
  paramDescription = "apply filters to stderr as well as stdout (default: false, stderr passes through unfiltered like Unix pipes)"

--------------------------------------------------------------------------------
-- Result Types (unique for ToolFunction instances)
--------------------------------------------------------------------------------
newtype GetCwdResult = GetCwdResult FilePath
  deriving stock (Show, Eq)

instance HasCodec GetCwdResult where
  codec = Autodocodec.dimapCodec GetCwdResult (\(GetCwdResult t) -> t) codec

newtype ReadFileResult = ReadFileResult Text
  deriving stock (Show, Eq)

instance HasCodec ReadFileResult where
  codec = Autodocodec.dimapCodec ReadFileResult (\(ReadFileResult t) -> t) codec

newtype WriteFileResult = WriteFileResult Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

data EditFileResult = EditFileResult
  { editSuccess :: Bool
  , editMessage :: Text
  } deriving stock (Show, Eq)

instance HasCodec EditFileResult where
  codec = Autodocodec.object "EditFileResult" $
    EditFileResult
      <$> Autodocodec.requiredField "success" "whether the edit succeeded" Autodocodec..= editSuccess
      <*> Autodocodec.requiredField "message" "description of what happened" Autodocodec..= editMessage

newtype MkdirResult = MkdirResult Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

newtype RemoveResult = RemoveResult Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

data GlobResult = GlobResult
  { globResults :: [Text]   -- ^ The file paths being returned
  , globMatches :: Int      -- ^ Total number of matches found
  , globShown :: Int        -- ^ Number of matches shown (length of results)
  } deriving stock (Show, Eq)

instance HasCodec GlobResult where
  codec = Autodocodec.object "GlobResult" $
    GlobResult
      <$> Autodocodec.requiredField "results" "matching file paths" Autodocodec..= globResults
      <*> Autodocodec.requiredField "matches" "total number of matches found" Autodocodec..= globMatches
      <*> Autodocodec.requiredField "shown" "number of matches shown in results" Autodocodec..= globShown

newtype GrepResult = GrepResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype DiffResult = DiffResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype BashResult = BashResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Result from cmd execution - parametrized by command name for unique ToolFunction instances
data CmdResult (command :: Symbol) = CmdResult
  { cmdExitCode :: Int
  , cmdStdout :: Text
  , cmdStderr :: Text
  } deriving stock (Show, Eq)

instance HasCodec (CmdResult command) where
  codec = Autodocodec.object "CmdResult" $
    CmdResult
      <$> Autodocodec.requiredField "exit_code" "command exit code" Autodocodec..= cmdExitCode
      <*> Autodocodec.requiredField "stdout" "filtered stdout" Autodocodec..= cmdStdout
      <*> Autodocodec.requiredField "stderr" "stderr (filtered if filter_stderr=true, otherwise unfiltered)" Autodocodec..= cmdStderr

-- | Result from cabal build - returns success status and output
data CabalBuildResult = CabalBuildResult
  { buildSuccess :: Bool
  , buildOutput :: Text
  , buildErrors :: Text
  } deriving stock (Show, Eq)

instance HasCodec CabalBuildResult where
  codec = Autodocodec.object "CabalBuildResult" $
    CabalBuildResult
      <$> Autodocodec.requiredField "success" "whether build succeeded" Autodocodec..= buildSuccess
      <*> Autodocodec.requiredField "output" "build stdout" Autodocodec..= buildOutput
      <*> Autodocodec.requiredField "errors" "build stderr" Autodocodec..= buildErrors

-- | Result from todo_write - unit type (nothing to return to LLM)
-- State effect handles the actual todo list mutation
data TodoWriteResult = TodoWriteResult
  deriving stock (Show, Eq)

instance HasCodec TodoWriteResult where
  codec = Autodocodec.dimapCodec (const TodoWriteResult) (const ()) Autodocodec.nullCodec

-- | Result from todo_read - returns the list of todos
newtype TodoReadResult = TodoReadResult [Todo]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Todo]

-- | Result from todo_check - returns a message about what happened
newtype TodoCheckResult = TodoCheckResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Result from todo_delete - returns a message about what happened
newtype TodoDeleteResult = TodoDeleteResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype SedPrintResult = SedPrintResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- ToolParameter instances for result types (required by ToolFunction)
instance ToolParameter ReadFileResult where
  paramName = "read_file_result"
  paramDescription = "file contents"

instance ToolParameter WriteFileResult where
  paramName = "write_file_result"
  paramDescription = "write success status"

instance ToolParameter EditFileResult where
  paramName = "edit_file_result"
  paramDescription = "edit result with success status and message"

instance ToolParameter MkdirResult where
  paramName = "mkdir_result"
  paramDescription = "mkdir success status"

instance ToolParameter RemoveResult where
  paramName = "remove_result"
  paramDescription = "remove success status"

instance ToolParameter GlobResult where
  paramName = "glob_result"
  paramDescription = "list of matching file paths"

instance ToolParameter GrepResult where
  paramName = "grep_result"
  paramDescription = "search results"

instance ToolParameter DiffResult where
  paramName = "diff_result"
  paramDescription = "unified diff output showing differences between files"

instance ToolParameter BashResult where
  paramName = "bash_result"
  paramDescription = "command output"

instance ToolParameter (CmdResult command) where
  paramName = "cmd_result"
  paramDescription = "command execution result with exit code and filtered output"

instance ToolParameter CabalBuildResult where
  paramName = "cabal_build_result"
  paramDescription = "cabal build result with success status and output"

instance ToolParameter TodoWriteResult where
  paramName = "result"
  paramDescription = "todo write result (always succeeds)"

instance ToolParameter TodoReadResult where
  paramName = "todos"
  paramDescription = "list of all todos"

instance ToolParameter TodoCheckResult where
  paramName = "result"
  paramDescription = "message describing what happened (todo checked, no match found, or multiple matches)"

instance ToolParameter TodoDeleteResult where
  paramName = "result"
  paramDescription = "message describing what happened (todo deleted, no match found, or multiple matches)"

instance ToolParameter SedPrintResult where
  paramName = "sed_print_result"
  paramDescription = "file lines matching the sed expression, with line numbers"

-- ToolFunction instances for result types
instance ToolFunction GetCwdResult where
  toolFunctionName = "getcwd"
  toolFunctionDescription = "Get the current working directory"

instance ToolFunction ReadFileResult where
  toolFunctionName = "read_file"
  toolFunctionDescription = "Read a file from the filesystem and return its contents"

instance ToolFunction WriteFileResult where
  toolFunctionName = "write_file"
  toolFunctionDescription = "Write content to a new file or overwrite existing file"

instance ToolFunction EditFileResult where
  toolFunctionName = "edit_file"
  toolFunctionDescription = "Edit an existing file by replacing old_string with new_string"

instance ToolFunction MkdirResult where
  toolFunctionName = "mkdir"
  toolFunctionDescription = "Create a directory, optionally creating parent directories"

instance ToolFunction RemoveResult where
  toolFunctionName = "remove"
  toolFunctionDescription = "Remove a file or directory, optionally recursive"

instance ToolFunction GlobResult where
  toolFunctionName = "glob"
  toolFunctionDescription = "Find files matching a glob pattern"

instance ToolFunction GrepResult where
  toolFunctionName = "grep"
  toolFunctionDescription = "Search file contents using regex pattern"

instance ToolFunction DiffResult where
  toolFunctionName = "diff"
  toolFunctionDescription = "Show differences between two files using unified diff format"

instance ToolFunction BashResult where
  toolFunctionName = "bash"
  toolFunctionDescription = "Execute a bash command and return output"

instance KnownSymbol command => ToolFunction (CmdResult command) where
  toolFunctionName = "call_" <> T.pack (symbolVal (Proxy @command))
  toolFunctionDescription = "Execute " <> T.pack (symbolVal (Proxy @command)) <> " command with optional output filtering. Filters (grep patterns, head/tail) are applied to stdout (and stderr if filter_stderr=true). Grep uses OR logic (match any pattern). Filters apply in order: grep → head → tail."

instance ToolFunction CabalBuildResult where
  toolFunctionName = "cabal_build"
  toolFunctionDescription = "Run cabal build in a specified directory and return build results"

instance ToolFunction TodoWriteResult where
  toolFunctionName = "todo_write"
  toolFunctionDescription = "Add a new todo item to the list"

instance ToolFunction TodoReadResult where
  toolFunctionName = "todo_read"
  toolFunctionDescription = "Read all current todos with their completion status"

instance ToolFunction TodoCheckResult where
  toolFunctionName = "todo_check"
  toolFunctionDescription = "Mark a todo as completed by text prefix (must match exactly one todo)"

instance ToolFunction TodoDeleteResult where
  toolFunctionName = "todo_delete"
  toolFunctionDescription = "Delete a todo by text prefix (must match exactly one todo)"

instance ToolFunction SedPrintResult where
  toolFunctionName = "sed_print"
  toolFunctionDescription = "Print specific line ranges from a file using sed-style expressions. Supports: line numbers (5p), ranges (10,20p), relative ranges (5,+10p for 10 lines from line 5), pattern matching (/text/p for lines containing 'text'), and combinations (5,/end/p, /start/,/end/p, /start/,+5p). Empty pattern (//) matches all lines. Multiple expressions can be separated by semicolons (5p;10,20p)."

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Truncation limits for LLM responses
maxResponseChars :: Int
maxResponseChars = 30000

maxResponseLines :: Int
maxResponseLines = 2000

-- | Truncate text response to limits, appending indicator if truncated
truncateResponse :: Text -> Text
truncateResponse t =
  let lines_ = T.lines t
      totalLines = length lines_
      totalChars = T.length t
      -- Apply line limit first
      (truncLines, linesTruncated, linesSkipped) =
        if totalLines > maxResponseLines
        then (take maxResponseLines lines_, True, totalLines - maxResponseLines)
        else (lines_, False, 0)
      joined = T.unlines truncLines
      -- Then apply char limit
      (result, charsTruncated, charsSkipped) =
        if T.length joined > maxResponseChars
        then (T.take maxResponseChars joined, True, T.length joined - maxResponseChars)
        else (joined, False, 0)
  in if linesTruncated || charsTruncated
     then result <> "\n\n[Response truncated. "
          <> (if linesTruncated
              then "Skipped " <> T.pack (show linesSkipped) <> " lines (showing first "
                   <> T.pack (show maxResponseLines) <> " of " <> T.pack (show totalLines) <> " total). "
              else "")
          <> (if charsTruncated
              then "Skipped " <> T.pack (show charsSkipped) <> " characters (showing first "
                   <> T.pack (show maxResponseChars) <> " of " <> T.pack (show totalChars) <> " total). "
              else "")
          <> "Use offset/limit parameters or more specific queries to see more.]"
     else t

--------------------------------------------------------------------------------
-- File Operations
--------------------------------------------------------------------------------

-- | Get current working directory

getCwd :: forall project r. (Members [FileSystem project, Fail] r) => Sem r GetCwdResult
getCwd  = do
  cwd <- FileSystem.getCwd @project
  return $ GetCwdResult (FilePath . T.pack $ cwd)


-- | Read a file from the filesystem with optional offset/limit and line numbering
readFile
  :: forall project r. (Members [FileSystemRead project, Fail] r)
  => FilePath
  -> Maybe Offset    -- ^ Optional 1-based line number to start from
  -> Maybe Limit     -- ^ Optional number of lines to return
  -> Sem r ReadFileResult
readFile (FilePath path) maybeOffset maybeLimit = do
  contents <- FileSystem.readFile @project (T.unpack path)
  let contentText = T.decodeUtf8 contents
      allLines = T.lines contentText
      totalLines = length allLines

      -- Extract offset and limit values (defaults: offset=1, limit=all)
      offsetVal = case maybeOffset of
        Just (Offset o) -> max 1 o  -- Ensure offset is at least 1
        Nothing -> 1
      limitVal = case maybeLimit of
        Just (Limit l) -> l
        Nothing -> totalLines

      -- Calculate 0-indexed start position
      startIdx = offsetVal - 1

      -- Select the range of lines
      selectedLines = take limitVal $ drop startIdx allLines
      endLine = startIdx + length selectedLines

      -- Add line numbers (using cat -n format)
      numberedLines = zipWith (\lineNum line -> T.pack (show lineNum) <> "\t" <> line)
                              [offsetVal..endLine]
                              selectedLines

      -- Add header if we're showing a subset
      header = if offsetVal > 1 || endLine < totalLines
               then "[Lines " <> T.pack (show offsetVal) <> "-" <> T.pack (show endLine)
                    <> " of " <> T.pack (show totalLines) <> " total]\n"
               else ""

      result = header <> T.unlines numberedLines

  return $ ReadFileResult (truncateResponse result)

-- | Write a new file
-- Fails if write operation cannot be completed
writeFile
  :: forall project r. Members [FileSystemWrite project, Fail] r
  => FilePath
  -> FileContent
  -> Sem r WriteFileResult
writeFile (FilePath path) (FileContent content) = do
  let bytes = T.encodeUtf8 content
  FileSystem.writeFile @project (T.unpack path) bytes
  return $ WriteFileResult True

-- | Edit existing file via string replacement
-- Returns error if old_string matches 0 or >1 times (must match exactly once)
-- Fails if file cannot be read or written
editFile
  :: forall project r. Members [FileSystemRead project, FileSystemWrite project, Fail] r
  => FilePath
  -> OldString
  -> NewString
  -> Sem r EditFileResult
editFile (FilePath path) (OldString old) (NewString new) = do
  contents <- FileSystem.readFile @project (T.unpack path)
  let contentText = T.decodeUtf8 contents
      (replaced, occurrences) = replaceAndCount old new contentText
  case occurrences of
    0 -> return $ EditFileResult False $
           "Error: old_string not found in file. No changes made."
    1 -> do
      let newBytes = T.encodeUtf8 replaced
      FileSystem.writeFile @project (T.unpack path) newBytes
      return $ EditFileResult True $
        "Successfully replaced 1 occurrence in " <> path
    n -> return $ EditFileResult False $
           "Error: old_string appears " <> T.pack (show n) <> " times in file. " <>
           "To avoid unintended changes, old_string must match exactly once. " <>
           "Please make old_string more specific."

-- | Replace all occurrences and count how many replacements were made
replaceAndCount :: Text -> Text -> Text -> (Text, Int)
replaceAndCount old new haystack
  | T.null old = (haystack, 0)
  | otherwise = go [] 0 haystack
  where
    go acc count text =
      case T.breakOn old text of
        (_, rest) | T.null rest -> (T.concat (reverse (text : acc)), count)
        (before, rest) ->
          let after = T.drop (T.length old) rest
          in go (new : before : acc) (count + 1) after

-- | Create a directory
-- Fails if directory creation fails
mkdir
  :: forall project r. Members [FileSystemWrite project, Fail] r
  => FilePath
  -> CreateParents
  -> Sem r MkdirResult
mkdir (FilePath path) (CreateParents createParents) = do
  FileSystem.createDirectory @project createParents (T.unpack path)
  return $ MkdirResult True

-- | Remove a file or directory
-- Fails if removal fails
remove
  :: forall project r. Members [FileSystemWrite project, Fail] r
  => FilePath
  -> Recursive
  -> Sem r RemoveResult
remove (FilePath path) (Recursive recursive) = do
  FileSystem.remove @project recursive (T.unpack path)
  return $ RemoveResult True

-- | Find files matching a pattern with fixed result limit
-- Returns structured result with total matches and shown count
-- For internal use where you need the complete list, use FileSystem.glob directly
glob
  :: forall project r. Members [FileSystem project, Fail] r
  => Pattern
  -> Sem r GlobResult
glob (Pattern pattern) = do
  -- Glob from current directory
  files <- FileSystem.glob @project "." (T.unpack pattern)
  let allFiles = map T.pack files
      totalMatches = length allFiles
      limitVal = maxResponseLines  -- Fixed reasonable default: 2000

      resultFiles = take limitVal allFiles
      shownCount = length resultFiles

  return $ GlobResult
    { globResults = resultFiles
    , globMatches = totalMatches
    , globShown = shownCount
    }

-- | Search file contents with regex, with optional context and glob filtering
-- Parameterized by filesystem/project to work with chrooted filesystems
grep
  :: forall project r. Member (Runix.Grep.Grep project) r
  => Pattern
  -> Maybe AfterContext
  -> Maybe BeforeContext
  -> Maybe FileGlob
  -> Sem r GrepResult
grep (Pattern pattern) maybeAfter maybeBefore maybeGlob = do
  -- Build GrepOptions from Maybe parameters
  let opts = Runix.Grep.GrepOptions
        { Runix.Grep.grepAfterContext = case maybeAfter of
            Just (AfterContext n) -> Just n
            Nothing -> Nothing
        , Runix.Grep.grepBeforeContext = case maybeBefore of
            Just (BeforeContext n) -> Just n
            Nothing -> Nothing
        , Runix.Grep.grepGlob = case maybeGlob of
            Just (FileGlob g) -> Just (T.unpack g)
            Nothing -> Nothing
        }
  -- Grep from current directory with options
  matches <- Runix.Grep.grepSearch @project "." (T.unpack pattern) opts
  -- Format matches as text
  let formatted = T.intercalate "\n" $
        map (\m -> T.pack (Runix.Grep.matchFile m) <> ":" <>
                   T.pack (show $ Runix.Grep.matchLine m) <> ":" <>
                   Runix.Grep.matchText m) matches
  return $ GrepResult (truncateResponse formatted)

-- | Run unified diff between two files
-- Fails if either file doesn't exist (uses Fail effect)
diff
  :: forall project r. Members '[FileSystem project, FileSystemRead project, Runix.Cmd.Cmd "diff", Fail] r
  => FilePath
  -> FilePath
  -> Sem r DiffResult
diff (FilePath file1) (FilePath file2) = do
  -- Verify files exist (for safety)
  exists1 <- FileSystem.fileExists @project (T.unpack file1)
  exists2 <- FileSystem.fileExists @project (T.unpack file2)

  if not exists1
    then fail $ "File does not exist: " ++ T.unpack file1
    else if not exists2
    then fail $ "File does not exist: " ++ T.unpack file2
    else do
      -- Run diff command with unified format (-u)
      -- diff returns exit code 0 if files are same, 1 if different, 2 on error
      output <- Runix.Cmd.call @"diff" ["-u", T.unpack file1, T.unpack file2]
      -- Check if diff failed (exit code 2 means error)
      if Runix.Cmd.exitCode output == 2
        then fail $ "diff command failed: " ++ T.unpack (Runix.Cmd.stderr output)
        else return $ DiffResult $ Runix.Cmd.stdout output

-- | Run unified diff between old content (via stdin) and a file
-- The label is used for the "old" file name in the diff output
diffContentVsFile
  :: forall project r. Members '[FileSystem project, FileSystemRead project, Runix.Cmd.Cmd "diff", Fail] r
  => String           -- ^ Label for old content (e.g., "path/to/file.old")
  -> ByteString       -- ^ Old content
  -> FilePath         -- ^ Path to current file
  -> Sem r DiffResult
diffContentVsFile label oldContent (FilePath file) = do
  -- Verify file exists
  exists <- FileSystem.fileExists @project (T.unpack file)
  if not exists
    then fail $ "File does not exist: " ++ T.unpack file
    else do
      -- Run diff with old content via stdin
      -- Use --label to set the filename for stdin content, and - to read from stdin
      output <- Runix.Cmd.callStdin @"diff"
                  ["-u", "--label", label, "-", T.unpack file]
                  oldContent
      -- Check if diff failed (exit code 2 means error)
      if Runix.Cmd.exitCode output == 2
        then fail $ "diff command failed: " ++ T.unpack (Runix.Cmd.stderr output)
        else return $ DiffResult $ Runix.Cmd.stdout output

--------------------------------------------------------------------------------
-- Shell Operations
--------------------------------------------------------------------------------

-- | Execute a bash command
bash
  :: Member Bash r
  => Command
  -> Sem r BashResult
bash (Command bashCmd) = do
  output <- Runix.Bash.bashExec (T.unpack bashCmd)
  -- Format output with stdout and stderr
  let result = if Runix.Bash.exitCode output == 0
               then Runix.Bash.stdout output
               else Runix.Bash.stdout output <> "\nSTDERR:\n" <> Runix.Bash.stderr output
  return $ BashResult (truncateResponse result)

-- | Execute an arbitrary command with optional output filtering
-- The command name is specified at the type level (e.g., @"sendmail"@)
-- This allows type-safe command execution with the Cmd effect
--
-- Filtering is applied to stdout (and stderr if filter_stderr=True) in this order:
-- 1. Filter by grep patterns (OR logic - keep lines matching any pattern)
-- 2. Apply head (take first N lines)
-- 3. Apply tail (take last N lines)
--
-- If filter_stderr is False (default), stderr passes through unfiltered (Unix pipe behavior)
--
-- Example: @cmd \@\"git\" (CmdArgs [\"status\"]) Nothing (GrepPatterns [\"modified\"]) (Just $ HeadLines 10) Nothing (FilterStderr False)@
cmd
  :: forall command r. Member (Runix.Cmd.Cmd command) r
  => CmdArgs                    -- ^ Command arguments
  -> Maybe WorkingDirectory     -- ^ Working directory (default: current)
  -> GrepPatterns               -- ^ Grep patterns to filter output (empty = no filtering)
  -> Maybe HeadLines            -- ^ Take first N lines
  -> Maybe TailLines            -- ^ Take last N lines
  -> FilterStderr               -- ^ Apply filters to stderr too (default: false)
  -> Sem r (CmdResult command)
cmd (CmdArgs args) maybeWorkDir (GrepPatterns patterns) maybeHead maybeTail (FilterStderr filterStderr) = do
  -- Execute the command
  let workDir = case maybeWorkDir of
        Just (WorkingDirectory wd) -> T.unpack wd
        Nothing -> "."
  output <- Runix.Cmd.callIn @command workDir (map T.unpack args)

  -- Build filter pipeline: grep → head → tail
  let applyGrep text = if null patterns
                       then text
                       else T.unlines . filter (\line -> any (`T.isInfixOf` line) patterns) . T.lines $ text
      applyHead text = case maybeHead of
        Just (HeadLines n) -> T.unlines . take n . T.lines $ text
        Nothing -> text
      applyTail text = case maybeTail of
        Just (TailLines n) -> let ls = T.lines text in T.unlines . drop (length ls - n) $ ls
        Nothing -> text

      filterPipeline = applyTail . applyHead . applyGrep

      filteredStdout = filterPipeline (Runix.Cmd.stdout output)
      filteredStderr = if filterStderr
                       then filterPipeline (Runix.Cmd.stderr output)
                       else Runix.Cmd.stderr output

  return $ CmdResult
    { cmdExitCode = Runix.Cmd.exitCode output
    , cmdStdout = truncateResponse filteredStdout
    , cmdStderr = truncateResponse filteredStderr
    }

-- | Run cabal build in a specified directory
cabalBuild
  :: Member (Runix.Cmd.Cmd "cabal") r
  => WorkingDirectory
  -> Sem r CabalBuildResult
cabalBuild (WorkingDirectory workDir) = do
  output <- Runix.Cmd.callIn @"cabal" (T.unpack workDir) ["build", "all"]
  let success = Runix.Cmd.exitCode output == 0
      stdout = truncateResponse $ Runix.Cmd.stdout output
      stderr = truncateResponse $ Runix.Cmd.stderr output
  return $ CabalBuildResult success stdout stderr

--------------------------------------------------------------------------------
-- Meta Operations
--------------------------------------------------------------------------------

-- | Add a todo to the list using State effect
-- Tool mutates state directly, returns unit to LLM
todoWrite
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoWriteResult
todoWrite (TodoText text) = do
  let newTodo = Todo { todoText = text, todoCompleted = False }
  modify (newTodo :)
  return TodoWriteResult

-- | Read all todos from the state
todoRead
  :: Member (State [Todo]) r
  => Sem r TodoReadResult
todoRead = do
  todos <- get @[Todo]
  -- Return in reverse order so newest todos are at the end (more natural)
  return $ TodoReadResult (reverse todos)

-- | Mark a todo as completed by text prefix
-- Returns a message indicating success, no match, or multiple matches
todoCheck
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoCheckResult
todoCheck (TodoText prefix) = do
  todos <- get @[Todo]
  let matches = filter (\todo -> prefix `T.isPrefixOf` todoText todo) todos
  case matches of
    [] -> return $ TodoCheckResult "No todo found matching that prefix"
    [matchedTodo] -> do
      -- Update the single matching todo
      let updatedTodos = map
            (\todo -> if todoText todo == todoText matchedTodo
                      then todo { todoCompleted = True }
                      else todo)
            todos
      put @[Todo] updatedTodos
      return $ TodoCheckResult $ "Checked off: " <> todoText matchedTodo
    multiple -> do
      let matchTexts = T.intercalate ", " (map todoText multiple)
      return $ TodoCheckResult $ "Multiple matches found: " <> matchTexts

-- | Delete a todo by text prefix
-- Returns a message indicating success, no match, or multiple matches
todoDelete
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoDeleteResult
todoDelete (TodoText prefix) = do
  todos <- get @[Todo]
  let matches = filter (\todo -> prefix `T.isPrefixOf` todoText todo) todos
  case matches of
    [] -> return $ TodoDeleteResult "No todo found matching that prefix"
    [matchedTodo] -> do
      -- Remove the single matching todo
      let updatedTodos = filter (\todo -> todoText todo /= todoText matchedTodo) todos
      put @[Todo] updatedTodos
      return $ TodoDeleteResult $ "Deleted: " <> todoText matchedTodo
    multiple -> do
      let matchTexts = T.intercalate ", " (map todoText multiple)
      return $ TodoDeleteResult $ "Multiple matches found: " <> matchTexts

-- | Print specific line ranges from a file using sed-style expressions
-- Supports:
--   - Line numbers: 5p (line 5)
--   - Ranges: 10,20p (lines 10-20)
--   - Relative: 5,+10p (10 lines starting from line 5)
--   - Pattern matching: /text/p (all lines containing "text")
--   - Empty pattern: //p (all lines)
--   - Combinations: 5,/end/p (from line 5 to first line containing "end")
--                   /start/,/end/p (from each "start" to next "end" - repeating ranges!)
--                   /start/,+5p (for EACH line matching "start", print it + 5 lines after)
--   - Multiple: 5p;10,20p (semicolon-separated)
--
-- Note: When a range starts with a pattern (/pattern/,...), it creates repeating ranges
-- (standard sed semantics) - the range is applied for each match of the pattern.
sedPrint
  :: forall project r. Members [FileSystemRead project, Fail] r
  => FilePath
  -> SedExpression
  -> Sem r SedPrintResult
sedPrint (FilePath path) (SedExpression expr) = do
  -- Parse the sed expression
  ranges <- case parseSedExpression expr of
    Left err -> fail $ T.unpack err
    Right r -> return r

  -- Read the file
  contents <- FileSystem.readFile @project (T.unpack path)
  let allLines = T.lines (T.decodeUtf8 contents)

  -- Extract and format the matching lines
  let selectedLines = extractLineRanges ranges allLines
      numberedLines = map (\(lineNum, line) -> T.pack (show lineNum) <> "\t" <> line) selectedLines

      result = T.unlines numberedLines

  return $ SedPrintResult (truncateResponse result)

-- | Sed location specification
data Location
  = Line Int           -- 5
  | Match Text         -- /pattern/
  | Relative Int       -- +10 (only valid as second part of range)
  deriving (Show, Eq)

-- | Sed line range specification
data SedRange
  = SingleLocation Location      -- 5p or /pattern/p
  | Range Location Location      -- 5,10p or 5,/test/p or /start/,/end/p or /pattern/,+5p
  deriving (Show, Eq)

-- | Parse sed-style expression into line ranges
parseSedExpression :: Text -> Either Text [SedRange]
parseSedExpression expr =
  let -- Split by semicolon for multiple expressions
      parts = T.split (== ';') expr
      parseResults = map parseSingleExpr parts
  in sequence parseResults
  where
    parseSingleExpr :: Text -> Either Text SedRange
    parseSingleExpr e =
      let e' = T.strip e
          -- Remove trailing 'p' if present
          withoutP = if T.isSuffixOf "p" e' then T.dropEnd 1 e' else e'
      in case T.split (== ',') withoutP of
        [single] -> do
          loc <- parseLocation single
          return $ SingleLocation loc
        [start, end] -> do
          startLoc <- parseLocation start
          endLoc <- parseLocation end
          -- Validate: relative offset not allowed as start
          case startLoc of
            Relative _ -> Left "Relative offset (+N) not allowed as start of range"
            _ -> case endLoc of
              Relative n | n < 0 -> Left "Negative offsets not allowed"
              _ -> Right $ Range startLoc endLoc
        _ -> Left $ "Invalid sed expression: " <> e'

    parseLocation :: Text -> Either Text Location
    parseLocation loc =
      let loc' = T.strip loc
      in if T.isPrefixOf "/" loc' && T.isSuffixOf "/" loc'
         then -- /pattern/ (empty pattern matches all lines)
              Right $ Match (T.drop 1 $ T.dropEnd 1 loc')
         else if T.isPrefixOf "+" loc'
         then -- +N
              case T.decimal (T.drop 1 loc') of
                Right (n, rest) | T.null rest -> Right $ Relative n
                _ -> Left $ "Invalid relative offset: " <> loc'
         else -- N
              case T.decimal loc' of
                Right (n, rest) | T.null rest -> Right $ Line n
                _ -> Left $ "Invalid location: " <> loc'

-- | Extract lines matching the specified ranges (1-indexed)
extractLineRanges :: [SedRange] -> [Text] -> [(Int, Text)]
extractLineRanges ranges allLines =
  let indexed = zip [1..] allLines
      -- Collect all matching line numbers for each range
      allMatches = concatMap (matchRange indexed) ranges
  in filter (\(lineNum, _) -> lineNum `elem` allMatches) indexed
  where
    matchRange :: [(Int, Text)] -> SedRange -> [Int]
    matchRange indexed range = case range of
      SingleLocation loc -> matchSingleLocation indexed loc
      Range startLoc endLoc ->
        -- If startLoc is a pattern match, we need repeating ranges (sed semantics)
        case startLoc of
          Match _ ->
            let startLines = matchSingleLocation indexed startLoc
            in concatMap (\start -> resolveRange indexed start endLoc) startLines
          _ ->
            -- For Line or Relative, just a single range
            let startLine = resolveStartLocation indexed startLoc
            in case startLine of
              Nothing -> []
              Just start -> resolveRange indexed start endLoc

    -- Resolve a single location to all matching line numbers
    matchSingleLocation :: [(Int, Text)] -> Location -> [Int]
    matchSingleLocation indexed loc = case loc of
      Line n -> [n]
      Match pattern -> [lineNum | (lineNum, lineText) <- indexed, pattern `T.isInfixOf` lineText]
      Relative _ -> []  -- Relative on its own doesn't make sense, ignore

    -- Resolve start location to first matching line number
    resolveStartLocation :: [(Int, Text)] -> Location -> Maybe Int
    resolveStartLocation indexed loc = case loc of
      Line n -> Just n
      Match pattern -> case [(lineNum, lineText) | (lineNum, lineText) <- indexed, pattern `T.isInfixOf` lineText] of
        [] -> Nothing
        (firstMatch, _):_ -> Just firstMatch
      Relative n -> Just n  -- Treat as absolute (though parser should reject this)

    -- Resolve a range from start line to end location
    resolveRange :: [(Int, Text)] -> Int -> Location -> [Int]
    resolveRange indexed startLine endLoc = case endLoc of
      Line endLine -> [startLine .. endLine]
      Relative offset -> [startLine .. (startLine + offset)]
      Match pattern ->
        -- Find first match of pattern at or after startLine
        case [(lineNum, lineText) | (lineNum, lineText) <- indexed, lineNum >= startLine, pattern `T.isInfixOf` lineText] of
          [] -> []  -- Pattern not found after start
          (endLine, _):_ -> [startLine .. endLine]
