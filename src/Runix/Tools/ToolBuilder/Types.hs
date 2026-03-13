{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- | Type system for tool-builder agent
--
-- Custom newtypes ensure type safety and provide ToolParameter instances
-- for the tool-builder's API.
module Runix.Tools.ToolBuilder.Types
  ( -- * Parameter Types
    ToolName (..)
  , ToolDescription (..)
  , ToolImplementation (..)

    -- * Result Types
  , BuildToolResult (..)
  ) where

import Data.Text (Text)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Tools (ToolParameter(..), ToolFunction(..))

--------------------------------------------------------------------------------
-- Parameter Types
--------------------------------------------------------------------------------

-- | Name of the tool to build/modify
newtype ToolName = ToolName Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Human-readable description of what the tool does
newtype ToolDescription = ToolDescription Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | The complete Haskell implementation (signature + body + instances)
newtype ToolImplementation = ToolImplementation Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Result from the tool-builder agent
data BuildToolResult = BuildToolResult
  { 
    buildMessage :: Text
  } deriving stock (Show, Eq)

instance HasCodec BuildToolResult where
  codec = Autodocodec.object "BuildToolResult" $
    BuildToolResult
      <$> Autodocodec.requiredField "message" "summary of what happened" Autodocodec..= buildMessage

--------------------------------------------------------------------------------
-- ToolParameter Instances
--------------------------------------------------------------------------------

instance ToolParameter ToolName where
  paramName = "tool_name"
  paramDescription = "name of the tool (e.g., searchFiles, analyzeCode)"

instance ToolParameter ToolDescription where
  paramName = "description"
  paramDescription = "what the tool does and when to use it"

instance ToolParameter ToolImplementation where
  paramName = "implementation"
  paramDescription = "complete Haskell code including type signature, function body, and all required instances"

instance ToolParameter BuildToolResult where
  paramName = "build_result"
  paramDescription = "result of tool build operation"

--------------------------------------------------------------------------------
-- ToolFunction Instance
--------------------------------------------------------------------------------

instance ToolFunction BuildToolResult where
  toolFunctionName = "build_tool"
  toolFunctionDescription = "Create a new tool using the tool-builder agent. Provides a specialized environment for generating Haskell tools with full compilation validation and automatic registration."
