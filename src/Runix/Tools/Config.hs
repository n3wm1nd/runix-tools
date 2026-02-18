-- | Filesystem phantom types for parameterized filesystem effects
--
-- These types are shared between the main library and generated-tools sublibrary.
-- The IO-heavy configuration loading stays in the main Config module.
module Runix.Tools.Config
  ( -- * Filesystem Types
    ProjectFS(..)
  , ClaudeConfigFS(..)
  , RunixToolsFS(..)
  ) where

import Runix.FileSystem (HasProjectPath(..))

-- | Project filesystem - the user's project directory (CWD)
-- This is the main workspace where user code lives
newtype ProjectFS = ProjectFS FilePath
  deriving stock (Show, Eq)

-- | Claude configuration filesystem - access to .claude directories
-- Typically read-only access for loading subagents, skills, etc.
newtype ClaudeConfigFS = ClaudeConfigFS FilePath
  deriving stock (Show, Eq)

-- | Runix tools filesystem - the runix-code codebase itself
-- Used for GeneratedTools.hs and other tool development
newtype RunixToolsFS = RunixToolsFS FilePath
  deriving stock (Show, Eq)

-- HasProjectPath instances for filesystem types
instance HasProjectPath ProjectFS where
  getProjectPath (ProjectFS path) = path

instance HasProjectPath ClaudeConfigFS where
  getProjectPath (ClaudeConfigFS path) = path

instance HasProjectPath RunixToolsFS where
  getProjectPath (RunixToolsFS path) = path
