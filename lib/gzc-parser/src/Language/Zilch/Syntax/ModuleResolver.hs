{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Zilch.Syntax.ModuleResolver (parseAndResolveModules) where

import Data.Text (Text)
import Data.Vector (Vector)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Diagnose (Diagnostic)
import Control.Monad.Except (MonadError (throwError), runExceptT, liftEither)
import Data.HashMap.Strict (HashMap)
import qualified Language.Zilch.Core.ConcreteSyntaxTree as CST
import Language.Zilch.Syntax.Errors
import Data.Bifunctor (first, bimap)
import Language.Zilch.Syntax.Lexer (runLexer)
import Control.Monad.State.Strict (MonadState, modify, get, runStateT)
import Language.Zilch.Syntax.Parser (runParser)
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (forM, when, forM_)
import qualified Data.Vector as V
import qualified Data.Text as Text
import System.FilePath ((</>), joinPath, (<.>), equalFilePath)
import System.Directory (doesFileExist, canonicalizePath, makeRelativeToCurrentDirectory)
import qualified Data.HashMap.Strict as H
import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Text.IO as T
import qualified Algebra.Graph.Acyclic.AdjacencyMap as GA
import Data.Located (Located((:@)), Position(..))
import Data.List (groupBy)

data ResolverState
  = RState
        (HashMap Text CST.Module)
        (G.AdjacencyMap Text)
        (HashMap FilePath [String])

instance Semigroup ResolverState where
  RState h1 a1 f1 <> RState h2 a2 f2 = RState (h1 <> h2) (a1 `G.overlay` a2) (f1 <> f2)

instance Monoid ResolverState where
  mempty = RState mempty (G.vertex "@@main") mempty

type ModuleResolver m = (?includePath :: Vector Text, MonadIO m, MonadError ResolverError m, MonadState ResolverState m)

-- | Tries to parse a file given its module name.
--
--   Fails if the file corresponding to the module is not in the include path,
--   or if a parse error occurs.
--
--   Returns a hashmap mapping module names to parsed modules,
--   alongside a topological sort (import order) of those.
--   You may want to reverse the topological order to start
--   with "non-importing" modules first.
parseAndResolveModules :: (?includePath :: Vector Text, MonadIO m) => Text -> m (Either (Diagnostic [] String Char) (), ([(FilePath, [String])], HashMap Text CST.Module, [Text]))
parseAndResolveModules moduleName = do
  includePath <- liftIO $ V.toList <$> mapM (canonicalizePath . Text.unpack) ?includePath
  includePath <- pure $ head <$> groupBy equalFilePath includePath
  includePath <- liftIO $ mapM makeRelativeToCurrentDirectory includePath
  let ?includePath = Text.pack <$> V.fromList includePath
  -- Canonicalize and eliminate duplicated include paths; also make them relative to the current directory

  bimap (first fromResolverError) getModules <$> runStateT (runExceptT $ parseFile moduleName "@@main" commandLine) mempty
  where getModules (RState mods graph fs) = (H.toList fs, mods, fromJust $ GA.topSort . GA.removeVertex "@@main" <$> GA.toAcyclic graph)

        commandLine = Position (1, 1) (1, 1) "@@main"

parseFile :: ModuleResolver m => Text -> Text -> Position -> m ()
parseFile moduleName from pos = do
  RState mods _ _ <- get
  !mod <- case mods H.!? moduleName of
    Nothing -> do
      let filename = toFilePath moduleName
      filepath <- queryIncludePath filename moduleName pos

      !content <- liftIO $ T.readFile filepath

      insertFileContent filepath content

      !tks <- liftEither (first Lexing $ runLexer content filepath)
      liftEither (first Parsing $ runParser tks filepath)
    Just m -> pure m

  insertDependencies moduleName mod from pos

  pure ()

queryIncludePath :: ModuleResolver m => FilePath -> Text -> Position -> m FilePath
queryIncludePath p mod pos = do
  found <- catMaybes . V.toList <$> forM ?includePath \ ip -> do
    let path = Text.unpack ip </> p
    pathExists <- liftIO $ doesFileExist path

    pure if pathExists then Just path else Nothing

  case found of
    []  -> throwError (FileNotFoundInIncludePath p pos)
    [p] -> pure p
    l   -> throwError (MultipleFilesFound mod l pos)

insertFileContent :: ModuleResolver m => FilePath -> Text -> m ()
insertFileContent filePath content = modify \ (RState m a f) -> RState m a (H.insertWith const filePath (Text.unpack <$> Text.lines content) f)

insertDependencies :: ModuleResolver m => Text -> CST.Module -> Text -> Position -> m ()
insertDependencies modName mod from pos = do
  let CST.Module _ is _ = mod
  let importNames = importName <$> is

  modify \ (RState h1 a1 f1) -> RState (H.insertWith const modName mod h1)
                                       (G.overlays $ a1 : (G.edge modName <$> importNames))
                                       f1

  RState _ g _ <- get
  checkDependencyCycle g modName from pos

  forM_ is \ i@(_ :@ p) -> parseFile (importName i) modName p
  where
    importName (CST.Import _ ((qual, i) :@ _) _ _ :@ _) = Text.intercalate "." (qual <> [i])

checkDependencyCycle :: ModuleResolver m => G.AdjacencyMap Text -> Text -> Text -> Position -> m ()
checkDependencyCycle g root from pos = dfs g root [(root, from)]
  where
    dfs graph root stack = forM_ (G.postSet root graph) \ p -> do
      when ((p, root) `elem` stack) do
        throwError (flip CyclicImports pos . dropWhile ((/= p) . fst) . reverse $ stack)
      dfs graph p ((p, root) : stack)

-- | Transforms a @.@ separated module-name by a @/@ separated include path-relative file path.
toFilePath :: Text -> FilePath
toFilePath = (<.> "zc") . joinPath . fmap Text.unpack . Text.split (== '.')
