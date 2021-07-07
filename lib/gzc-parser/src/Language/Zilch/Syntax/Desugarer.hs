{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Zilch.Syntax.Desugarer (runDesugarer) where

import Control.Monad.State.Strict (MonadState, evalStateT, modify)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Language.Zilch.Core.ConcreteSyntaxTree as CST
import qualified Language.Zilch.Core.AbstractSyntaxTree as AST
import Text.Diagnose (Diagnostic, (<++>), diagnostic)
import Control.Monad.Except (MonadError, runExcept, throwError)
import Language.Zilch.Syntax.Errors (DesugarerError (ConflictingFixitySpecifiers), fromDesugarerError)
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as H
import Control.Monad (void, forM)
import Data.Located (Located((:@)), unwrapLocated)
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))

type Desugarer m = (MonadState DesugarerState m, MonadError DesugarerError m)

data DesugarerState
  = DesugarerState
      (HashMap Text (HashMap CST.Identifier CST.Fixity))  -- ^ Operator fixities per module

instance Semigroup DesugarerState where
  DesugarerState m1 <> DesugarerState m2 = DesugarerState (m1 <> m2)

instance Monoid DesugarerState where
  mempty = DesugarerState mempty


runDesugarer :: [Text] -> HashMap Text CST.Module -> Either (Diagnostic [] String Char) (HashMap Text AST.Module)
runDesugarer topsort mods = first toDiagnostic . runExcept $ evalStateT (exploreModules mods *> desugarModules topsort mods) mempty
  where
    toDiagnostic = (<++>) diagnostic . fromDesugarerError

exploreModules :: Desugarer m => HashMap Text CST.Module -> m ()
exploreModules = void . H.traverseWithKey exploreModule

-- | Registers the fixity of all defined operators in the module.
exploreModule :: Desugarer m => Text -> CST.Module -> m ()
exploreModule modName (CST.Module _ _ decls) = do
  fixs <- H.fromList . catMaybes <$> forM decls \ ((!specs, !decl) :@ _) -> case decl of
    CST.Def (CST.Decl _ (name :@ _) _ _ _) _ _ :@ _ ->
      case extractFixity <$> filter isFixitySpec specs of
        []  -> pure Nothing
        [f] -> pure $ Just (name, unwrapLocated f)
        fs  -> throwError (ConflictingFixitySpecifiers $ fs <&> \ (f :@ p) -> (p, f))
    _                                               -> pure Nothing
  modify \ (DesugarerState m) -> DesugarerState (H.insert modName fixs m)
  where
    isFixitySpec (CST.OpFix _ :@ _) = True
    isFixitySpec _                  = False

    extractFixity (CST.OpFix f :@ p) = f :@ p
    extractFixity _                  = undefined

desugarModules :: Desugarer m => [Text] -> HashMap Text CST.Module -> m (HashMap Text AST.Module)
desugarModules [] _        = pure mempty
desugarModules (m:ms) mods = desugarModules ms mods
