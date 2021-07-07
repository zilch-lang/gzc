{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Zilch.Syntax.Errors where

import qualified Text.Megaparsec as MP
import Language.Zilch.Syntax.Internal (Hintable(..))
import Text.Diagnose (hint, Diagnostic, Position, diagnostic, (<++>), reportError, Marker(..), Report)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (intercalate, foldl')
import Data.Functor ((<&>))
import Language.Zilch.Core.ConcreteSyntaxTree (Fixity(..))
import Data.Bifunctor (second)

data LexerError
  = InvalidEscapeSequence Char
  deriving (Eq, Ord)

instance Show LexerError where
  show (InvalidEscapeSequence c) = "invalid escape sequence '\\" <> [c] <> "'"

instance MP.ShowErrorComponent LexerError where
  showErrorComponent = show

instance Hintable LexerError where
  hints _ = []

data ParseError
  = MissingEnumVariantParameterList
  | HashWithSpaceInMetaAttributes
  | UnsupportedEmptyEnums
  deriving (Eq, Ord)

instance Show ParseError where
  show MissingEnumVariantParameterList = "missing parameter list for enumeration constructor"
  show HashWithSpaceInMetaAttributes   = "there should not be any space between the two '#' and '[' tokens"
  show UnsupportedEmptyEnums           = "enumerations with no constructors are not supported"

instance MP.ShowErrorComponent ParseError where
  showErrorComponent = show

instance Hintable ParseError where
  hints MissingEnumVariantParameterList = [hint "You can specify an empty parameter list '()' if your constructor does not take parameters."]
  hints HashWithSpaceInMetaAttributes   = []
  hints UnsupportedEmptyEnums           = []

data ResolverError
  = Lexing (Diagnostic [] String Char)             -- ^ A lexing error happened
  | Parsing (Diagnostic [] String Char)            -- ^ A parsing error happened
  | CyclicImports [(Text, Text)] Position          -- ^ @A@ includes @B@, which ends up including @A@
  | FileNotFoundInIncludePath FilePath Position    -- ^ A filename was not found in the include path
  | MultipleFilesFound Text [FilePath] Position    -- ^ Multiple files with the same name were found in the include path

fromResolverError :: (?includePath :: Vector Text) => ResolverError -> Diagnostic [] String Char
fromResolverError (Lexing d)                      = d
fromResolverError (Parsing d)                     = d
fromResolverError (CyclicImports ids p)           =
  diagnostic <++> reportError "Cyclic imports detected"
                              [(p, Where showCycle)]
                              []
  where
    showCycle = "From top to bottom:\n- " <> intercalate "\n- " (ids <&> \ (f, i) -> "'" <> Text.unpack f <> "' is included by '" <> Text.unpack i <> "'")
fromResolverError (FileNotFoundInIncludePath f p) =
  diagnostic <++> reportError ("File '" <> f <> "' not found in the include path")
                              [(p, Where . showIncludePath $ V.toList ?includePath)]
                              []
  where
    showIncludePath [] = ""
    showIncludePath l  = "Current include path:\n- " <> intercalate "\n- " (Text.unpack <$> l)
fromResolverError (MultipleFilesFound m fs p)     =
  diagnostic <++> reportError ("Multiple files were found to match the import of '" <> Text.unpack m <> "'")
                              [(p, Where showFiles)]
                              []
  where
    showFiles = "These files match the import:\n- " <> intercalate "\n- " fs

data DesugarerError
  = AmbiguousOperatorSequence [(Text, Fixity, Position)] Position
  | ConflictingFixitySpecifiers [(Position, Fixity)]

fromDesugarerError :: DesugarerError -> Report String
fromDesugarerError (AmbiguousOperatorSequence ops p) =
  reportError "Ambiguous operator sequence can be parsed in multiple ways"
    (foldl' merge [ (p, This "while desugaring this expression") ] ops)
    []
  where
    merge acc (_, opFix, opPos) = acc <> [ (opPos, Where $ "Found fixity '" <> show opFix <> "' for this operator") ]
fromDesugarerError (ConflictingFixitySpecifiers fixs) =
  reportError "Conflicting fixity specifications for operator"
    (fixs <&> second (Where . (<>) "Found fixity: " . show))
    []
