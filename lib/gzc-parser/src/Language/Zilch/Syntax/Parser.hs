{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Zilch.Syntax.Parser (runParser) where

import qualified Text.Megaparsec as MP
import Language.Zilch.Syntax.Errors (ParseError(..))
import Language.Zilch.Core.Tokens (LToken)
import Text.Diagnose (Diagnostic)
import qualified Language.Zilch.Core.ConcreteSyntaxTree as CST
import Data.Bifunctor (first)
import Language.Zilch.Syntax.Internal (megaparsecBundleToDiagnostic)
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Applicative (empty, (<|>))
import Data.Located (Located(..), Position(..), position, unwrapLocated)
import qualified Language.Zilch.Core.Tokens as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Zilch.Pretty.Tokens (pretty)
import Data.List (foldl')
import qualified Data.Text as Text
import Debug.Trace (traceShow)

type Parser m = (MP.MonadParsec ParseError (Vector LToken) m, MonadFail m)

-- | Runs the parser on a list of tokens obtained through lexing.
runParser :: [LToken] -> String -> Either (Diagnostic [] String Char) CST.Module
runParser tks filename = first toDiagnostic (MP.runParser parseFile filename (V.fromList tks))

-- | Transforms a 'ParseErrorBundle' into a 'Diagnostic'.
toDiagnostic :: MP.ParseErrorBundle (Vector LToken) ParseError -> Diagnostic [] String Char
toDiagnostic = megaparsecBundleToDiagnostic "Parse error on input"
{-# INLINE toDiagnostic #-}

------------------------------------------------------------------------------------------------------------------------------

-- | Eliminates any comment after executing a parser.
lexeme :: Parser m => m a -> m a
lexeme p = MPL.lexeme whitespace p
{-# INLINE lexeme #-}

-- | Parses any remaining whitespace (inline comments).
whitespace :: Parser m => m ()
whitespace = MPL.space empty parseLineComment empty <|> pure ()
{-# INLINE whitespace #-}

-- | See @'MPL.nonIndented'@.
nonIndented :: Parser m => m a -> m a
nonIndented = MPL.nonIndented whitespace
{-# INLINE nonIndented #-}

-- | See @'MPL.indentBlock'@.
indentBlock :: Parser m => m a -> m [a]
indentBlock p = do
  pos <- whitespace *> MPL.indentLevel
  p `MP.sepBy1` indentGuard EQ pos

-- | See @'MPL.lineFold'@.
lineFold :: Parser m => (m () -> m a) -> m a
lineFold = MPL.lineFold whitespace
{-# INLINE lineFold #-}

-- | Transforms a simple parser into a parser returning a located value.
located :: Parser m => m a -> m (Located a)
located p = do
  MP.SourcePos file beginLine beginColumn <- MP.getSourcePos
  MP.State input0 offset0 _ _ <- MP.getParserState
  -- NOTE: we need to fetch the input stream before, because it is not complete
  -- (it does not necessarily contain all tokens at once).

  res <- p

  MP.State _ offset _ _ <- MP.getParserState
  let Position _ (endLine, endColumn) _ = position $ input0 V.! (offset - 1 - offset0)

  -- NOTE: We need to fetch the last token parsed, which is located right before the
  --       currently available token.
  --       Thankfully, our tokens have their locations, so we can use the ending location
  --       of the previous token to get the ending location of the result of our parser @p@.

  let pos = Position (fromIntegral $ MP.unPos beginLine, fromIntegral $ MP.unPos beginColumn)
                     (endLine, endColumn)
                     file

  pure $ res :@ pos

-- | See @'MPL.indentGuard'@.
indentGuard :: Parser m => Ordering -> MP.Pos -> m MP.Pos
indentGuard  = MPL.indentGuard whitespace
{-# INLINE indentGuard #-}

-------------------------------------------------------------------------------------------------------------------------------

-- | Parses a line comment token.
parseLineComment :: Parser m => m ()
parseLineComment = MP.hidden $ () <$ parseToken \ case
  L.InlineComment _ -> True
  _                 -> False
{-# INLINE parseLineComment #-}

-- | Applies a parser between parentheses.
betweenParens :: Parser m => m a -> m a
betweenParens p = MP.between (parseSymbol L.LParen) (parseSymbol L.RParen) p
{-# INLINE betweenParens #-}

-- | Applies a parser between square brackets.
betweenBrackets :: Parser m => m a -> m a
betweenBrackets p = MP.between (parseSymbol L.LBrack) (parseSymbol L.RBrack) p
{-# INLINE betweenBrackets #-}

-- | Applies a parser between curly braces.
betweenBraces :: Parser m => m a -> m a
betweenBraces p = MP.between (parseSymbol L.LBrace) (parseSymbol L.RBrace) p
{-# INLINE betweenBraces #-}

-- | Applies a parser between angles.
betweenAngles :: Parser m => m a -> m a
betweenAngles p = MP.between (parseSymbol L.LAngle) (parseSymbol L.RAngle) p
{-# INLINE betweenAngles #-}

-- | Parses a given token in the stream.
parseSymbol :: Parser m => L.Token -> m LToken
parseSymbol tk = MP.satisfy ((== tk) . unwrapLocated) MP.<?> show (pretty tk)
{-# INLINE parseSymbol #-}

parseToken :: Parser m => (L.Token -> Bool) -> m LToken
parseToken p = MP.satisfy (p . unwrapLocated)
{-# INLINE parseToken #-}

-------------------------------------------------------------------------------------------------------------------------------

-- | Parses many modules composing a source file.
parseFile :: Parser m => m CST.Module
parseFile = lexeme (pure ()) *> lexeme parseModule <* MP.eof

-- | Parses a module, composed of a header, a list of imports and many declarations.
parseModule :: Parser m => m CST.Module
parseModule =
  CST.Module <$> lexeme parseModuleHeader
             <*> MP.many (lexeme $ nonIndented parseImport)
             <*> MP.many (lexeme $ nonIndented parseTopLevelDeclaration)

-- | Parses a module header, containing the name of the module and the optional export list.
parseModuleHeader :: Parser m => m (Maybe [Located CST.Identifier])
parseModuleHeader =
  MP.optional do
    lexeme (parseSymbol L.Export)
    betweenParens (parseQualifiedIdentifier `MP.sepBy` parseSymbol L.Comma)

-- | Parses a module import.
parseImport :: Parser m => m (Located CST.Import)
parseImport = located do
  CST.Import <$> MP.option False (True <$ parseSymbol L.Open)
             <*> (parseSymbol L.Import *> parseQualifiedIdentifier)
             <*> MP.optional (parseSymbol L.As *> parseQualifiedIdentifier)
             <*> MP.optional (betweenParens $ parseImportUnit `MP.sepBy` parseSymbol L.Comma)
  where
    parseImportUnit = (,) <$> parseQualifiedIdentifier
                          <*> MP.optional (parseSymbol L.As *> parseQualifiedIdentifier)

-- | Parses any top-level declaration
parseTopLevelDeclaration :: Parser m => m (Located CST.TopLevelDeclaration)
parseTopLevelDeclaration = located do
  (,) <$> MP.option [] (lexeme parseMetaAttributes)
      <*> lexeme do
            MP.choice [ parseFunctionDefinition True
                      , parseEnumDefinition
                      , parseRecordDefinition
                      , parseTypeAliasDefinition
                      , parseTypeclassDefinition
                      , parseImplementationDefinition
                      ]

-- | Parses an identifier.
parseQualifiedIdentifier :: Parser m => m (Located CST.Identifier)
parseQualifiedIdentifier = located (toIdentifier <$> qualIdentifier)
  where
    qualIdentifier = MP.choice
      [ MP.try $ (:) <$> symbol False <*> (parseSymbol L.Dot *> qualIdentifier)
      , pure <$> symbol True
      ]

    toIdentifier l = (init l, last l)
    {-# INLINE toIdentifier #-}

    symbol False = identifierName . unwrapLocated <$> identifier
    symbol True  = identifierName . unwrapLocated <$> (betweenParens operator <|> identifier)
    {-# INLINE symbol #-}

    operator = parseToken \ case
      L.Operator _ -> True
      _            -> False
    {-# INLINE operator #-}
    identifier = parseToken \ case
      L.Identifier _ -> True
      _              -> False
    {-# INLINE identifier #-}

    identifierName (L.Identifier i) = i
    identifierName (L.Operator o)   = o
    identifierName _                = error "parseQualifiedIdentifier@identifierName: not an identifier"
    {-# INLINE identifierName #-}

-- | Parses a function definition.
parseFunctionDefinition :: Parser m => Bool -> m (Located CST.Declaration)
parseFunctionDefinition withWhere = located $ lineFold \ s ->
  CST.Def <$> parseFunctionDeclaration
          <*> (lexeme (parseSymbol L.ColonEquals) *> lexeme (parseExpression s))
          <*> if withWhere then MP.option [] whereBlock else pure []
  where
    whereBlock = lexeme (parseSymbol L.Where) *> indentBlock (lexeme $ parseFunctionDefinition False)

-- | Parses a function header.
parseFunctionDeclaration :: Parser m => m CST.FunctionDeclaration
parseFunctionDeclaration = lineFold \ s -> do
  recursive <- lexeme (MP.choice [ False <$ parseSymbol L.Let, True <$ parseSymbol L.Rec ])
  universal <- MP.option ([], []) . lexeme $ betweenAngles ((,) <$> parseParameters (lexeme $ parseKind s) <*> MP.option [] (parseConstraints s))
  name <- lexeme parseQualifiedIdentifier
  parameters <- MP.optional . lexeme $ betweenParens (parseParameters (lexeme $ parseType s))
  returnType <- MP.optional $ lexeme (parseSymbol L.Colon) *> lexeme (parseType s)
  pure (CST.Decl recursive name universal parameters returnType)
  where
    parseConstraints s = do
      lexeme (parseSymbol L.Pipe)
      lexeme (parseType s) `MP.sepBy1` lexeme (parseSymbol L.Comma)

-- | Parses a comma-separated parameters list where type annotations are parsed with the second parameter.
parseParameters :: Parser m => m a -> m [Located (Located CST.Identifier, Maybe a)]
parseParameters p =
  located ((,) <$> lexeme parseQualifiedIdentifier
               <*> MP.optional (lexeme (parseSymbol L.Colon) *> p)) `MP.sepBy` lexeme (parseSymbol L.Comma)

-- | Parses a list of meta-specifiers enclosed in @#[...]@.
parseMetaAttributes :: Parser m => m [Located CST.MetaSpecifier]
parseMetaAttributes = do
  i <- MPL.indentLevel
  o <- MP.getOffset
  parseSymbol L.Hash

  MP.observing (indentGuard EQ (MP.mkPos (MP.unPos i + 1))) >>= \ case
    Left _  -> MP.setOffset o *> MP.customFailure HashWithSpaceInMetaAttributes
    Right _ -> pure ()

  betweenBrackets (parseMetaSpecifier `MP.sepBy` parseSymbol L.Comma)

-- | Parses a meta-specifier.
parseMetaSpecifier :: Parser m => m (Located CST.MetaSpecifier)
parseMetaSpecifier = located $ MP.choice
  [ parseDefault
  , CST.OpFix <$> parseInfixL
  , CST.OpFix <$> parseInfixR
  , CST.OpFix <$> parseInfix
  ]
  where
    parseDefault = CST.DefaultImpl <$ parseSymbol (L.Identifier "default")
    {-# INLINE parseDefault #-}

    parseInfixL = CST.InfixL <$> (parseSymbol (L.Identifier "infixl") *> parseInteger)
    {-# INLINE parseInfixL #-}

    parseInfixR = CST.InfixR <$> (parseSymbol (L.Identifier "infixr") *> parseInteger)
    {-# INLINE parseInfixR #-}

    parseInfix = CST.Infix <$> (parseSymbol (L.Identifier "infix") *> parseInteger)
    {-# INLINE parseInfix #-}

    parseInteger = (\ (L.Integer i) -> read (Text.unpack i)) . unwrapLocated <$> parseToken \ case
      L.Integer _ -> True
      _           -> False

-- | Parses an enumeration definition.
parseEnumDefinition :: Parser m => m (Located CST.Declaration)
parseEnumDefinition = lineFold \ s -> located do
  o <- MP.getOffset
  lexeme (parseSymbol L.Enum)
  CST.Enum <$> lexeme parseQualifiedIdentifier
           <*> MP.option [] (lexeme $ betweenParens (parseParameters (lexeme $ parseKind s)))
           <*> (lexeme (parseSymbol L.ColonEquals) *> constructors o)
  where
    constructors o = MP.observing (indentBlock $ lexeme parseEnumConstructor) >>= \ case
      Left e  -> do
        o2 <- MP.getOffset

        if o2 == o
        then MP.setOffset o *> MP.customFailure UnsupportedEmptyEnums
        else MP.parseError e
      Right r -> pure r

    parseEnumConstructor = lineFold \ s -> do
      o <- MP.getOffset
      name <- lexeme parseQualifiedIdentifier

      (MP.try . MP.lookAhead . MP.observing $ parseSymbol L.LParen) >>= \ case
        Left _  -> MP.setOffset o *> MP.customFailure MissingEnumVariantParameterList
        Right _ -> (name ,) <$> betweenParens (lexeme (parseType s) `MP.sepBy` lexeme (parseSymbol L.Comma))

-- | Parses a record definition.
parseRecordDefinition :: Parser m => m (Located CST.Declaration)
parseRecordDefinition = lineFold \ s -> located do
  lexeme (parseSymbol L.Record)
  CST.Record <$> lexeme parseQualifiedIdentifier
             <*> MP.option [] (lexeme $ betweenParens (parseParameters (lexeme $ parseKind s)))
             <*> (lexeme (parseSymbol L.ColonEquals) *> MP.option [] (indentBlock $ lexeme parseRecordField))
  where
    parseRecordField = lineFold \ s -> do
      (,) <$> (lexeme parseQualifiedIdentifier <* s <* lexeme (parseSymbol L.Colon) <* s)
          <*> parseType s

-- | Parses a type alias definition.
parseTypeAliasDefinition :: Parser m => m (Located CST.Declaration)
parseTypeAliasDefinition = lineFold \ s -> located do
  lexeme (parseSymbol L.Alias)
  CST.Type <$> lexeme parseQualifiedIdentifier
           <*> MP.option [] (lexeme $ betweenParens (parseParameters (lexeme $ parseKind s)))
           <*> (lexeme (parseSymbol L.ColonEquals) *> parseType s)

-- | Parses a type class definition.
parseTypeclassDefinition :: Parser m => m (Located CST.Declaration)
parseTypeclassDefinition = lineFold \ s -> located do
  lexeme (parseSymbol L.Class)
  CST.Class <$> lexeme parseQualifiedIdentifier
            <*> MP.option [] (lexeme $ betweenParens (parseParameters (lexeme $ parseKind s)))
            <*> MP.option [] (lexeme (parseSymbol L.Pipe *> (lexeme (parseType s) `MP.sepBy` lexeme (parseSymbol L.Comma))))
            <*> (lexeme (parseSymbol L.ColonEquals) *> MP.option [] (indentBlock $ lexeme parseClassMember))
  where
    parseClassMember = lineFold \ s -> do
      (,) <$> (lexeme parseQualifiedIdentifier <* s <* lexeme (parseSymbol L.Colon) <* s)
          <*> parseType s

-- | Parses a type class implementation.
parseImplementationDefinition :: Parser m => m (Located CST.Declaration)
parseImplementationDefinition = lineFold \ s -> located do
  lexeme (parseSymbol L.Impl)
  flip CST.Impl <$> MP.option ([], []) (lexeme $ betweenAngles ((,) <$> parseParameters (parseKind s) <*> MP.option [] (parseConstraints s)))
                <*> lexeme parseQualifiedIdentifier
                <*> MP.option [] (lexeme $ betweenParens (lexeme (parseType s) `MP.sepBy` lexeme (parseSymbol L.Comma)))
                <*> (lexeme (parseSymbol L.ColonEquals) *> MP.option [] (indentBlock (lexeme $ parseFunctionDefinition True)))
  where
    parseConstraints s = do
      lexeme (parseSymbol L.Pipe)
      lexeme (parseType s) `MP.sepBy1` lexeme (parseSymbol L.Comma)

-- | Parses an expression.
parseExpression :: Parser m => m () -> m (Located CST.Expression)
parseExpression s = MP.label "an expression" . located $ lexeme expressionAtom `MP.sepBy1` lexeme operator
  where
    expressionAtom = do
      fun <- lexeme . located $ MP.choice
        [ MP.try lambdaExpression
        , ifExpression
        , letExpression
        , caseExpression
        , recordExpression
        , holeExpression
        , wildcardExpression
        , variableExpression
        , literalExpression
        , CST.ParensE <$> betweenParens (parseExpression s)
        ]
      args <- MP.many . lexeme . located $ s *> betweenParens ((s *> lexeme (parseExpression s)) `MP.sepBy` (s *> lexeme (parseSymbol L.Comma)))
      access <- MP.optional $ lexeme (parseSymbol L.Dot) *> parseQualifiedIdentifier

      let makeApp f x =
            let Position begin _ file = position f
                Position _ end _      = position x
                pos                   = Position begin end file
            in [CST.ApplicationE f (unwrapLocated x) :@ pos] :@ pos

      let expr = foldl' makeApp ([fun] :@ position fun) args

      case access of
        Nothing -> pure (head $ unwrapLocated expr)
        Just a  ->
          let Position begin _ file = position expr
              Position _ end _      = position a
              pos                   = Position begin end file
          in pure $ CST.RecordAccessE expr a :@ pos

    operator = located do
      CST.OperatorE . (\ (L.Operator o :@ p) -> ([], o) :@ p) <$> parseToken \ case
        L.Operator _ -> True
        _            -> False
    {-# INLINE operator #-}

    lambdaExpression = do
      CST.FnE <$> lexeme (betweenParens $ parseParameters (lexeme (parseType s)))
              <*> (s *> lexeme (parseSymbol L.RightArrow) *> s *> parseExpression s)
    {-# INLINE lambdaExpression #-}

    ifExpression =
      CST.IfE <$> (lexeme (parseSymbol L.If) *> s *> lexeme (parseExpression s))
              <*> (lexeme (parseSymbol L.Then) *> s *> lexeme (parseExpression s))
              <*> (lexeme (parseSymbol L.Else) *> s *> lexeme (parseExpression s))
    {-# INLINE ifExpression #-}

    letExpression = lineFold \ s -> do
      CST.LetE <$> MP.some (parseFunctionDefinition False)
               <*> (s *> lexeme (parseSymbol L.In) *> s *> parseExpression s)
    {-# INLINE letExpression #-}

    caseExpression = do
      lexeme (parseSymbol L.Case) <* s
      expr <- lexeme (parseExpression s) <* s
      lexeme (parseSymbol L.Of)
      CST.CaseE expr <$> indentBlock (s *> caseBranch)
    {-# INLINE caseExpression #-}

    caseBranch = lineFold \ s -> do
      (,) <$> (lexeme (parsePattern s) <* s)
          <*> (lexeme (parseSymbol L.RightArrow) <* s *> lexeme (parseExpression s))
    {-# INLINE caseBranch #-}

    holeExpression = CST.TypedHoleE <$ parseSymbol L.Question
    {-# INLINE holeExpression #-}

    wildcardExpression = CST.WildcardE <$ parseSymbol L.Underscore
    {-# INLINE wildcardExpression #-}

    variableExpression = CST.IdentifierE <$> parseQualifiedIdentifier
    {-# INLINE variableExpression #-}

    recordExpression =
      CST.RecordE <$> betweenBraces (lexeme recordField `MP.sepBy` lexeme (parseSymbol L.Comma))
    {-# INLINE recordExpression #-}

    recordField = lineFold \ s ->
      (,) <$> (lexeme parseQualifiedIdentifier <* s)
          <*> (lexeme (parseSymbol L.ColonEquals) <* s *> parseExpression s)
    {-# INLINE recordField #-}

    literalExpression = MP.choice
      [ CST.IntegerE . (\ (L.Integer i) -> i) . unwrapLocated <$> parseToken \ case
          L.Integer _ -> True
          _           -> False
      , CST.StringE . (\ (L.String s) -> s) . unwrapLocated <$> parseToken \ case
          L.String _ -> True
          _          -> False
      , CST.CharE . (\ (L.Character c) -> c) . unwrapLocated <$> parseToken \ case
          L.Character _ -> True
          _             -> False
      ]
    {-# INLINE literalExpression #-}

-- | Parses a pattern.
parsePattern :: Parser m => m () -> m (Located CST.Pattern)
parsePattern s = MP.label "a pattern" . located $ lexeme patternAtom `MP.sepBy1` lexeme operator
  where
    patternAtom = located $ MP.choice
      [ wildcardPattern
      , integerPattern
      , MP.try constructorPattern
      , CST.VariableP <$> parseQualifiedIdentifier
      , CST.ParensP <$> betweenParens (parsePattern s) ]

    operator = located do
      CST.OperatorP . (\ (L.Operator o :@ p) -> ([], o) :@ p) <$> parseToken \ case
        L.Operator _ -> True
        _            -> False
    {-# INLINE operator #-}

    integerPattern = CST.IntegerP . (\ (L.Integer i) -> i) . unwrapLocated <$> parseToken \ case
      L.Integer _ -> True
      _           -> False
    {-# INLINE integerPattern #-}

    constructorPattern = do
      CST.ConstructorP <$> lexeme parseQualifiedIdentifier
                       <*> betweenParens (lexeme (parsePattern s) `MP.sepBy` lexeme (parseSymbol L.Comma))
    {-# INLINE constructorPattern #-}

    wildcardPattern = CST.WildcardP <$ parseSymbol L.Underscore
    {-# INLINE wildcardPattern #-}

-- | Parses a type expression.
parseType :: Parser m => m () -> m (Located CST.Type)
parseType s = lexeme typeAtom
  where
    typeAtom = do
      fun <- lexeme . located $ MP.choice
        [ forallType
        , constrainedType
        , MP.try functionType
        , identifierType
        , wildcardType
        , CST.ParensT <$> betweenParens (lexeme (parseType s))
        ]
      args <- MP.many . lexeme . located $ s *> MP.choice
        [ betweenParens $ (s *> lexeme (parseType s)) `MP.sepBy` (s *> lexeme (parseSymbol L.Comma))
        , pure <$> (s *> parseType s)
        ]

      let makeApp f x =
            let Position begin _ file = position f
                Position _ end _      = position x
            in CST.ApplicationT f (unwrapLocated x) :@ Position begin end file

      pure (foldl' makeApp fun args)

    forallType = do
      lexeme (parseSymbol L.Forall) <* s
      CST.ForallT <$> lexeme (betweenAngles $ s *> parseParameters (parseKind s) <* s)
                  <*> parseType s
    {-# INLINE forallType #-}

    constrainedType =
      CST.ConstrainedT <$> lexeme (betweenBrackets $ lexeme (parseType s) `MP.sepBy` lexeme (parseSymbol L.Comma))
                       <*> (s *> parseType s)
    {-# INLINE constrainedType #-}

    functionType = do
      CST.FunctionT <$> lexeme (betweenParens $ lexeme (parseType s) `MP.sepBy` lexeme (parseSymbol L.Comma))
                    <*> (s *> lexeme (parseSymbol L.RightArrow) *> s *> parseType s)

    identifierType =
      CST.IdentifierT <$> parseQualifiedIdentifier
    {-# INLINE identifierType #-}

    wildcardType = CST.WildcardT <$ parseToken \ case
      L.UniUnderscore -> True
      L.Underscore    -> True
      _               -> False
    {-# INLINE wildcardType #-}

-- | Parses a kind expression.
parseKind :: Parser m => m () -> m (Located CST.Kind)
parseKind s = located $ s *> MP.choice
  [ CST.FunctionK <$> lexeme (betweenParens $ lexeme (parseKind s) `MP.sepBy1` lexeme (parseSymbol L.Comma))
                  <*> (lexeme (parseSymbol L.RightArrow) *> parseKind s)
  , CST.ParensK <$> betweenParens (lexeme $ parseKind s)
  , CST.TypeK <$ parseSymbol (L.Identifier "type")
  ]
