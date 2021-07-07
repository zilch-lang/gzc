{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module LexerSpec (spec) where

import Test.Hspec
import Language.Zilch.Syntax.Lexer (runLexer)
import Data.Either (fromRight)
import Common
import Text.RawString.QQ
import qualified Language.Zilch.Core.Tokens as L

spec :: Spec
spec = parallel do
  emptyStream
  spaceStream
  onlyKeywordStream
  identifierStream
  operatorStream
  commentStream
  invalidEscapeStream
  stringAndCharStream

-------------------------------------------------------------------------
----- HINT: use [r|...|] for raw strings

specFilename :: String
specFilename = "<test>"
{-# INLINE specFilename #-}

emptyStream :: Spec
emptyStream = describe "on an empty stream" do
  let result = runLexer "" specFilename
  it "successfully runs" do
    succeeds result (specFilename, "")
  it "generates no lexeme" do
    right result `shouldBe` []

spaceStream :: Spec
spaceStream = describe "on an invisible stream" do
  let content = "     \t\r    \v"
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates no lexeme" do
    right result `shouldBe` []

onlyKeywordStream :: Spec
onlyKeywordStream = describe "on a keyword-only stream" do
  let content = [r|case module export let def|]
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates keywords" do
    right result `shouldBe` [ L.Case @- (1, 1, 1, 5, 0, specFilename)
                            , L.Module @- (1, 6, 1, 12, 0, specFilename)
                            , L.Export @- (1, 13, 1, 19, 0, specFilename)
                            , L.Let @- (1, 20, 1, 23, 0, specFilename)
                            , L.Def @- (1, 24, 1, 27, 0, specFilename) ]

identifierStream :: Spec
identifierStream = describe "on an identifier stream" do
  let content = [r|someId idk IDK Something (+++-) A.B|]
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates identifiers" do
    right result `shouldBe` [ L.Identifier "someId" @- (1, 1, 1, 7, 0, specFilename)
                            , L.Identifier "idk" @- (1, 8, 1, 11, 0, specFilename)
                            , L.Identifier "IDK" @- (1, 12, 1, 15, 0, specFilename)
                            , L.Identifier "Something" @- (1, 16, 1, 25, 0, specFilename)
                            , L.LParen @- (1, 18, 1, 19, 0, specFilename)
                            , L.Operator "+++-" @- (1, 19, 1, 23, 0, specFilename)
                            , L.RParen @- (1, 23, 1, 24, 0, specFilename)
                            , L.Identifier "A" @- (1, 26, 1, 27, 0, specFilename)
                            , L.Dot @- (1, 17, 1, 28, 0, specFilename)
                            , L.Identifier "B" @- (1, 28, 1, 29, 0, specFilename) ]

operatorStream :: Spec
operatorStream = describe "on an operator stream" do
  let content = [r|++.--(#)~|]
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates symbols" do
    right result `shouldBe` [ L.Operator "++.--" @- (1, 1, 1, 6, 0, specFilename)
                            , L.LParen @- (1, 6, 1, 7, 0, specFilename)
                            , L.Hash @- (1, 7, 1, 8, 0, specFilename)
                            , L.RParen @- (1, 8, 1, 9, 0, specFilename)
                            , L.Operator "~" @- (1, 9, 1, 10, 0, specFilename) ]

commentStream :: Spec
commentStream = describe "on a comment stream" do
  let content = [r|-- Hello, world!|]
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates a comment" do
    right result `shouldBe` [ L.InlineComment "Hello, world!" @- (1, 1, 1, 17, 0, specFilename) ]

invalidEscapeStream :: Spec
invalidEscapeStream = describe "on an invalid escape character string stream" do
  let content = [r|"abc\d"|]
  let result = runLexer content specFilename
  it "successfully fails" do
    fails result

stringAndCharStream :: Spec
stringAndCharStream = describe "on a string stream" do
  let content = [r|"hello" 'w' 'o' 'r' 'l' 'd'|]
  let result = runLexer content specFilename
  it "successfully runs" do
    succeeds result (specFilename, content)
  it "generates strings and characters" do
    right result `shouldBe` [ L.String "hello" @- (1, 1, 1, 8, 0, specFilename)
                            , L.Character "w" @- (1, 9, 1, 12, 0, specFilename)
                            , L.Character "o" @- (1, 13, 1, 16, 0, specFilename)
                            , L.Character "r" @- (1, 17, 1, 20, 0, specFilename)
                            , L.Character "l" @- (1, 21, 1, 24, 0, specFilename)
                            , L.Character "d" @- (1, 25, 1, 28, 0, specFilename) ]
