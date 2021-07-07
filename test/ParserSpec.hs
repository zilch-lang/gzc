{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module ParserSpec (spec) where

import Test.Hspec
import Common
import qualified Language.Zilch.Core.ConcreteSyntaxTree as CST
import Language.Zilch.Syntax.Parser (runParser)
import qualified Language.Zilch.Core.Tokens as L
import Text.RawString.QQ

spec :: Spec
spec = parallel do
  emptyStream
  commentStream
  simpleExportList
  missingParentheseInExport
  simpleImport
  openedImportNoList

-----------------------------------------------------------------------------------

specFilename :: String
specFilename = "<test>"

emptyStream :: Spec
emptyStream = describe "on an empty stream" do
  let content = []
  let result = runParser content specFilename
  it "successfully runs" do
    succeeds result (specFilename, "")
  it "generates an empty module" do
    right result `shouldBe` CST.Module Nothing [] []

commentStream :: Spec
commentStream = describe "on a comment stream" do
  let content = [ L.InlineComment "abcdef" @- (1, 1, 1, 10, 0, specFilename) ]
  let result = runParser content specFilename
  it "successfully runs" do
    succeeds result (specFilename, [r|-- abcdef|])
  it "generates an empty module" do
    right result `shouldBe` CST.Module Nothing [] []

simpleExportList :: Spec
simpleExportList = describe "with only an empty export list" do
  let content = [ L.Export @- (1, 1, 1, 7, 0, specFilename)
                , L.LParen @- (1, 8, 1, 9, 0, specFilename)
                , L.RParen @- (1, 9, 1, 10, 0, specFilename) ]
  let result = runParser content specFilename
  it "successfully runs" do
    succeeds result (specFilename, [r|export ()|])
  it "generates a module with an empty export list" do
    right result `shouldBe` CST.Module (Just []) [] []

missingParentheseInExport :: Spec
missingParentheseInExport = describe "with a missing ')' in an export list" do
  let content = [ L.Export @- (1, 1, 1, 7, 0, specFilename)
                , L.LParen @- (1, 8, 1, 9, 0, specFilename) ]
  let result = runParser content specFilename
  it "correctly fails on the missing closing parenthese" do
    fails result

simpleImport :: Spec
simpleImport = describe "on a simple import" do
  let content = [ L.Import @- (1, 1, 1, 7, 0, specFilename)
                , L.Identifier "A" @- (1, 8, 1, 9, 0, specFilename) ]
  let result = runParser content specFilename
  it "correctly runs" do
    succeeds result (specFilename, [r|import A|])
  it "generates a single unqualified module import" do
    right result `shouldBe` CST.Module Nothing [ CST.Import False (([], "A") @+ (1, 8, 1, 9, specFilename)) Nothing Nothing @+ (1, 1, 1, 9, specFilename)] []

openedImportNoList :: Spec
openedImportNoList = describe "on an opened import with no import list" do
  let content = [ L.Open @- (1, 1, 1, 5, 0, specFilename)
                , L.Import @- (1, 6, 1, 12, 0, specFilename)
                , L.Identifier "A" @- (1, 13, 1, 14, 0, specFilename) ]
  let result = runParser content specFilename
  it "correctly runs" do
    succeeds result (specFilename, [r|open import A|])
  it "generates an opened import" do
    right result `shouldBe` CST.Module Nothing [ CST.Import True (([], "A") @+ (1, 13, 1, 14, specFilename)) Nothing Nothing @+ (1, 1, 1, 14, specFilename) ] []
