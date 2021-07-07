module Common where

import Test.Hspec
import Text.Diagnose (Diagnostic, PrettyText, prettyText, (<~<), Position (Position))
import Text.PrettyPrint.ANSI.Leijen (plain)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IndentLocated as I
import qualified Data.Located as L

fails :: Either (Diagnostic [] String Char) b -> Expectation
fails (Left d)    = pure ()
fails (Right res) = expectationFailure "Test was expected to fail but returned a correct value."

succeeds :: Either (Diagnostic [] String Char) b -> (String, Text) -> Expectation
succeeds (Left d)  (file, ct) = expectationFailure $ "Test was expected to succeed but failed with the error:\n"
                                                  <> show (plain $ prettyText (d <~< (file, lines $ Text.unpack ct)))
succeeds (Right r) _          = pure ()

right :: Either a b -> b
right (Right x) = x

left :: Either a b -> a
left (Left x) = x

infix 5 @-
(@-) :: a -> (Integer, Integer, Integer, Integer, Integer, String) -> I.Located a
x @- (bl, bc, el, ec, i, f) = I.ILocated (Position (bl, bc) (el, ec) f) i x
{-# INLINE (@-) #-}

infix 5 @+
(@+) :: a -> (Integer, Integer, Integer, Integer, String) -> L.Located a
x @+ (bl, bc, el, ec, f) = x L.:@ L.Position (bl, bc) (el, ec) f
{-# INLINE (@+) #-}
