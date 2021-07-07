{-# LANGUAGE PackageImports #-}

module Data.Located (module NSC_Data_Located, position, unwrapLocated) where

import "nsc-core" Data.Located as NSC_Data_Located (Located(..), Position(..))

position :: Located a -> Position
position (_ :@ p) = p

unwrapLocated :: Located a -> a
unwrapLocated (x :@ _) = x
