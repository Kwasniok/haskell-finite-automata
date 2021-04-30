{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.FA.Alphabet (
    Alphabet,
    empty,
    CharAlphabet,
) where

import Prelude
import qualified Data.Set as Set
import qualified Data.FA.Symbol as Symbol

class (Symbol.Symbol a) => Alphabet a where
    empty :: Set.Set a

type CharAlphabet = Set.Set Symbol.CharSymbol

instance Alphabet Symbol.CharSymbol where
    empty = Set.empty
