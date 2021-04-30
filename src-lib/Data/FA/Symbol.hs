{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.FA.Symbol (
    Symbol,
    empty,
    CharSymbol,
) where

import Prelude

class (Ord a) => Symbol a where
    empty :: a
    isEmpty :: a -> Bool
    isEmpty x = x == empty

data EmptySymbol = EmptySymbol deriving (Show, Eq, Ord)

type CharSymbol = Either Char EmptySymbol

instance Symbol CharSymbol where
    empty = Right EmptySymbol :: Either Char EmptySymbol
