{-# LANGUAGE ScopedTypeVariables #-}

module Data.FA.Symbol (
    Symbol,
    alphabet,
) where

import Data.Collection.Finite
import Data.Collection.FiniteSet

class (Finite a, Eq a) => Symbol a where
    alphabet :: [a]
    alphabet = elements

instance Symbol () where

instance Symbol Bool where

instance (Symbol a, Symbol b) => Symbol (a,b)

instance (Symbol a) => Symbol (FiniteSet a) where
