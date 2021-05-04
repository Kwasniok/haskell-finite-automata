{-# LANGUAGE ScopedTypeVariables #-}

module Data.FA.Symbol (
    Symbol,
    alphabet,
) where

import Data.Collection.Finite

class (Finite a) => Symbol a where
    alphabet :: [a]
    alphabet = elements

instance Symbol () where

instance Symbol Bool where

instance (Symbol a, Symbol b) => Symbol (a,b)
