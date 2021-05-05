{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FA.Base (
    FA,
    accepts,
) where

import Data.FA.Symbol
import Data.FA.State

-- a finite automata type c with symbol type b
class (Symbol b) => FA b c where
    accepts :: c -> [b] -> Bool
