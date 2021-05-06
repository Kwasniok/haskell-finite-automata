{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FA.Base (
    FARead,
    FAAccept,
    readSymbol,
    readWord,
    accepts,
) where

import Data.FA.Symbol
import Data.FA.State

-- meaning of types:
-- a : state type
-- b : symbol type
-- c : finite automata type
class (State a, Symbol b) => FARead a b c where
    readSymbol :: c -> a -> b -> a
    readWord :: c -> [b] -> a

-- meaning of types:
-- b : symbol type
-- c : finite automata type
class (Symbol b) => FAAccept b c where
    accepts :: c -> [b] -> Bool
