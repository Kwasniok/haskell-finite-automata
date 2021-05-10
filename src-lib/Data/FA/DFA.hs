{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Data.FA.DFA
-- Description : Deterministic finite automata (DFA).
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Representation of deterministic finite automata based on Michael Sipser's book 'Introduction to the Theory of Computation' (3rd edition), Cengage Learning 2013.
--
-- @since 0.1.0.0

module Data.FA.DFA (
    DFA (MkDFA),
) where

import Data.FA.State
import Data.FA.Symbol
import qualified Data.FA.Base as Base
import Data.Collection.FiniteSet

-- | Deterministic finite automaton (DFA).
-- Representation of deterministic finite automata based on Michael Sipser's book 'Introduction to the Theory of Computation' (3rd edition), Cengage Learning 2013, page 35.
--
-- @since 0.1.0.0
data DFA a b where
    -- | Constructor of DFAs.
    --
    -- __Note__: The set of states and the alphabet are implicitly given by the 'Data.Collection.Finite.Finite' types @a@ and @b@.
    --
    -- >>> -- transition function: from state `q` via symbol (Just s) or empty move (Nothing) to set of states
    -- >>> -- behaviour: Go to state `s`.
    -- >>> t q s = s
    -- >>> -- DFA starting in state `False` and accepting a word when ending up in `True`
    -- >>> dfa = MkDFA t False (singleton True)
    --
    -- @since 0.1.0.0
    MkDFA :: (State a, Ord a, Symbol b) => (a -> b -> a) -> a -> FiniteSet a -> DFA a b

-- | @since 0.1.0.0
instance (Show a, Show b) => Show (DFA a b) where
    show (MkDFA t q0 qas) = (++) "DFA" $ show (states :: [a], alphabet :: [b], t, q0, qas)

readSymbol :: DFA a b -> a -> b -> a
readSymbol (MkDFA t _ _) q0 s = (t q0 s)

readWord :: DFA a b -> [b] -> a
readWord dfa@(MkDFA _ q0 _) word = f q0 word where
    f q [] = q
    f q (w0:w) = f (readSymbol dfa q w0) w

accepts :: DFA a b -> [b] -> Bool
accepts dfa@(MkDFA _ _ qas) word = elementOf (readWord dfa word) qas

-- | @since 0.1.0.0
instance (State a, Ord a, Symbol b) => Base.FARead a b (DFA a b) where
    readSymbol = readSymbol
    readWord = readWord

-- | @since 0.1.0.0
instance (State a, Symbol b) => Base.FAAccept b (DFA a b) where
    accepts = accepts
