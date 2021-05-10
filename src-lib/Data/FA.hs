{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.FA
-- Description : Finite automata.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module for emulating finite automata. Representations of finite automata are based on Michael Sipser's book 'Introduction to the Theory of Computation' (3rd edition), Cengage Learning 2013.
--
-- @since 0.1.0.0

module Data.FA (
    State,
    Symbol,
    FARead,
    readSymbol,
    readWord,
    FAAccept,
    accepts,
    DFA (MkDFA),
    NFA (MkNFA),
    dfaToNfa,
    nfaToDfa,
) where

import Prelude hiding (map, filter)
import Data.Maybe
import Data.Collection.Finite
import Data.Collection.FiniteSet
import Data.FA.State
import Data.FA.Symbol
import Data.FA.Base (FARead, FAAccept, readSymbol, readWord, accepts)
import Data.FA.DFA (DFA (MkDFA))
import Data.FA.NFA (NFA (MkNFA))
import qualified Data.FA.DFA as DFA
import qualified Data.FA.NFA as NFA

-- | __internal only__:
-- transforms f :: (a -> b) to h ::(Maybe a -> c)
-- with a transformation g :: (b -> c) and a default value d :: c
-- such that:
-- h Nothing = d :: c)
-- h (Just x) = g (f x)
maybefy :: (a -> b) -> (b -> c) -> c -> (Maybe a -> c)
maybefy _ _ d Nothing = d
maybefy f g _ (Just x) = g (f x)

-- | Converts a `DFA` to an equivalent `NFA`.
--
-- >>> dfa = MkDFA (\q -> \x -> x) False (singleton True)
-- >>> accepts dfa [True]
-- True
-- >>> accepts dfa [False]
-- False
-- >>> nfa = dfaToNfa dfa
-- >>> accepts nfa [True]
-- True
-- >>> accepts nfa [False]
-- False
--
-- @since 0.1.0.0
dfaToNfa :: (State a, Symbol b, Ord b) => DFA a b -> NFA a b
dfaToNfa (MkDFA t1 qi qas) = (MkNFA t2 qi qas) where
    t2 q = maybefy (t1 q) singleton Empty

-- | Converts a `NFA` to an equivalent `DFA` (using the power automaton algorithm).
--
-- >>> import Data.Maybe
-- >>> nfa = MkNFA (\q -> maybe Empty (\x -> singleton x)) False (singleton True)
-- >>> accepts nfa [True]
-- True
-- >>> accepts nfa [False]
-- False
-- >>> dfa = nfaToDfa nfa
-- >>> accepts dfa [True]
-- True
-- >>> accepts dfa [False]
-- False
--
-- @since 0.1.0.0
nfaToDfa :: forall a b. (State a, Symbol b, Ord a) => NFA a b -> DFA (FiniteSet a) b
nfaToDfa nfa@(MkNFA t1 qis1 qas1) = MkDFA t2 qis2 qas2 where
    t2 :: FiniteSet a -> b -> FiniteSet a
    t2 qs a = readSymbol nfa qs a
    qis2 :: FiniteSet a
    qis2 = singleton qis1
    qas2 :: FiniteSet (FiniteSet a)
    qas2 = filter anyAccepting $ fromList (states :: [FiniteSet a])
    -- True if any of the states is an accepting state
    -- FiniteSet a -> Bool
    anyAccepting = (any (\q -> elementOf q qas1)) . toList
