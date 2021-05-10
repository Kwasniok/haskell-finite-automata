{-# LANGUAGE ScopedTypeVariables #-}

module Data.FA (
    State,
    Symbol,
    FARead,
    FAAccept,
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

-- transforms f :: (a -> b) to h ::(Maybe a -> c)
-- with a transformation g :: (b -> c) and a default value d ::c
-- such that:
-- h Nothing = d :: c)
-- h (Just x) = g (f x)
maybefy :: (a -> b) -> (b -> c) -> c -> (Maybe a -> c)
maybefy _ _ d Nothing = d
maybefy f g _ (Just x) = g (f x)

-- converts a DFA to an equivalent NFA
dfaToNfa :: (State a, Symbol b, Ord b) => DFA a b -> NFA a b
dfaToNfa (MkDFA t1 qi qas) = (MkNFA t2 qi qas) where
    t2 q = maybefy (t1 q) singleton Empty

-- converts an NFA to an equivalent DFA via the power automaton
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
