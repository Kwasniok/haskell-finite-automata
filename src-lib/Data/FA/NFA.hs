{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Data.FA.NFA
-- Description : Non-Deterministic finite automata (NFA).
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Representation of deterministic finite automata based on Michael Sipser's book 'Introduction to the Theory of Computation' (3rd edition), Cengage Learning 2013.
--
-- @since 0.1.0.0

module Data.FA.NFA (
    NFA (MkNFA),
) where

import Prelude hiding (map, fold, union, size)
import Data.Maybe
import Data.Collection.FiniteSet
import Data.FA.State
import Data.FA.Symbol
import qualified Data.FA.Base as Base

-- | Non-Deterministic finite automaton (NFA).
-- Represents a regular language an accepts exactly the words belonging to it.
-- The model of NFAs is equivalent to the model of DFAs but a specific representation of the same regular language may be more efficient or more straight forward to construct than corresponding DFAs.
-- Representation of non-deterministic finite automata based on Michael Sipser's book 'Introduction to the Theory of Computation' (3rd edition), Cengage Learning 2013, page 53.
--
-- @since 0.1.0.0
data NFA a b where
    -- | Constructor of NFAs.
    --
    -- __Note__: The set of states and the alphabet are implicitly given by the 'Data.Collection.Finite.Finite' types @a@ and @b@.
    --
    -- >>> -- transition function: from state `q` via symbol (Just s) or empty move (Nothing) to set of states
    -- >>> -- behaviour: Go to state `s` when it is a symbol or else terminate.
    -- >>> t q = maybe Empty (\s -> singleton s)
    -- >>> -- NFA starting in state `False` and accepting a word when ending up in `True`
    -- >>> nfa = MkNFA t False (singleton True)
    --
    -- @since 0.1.0.0
    MkNFA :: (State a, Ord a, Symbol b, Ord b) => (a -> Maybe b -> FiniteSet a) -> a -> FiniteSet a -> NFA a b

-- | @since 0.1.0.0
instance (Show a, Show b) => Show (NFA a b) where
    show (MkNFA t qis qas) = (++) "NFA" $ show (states :: [a], alphabet :: [b], t, qis, qas)

-- | Consumes any amount (zero or more) of empty symbols simultaneously based on a set of starting states and returns the corresponding states.
--
-- >>> -- transition function: switch state when reading an empty symbol
-- >>> t q s = if (isNothing s) then (singleton (not q)) else Empty
-- >>> -- two state NFA
-- >>> nfa = MkNFA t False (singleton True)
-- >>> -- start with a single state
-- >>> readEmptySymbols nfa (singleton False)
-- {False,True}
-- >>> reached `{True}` with zero transitions, `{False}` with one transition and any other number of transitions leads to `{False, True}`.
--
-- @since 0.1.0.0
readEmptySymbols :: forall a b. NFA a b -> FiniteSet a -> FiniteSet a
readEmptySymbols (MkNFA t _ _) qis = Prelude.foldr union Empty qss where
    -- list of sets of states [Q_0, Q_1, ... Q_n] where n = # states in the NFA - 1
    -- Each Set Q_k is a set of states reachable from any of the the initial
    -- states in qis by exactly k empty moves.
    -- qss :: [FiniteSet a]
    qss = Prelude.take (length (states :: [a])) (iterate res qis)
    -- read a single empy symbol: all states reachable by exaclty one empty move
    -- from any of the states qs
    -- res :: FiniteSet a -> FiniteSet a
    res qs = flatten (map (\q -> t q Nothing) qs)

-- | Consumes a single symbol without any empty moves. Returns a set of states where each element is reached b reading the given symbol form any of the initial states.
--
-- >>> -- transition function: go to state with the given designator
-- >>> t q s = if (isJust s) then (singleton (fromJust s)) else Empty
-- >>> -- two state NFA
-- >>> nfa = MkNFA t False (singleton True)
-- >>> -- start with a single state
-- >>> readNonEmptySymbol nfa (singleton False) True
-- {True}
--
-- @since 0.1.0.0
readNonEmptySymbol :: NFA a b -> FiniteSet a -> b -> FiniteSet a
readNonEmptySymbol (MkNFA t _ _) qs x = flatten (map (\q -> (t q (Just x))) qs)

readSymbol :: forall a b. (Ord a) => NFA a b -> FiniteSet a -> b -> FiniteSet a
readSymbol nfa q0s x = (emptyMoves . (nonEmptyMove x) . emptyMoves) q0s where
    -- all states reachable by any number (incl. 0) of empty moves from any of
    -- the states
    emptyMoves :: FiniteSet a -> FiniteSet a
    emptyMoves = readEmptySymbols nfa
    -- all states reachable by exactly the symbol x from any of the given states
    -- (no empty moves allowed)
    nonEmptyMove :: b -> FiniteSet a -> FiniteSet a
    nonEmptyMove x qs = readNonEmptySymbol nfa qs x


readWord :: forall a b. NFA a b -> [b] -> FiniteSet a
readWord nfa@(MkNFA t q0 _) word = recReadWord q0s word where
    -- all states reachable by any number (incl. 0) of empty moves from any of
    -- the states
    emptyMoves :: FiniteSet a -> FiniteSet a
    emptyMoves = readEmptySymbols nfa
    -- all states reachable by exactly the symbol x from any of the given states
    -- (no empty moves allowed)
    nonEmptyMove :: b -> FiniteSet a -> FiniteSet a
    nonEmptyMove x qs = readNonEmptySymbol nfa qs x
    --
    readSymbol :: b -> FiniteSet a -> FiniteSet a
    readSymbol x = emptyMoves . (nonEmptyMove x)
    -- initial state (incl. empty moves)
    -- q0s :: FiniteSet a
    q0s = emptyMoves (singleton q0)
    -- recursive read word function, consumes one symbol per step
    -- recReadWord :: FiniteSet a -> [b] -> FiniteSet a
    recReadWord qs [] = qs
    recReadWord qs (w0:w) = recReadWord (readSymbol w0 qs) w

accepts :: NFA a b -> [b] -> Bool
accepts nfa@(MkNFA _ _ qas) word = anyAccepting qfs where
    -- set of states after reading the entire word ('final' states)
    -- qfs :: FiniteSet a
    qfs = readWord nfa word
    -- True if any of the states is an accepting state
    -- FiniteSet a -> Bool
    anyAccepting = (any (\q -> elementOf q qas)) . toList

-- | @since 0.1.0.0
instance (State a, Ord a, Symbol b) => Base.FARead (FiniteSet a) b (NFA a b) where
    readSymbol = readSymbol
    readWord = readWord

-- | @since 0.1.0.0
instance (State a, Symbol b) => Base.FAAccept b (NFA a b) where
    accepts = accepts
