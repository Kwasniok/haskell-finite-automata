{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.FA.NFA (
    NFA (MkNFA),
) where

import Data.Maybe
import Data.Set
import Data.FA.State
import Data.FA.Symbol
import qualified Data.FA.Base as Base

data NFA a b where
    MkNFA :: (State a, Ord a, Symbol b, Ord b) => (a -> Maybe b -> Set a) -> a -> Set a -> NFA a b
    -- MkNFA transitionFunction(t) initialState(q0) acceptingStates(qas)
    {- Note: The arguments states and alphabet are implicitly given via the
             types a and b respectively.
    -}

instance (Show a, Show b) => Show (NFA a b) where
    show (MkNFA t qis qas) = (++) "NFA" $ show (states :: [a], alphabet :: [b], t, qis, qas)

-- AUX
flatten :: (Ord a) => Set (Set a) -> Set a
flatten = Data.Set.fold union empty

readEmptySymbols :: NFA a b -> Set a -> Set a
readEmptySymbols (MkNFA t _ _) qis = Prelude.foldr union empty qss where
    -- list of states reachable from the initial states qis by 0,1,2,...,|qis|
    -- empty moves
    -- qss :: [Set a]
    qss = Prelude.take (size qis) (iterate res qis)
    -- read single empy symbol: allstates reachable by exaclty one empty move
    -- from any of the states qs
    -- res :: Set a -> Set a
    res qs = flatten (Data.Set.map (\q -> t q Nothing) qs)

readNonEmptySymbol :: NFA a b -> Set a -> b -> Set a
readNonEmptySymbol (MkNFA t _ _) qs x = flatten (Data.Set.map (\q -> (t q (Just x))) qs)

readSymbol :: forall a b. (Ord a) => NFA a b -> Set a -> b -> Set a
readSymbol nfa q0s x = (emptyMoves . (nonEmptyMove x) . emptyMoves) q0s where
    -- all states reachable by any number (incl. 0) of empty moves from any of
    -- the states
    emptyMoves :: Set a -> Set a
    emptyMoves = readEmptySymbols nfa
    -- all states reachable by exactly the symbol x from any of the given states
    -- (no empty moves allowed)
    nonEmptyMove :: b -> Set a -> Set a
    nonEmptyMove x qs = readNonEmptySymbol nfa qs x


readWord :: forall a b. NFA a b -> [b] -> Set a
readWord nfa@(MkNFA t q0 _) word = recReadWord q0s word where
    -- all states reachable by any number (incl. 0) of empty moves from any of
    -- the states
    emptyMoves :: Set a -> Set a
    emptyMoves = readEmptySymbols nfa
    -- all states reachable by exactly the symbol x from any of the given states
    -- (no empty moves allowed)
    nonEmptyMove :: b -> Set a -> Set a
    nonEmptyMove x qs = readNonEmptySymbol nfa qs x
    --
    readSymbol :: b -> Set a -> Set a
    readSymbol x = emptyMoves . (nonEmptyMove x)
    -- initial state (incl. empty moves)
    -- q0s :: Set a
    q0s = emptyMoves (singleton q0)
    -- recursive read word function, consumes one symbol per step
    -- recReadWord :: Set a -> [b] -> Set a
    recReadWord qs [] = qs
    recReadWord qs (w0:w) = recReadWord (readSymbol w0 qs) w

accepts :: NFA a b -> [b] -> Bool
accepts nfa@(MkNFA _ _ qas) word = anyAccepting qfs where
    -- set of states after reading the entire word ('final' states)
    -- qfs :: Set a
    qfs = readWord nfa word
    -- True if any of the states is an accepting state
    -- Set a -> Bool
    anyAccepting = (any (\q -> elem q qas)) . toList

instance (State a, Symbol b) => Base.FAAccept b (NFA a b) where
    accepts = accepts
