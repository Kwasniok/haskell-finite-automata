{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.FA.NFA (
    NFA (MkNFA),
    readSymbol,
    readWord,
    accepts,
) where

import Data.Maybe
import Data.Set
import Data.FA.State
import Data.FA.Symbol

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

readSymbol :: NFA a b -> a -> b -> Set a
readSymbol (MkNFA t _ _) q0 s = (t q0 (Just s))

readWord :: NFA a b -> [b] -> Set a
readWord nfa@(MkNFA _ q0 _) word = rrw q0s word where
    -- initial state (incl. empty move)
    -- q0s :: Set a
    q0s = emptyMove (singleton q0)
    -- all states reachable by any number (incl. 0) of empty moves from any of
    -- the states
    -- emptyMove :: Set a -> Set a
    emptyMove = (readEmptySymbols nfa)
    -- recursive read word function, takes one symbol per step
    -- rrw :: Set a -> [b] -> Set a
    rrw qs [] = qs
    rrw qs (w0:w) = rrw (rs qs) w
        where
            -- next set of states from current set of states and current symbol
            -- rs :: Set a -> Set a
            rs = emptyMove . flatten . (Data.Set.map (\q -> readSymbol nfa q w0))

accepts :: NFA a b -> [b] -> Bool
accepts nfa@(MkNFA _ _ qas) word = anyAccepting qfs where
    -- set of states after reading the entire word ('final' states)
    -- qfs :: Set a
    qfs = readWord nfa word
    -- True if any of the states is an accepting state
    -- Set a -> Bool
    anyAccepting = (any (\q -> elem q qas)) . toList
