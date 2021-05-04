{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.FA.DFA (
    DFA (MkDFA),
    readSymbol,
    readWord,
    accepts,
) where

import Data.FA.State
import Data.FA.Symbol

data DFA a b where
    MkDFA :: (State a, Symbol b) => (a -> b -> a) -> a -> [a] -> DFA a b
    -- MkDFA transitionFunction(t) initialState(q0) acceptingStates(qas)
    {- Note: The arguments states and alphabet are implicitly given via the
             types a and b respectively.
    -}

instance (Show a, Show b) => Show (DFA a b) where
    show (MkDFA t q0 qas) = (++) "DFA" $ show (states :: [a], alphabet :: [b], t, q0, qas)

readSymbol :: DFA a b -> a -> b -> a
readSymbol (MkDFA t _ _) q0 s = (t q0 s)

readWord :: DFA a b -> [b] -> a
readWord dfa@(MkDFA _ q0 _) word = f q0 word where
    f q [] = q
    f q (w0:w) = f (readSymbol dfa q w0) w

accepts :: DFA a b -> [b] -> Bool
accepts dfa@(MkDFA _ _ qas) word = elem (readWord dfa word) qas
