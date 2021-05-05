module Data.FA (
State,
    Symbol,
    FA,
    accepts,
    DFA (MkDFA),
    NFA (MkNFA),
    dfaToNfa,
) where

import Data.Maybe
import Data.Set as Set
import Data.Collection.Finite
import Data.FA.State
import Data.FA.Symbol
import Data.FA.Base (FA, accepts)
import Data.FA.DFA (DFA (MkDFA))
import Data.FA.NFA (NFA (MkNFA))

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
    t2 q = maybefy (t1 q) Set.singleton Set.empty
