module Data.FA (
State,
    Symbol,
    FA,
    accepts,
    DFA (MkDFA),
    NFA (MkNFA),
) where

import Data.Collection.Finite
import Data.FA.State
import Data.FA.Symbol
import Data.FA.Base (FA, accepts)
import Data.FA.DFA (DFA (MkDFA))
import Data.FA.NFA (NFA (MkNFA))
