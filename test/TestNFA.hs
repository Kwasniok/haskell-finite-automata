module Main (main) where

import Prelude
import System.Exit (exitFailure, exitSuccess)

import Data.Set
import Data.Maybe
import Data.FA.NFA as NFA

qs = fromList [0,1]
as = fromList ['a','b']
tf q (Just a) = singleton (1 - q)
tf q Nothing = singleton q
qi = 0
qfs = fromList [1]
nfa = NFA.NFA qs as tf qi qfs

main :: IO ()
main = if
    (NFA.read nfa "" == (singleton 0)) &&
    (NFA.read nfa "a" == (singleton 1)) &&
    (NFA.read nfa "ab" == (singleton 0)) &&
    (NFA.accepts nfa "" == False) &&
    (NFA.accepts nfa "a" == True) &&
    (NFA.accepts nfa "ab" == False)
    then exitSuccess else exitFailure
