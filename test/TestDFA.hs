module Main (main) where

import Prelude
import System.Exit (exitFailure, exitSuccess)

import Data.Set
import Data.FA.DFA as DFA

qs = fromList [0,1]
as = fromList ['a','b']
tf q a = 1 - q
qi = 0
qfs = fromList [1]
dfa = DFA.DFA qs as tf qi qfs

main :: IO ()
main = if
    (DFA.read dfa "" == 0) &&
    (DFA.read dfa "a" == 1) &&
    (DFA.read dfa "ab" == 0) &&
    (DFA.accepts dfa "" == False) &&
    (DFA.accepts dfa "a" == True) &&
    (DFA.accepts dfa "ab" == False)
    then exitSuccess else exitFailure
