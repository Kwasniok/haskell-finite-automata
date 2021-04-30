module Data.FA.DFA (
    DFA (DFA),
    read,
    accepts,
) where

import qualified Prelude
import qualified Data.Set as Set

data DFA q a = DFA {
    states :: Set.Set q,
    alphabet :: Set.Set a,
    transferFunc :: q -> a -> q,
    initialState :: q,
    acceptStates :: Set.Set q
}

next :: DFA q a -> a -> DFA q a
next (DFA qs as tf qi qfs) a = DFA qs as tf (tf qi a) qfs

read :: DFA q a -> [a] -> q
read fa [] = initialState fa
read fa (w:ws) = read (next fa w) ws

accepts :: (Prelude.Ord q) => DFA q a -> [a] -> Prelude.Bool
accepts fa ws = Set.member (read fa ws) (acceptStates fa)
