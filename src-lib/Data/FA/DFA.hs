module Data.FA.DFA (
    DFA (DFA),
    read,
    accepts,
    showTransferRules,
) where

import qualified Prelude
import qualified Data.Set as Set
import qualified Data.List as List

data DFA q a = DFA {
    states :: Set.Set q,
    alphabet :: Set.Set a,
    transferFunc :: q -> a -> q,
    initialState :: q,
    acceptStates :: Set.Set q
}

readSymbol :: DFA q a -> a -> DFA q a
readSymbol (DFA qs as tf qi qfs) a = DFA qs as tf (tf qi a) qfs

read :: DFA q a -> [a] -> q
read fa [] = initialState fa
read fa (w:ws) = read (readSymbol fa w) ws

accepts :: (Prelude.Ord q) => DFA q a -> [a] -> Prelude.Bool
accepts fa ws = Set.member (read fa ws) (acceptStates fa)

showTransferRules :: (Prelude.Ord q, Prelude.Show q, Prelude.Ord a, Prelude.Show a) => DFA q a -> Prelude.String
showTransferRules (DFA qs as tf qi qfs) = rules
    where
        rules = (List.intercalate ", " (Prelude.map showRule (Set.elems edgeTriples)))
        edgeTriples = Set.map makeEdgeTriple (Set.cartesianProduct qs as)
        showRule (q1, q2, a) = "(" List.++ (Prelude.show q1) List.++ "->" List.++ (Prelude.show q2) List.++ ")->" List.++  (Prelude.show a)
        makeEdgeTriple (q, a) = (q, tf q a, a)
