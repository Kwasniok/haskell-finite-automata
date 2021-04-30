module Data.FA.NFA (
    NFA (NFA),
    read,
    accepts,
) where

import qualified Prelude
import qualified Data.Set as Set
import Data.Maybe as Maybe

data NFA q a = NFA {
    states :: Set.Set q,
    alphabet :: Set.Set a,
    transferFunc :: q -> Maybe a -> Set.Set q,
    initialState :: q,
    acceptStates :: Set.Set q
}

readEmptySymbol :: (Prelude.Ord q) => NFA q a -> Set.Set q -> Set.Set q
readEmptySymbol (NFA qs as tf qi qfs) qins = union (Set.map transfer qins)
    where
        union = Set.foldr Set.union Set.empty
        transfer = (\q -> tf q Nothing)

readEmptySymbolStar :: (Prelude.Ord q) => NFA q a -> Set.Set q -> Set.Set q
readEmptySymbolStar fa@(NFA qs as tf qi qfs) qins = (Prelude.iterate step qins) Prelude.!! (Set.size qs)
    where
        step qins = Set.union qins (readEmptySymbol fa qins)

readNonEmptySymbol :: (Prelude.Ord q) => NFA q a -> Set.Set q -> a -> Set.Set q
readNonEmptySymbol (NFA qs as tf qi qfs) qins a = union (Set.map transfer qins)
    where
        union = Set.foldr Set.union Set.empty
        transfer = (\q -> tf q (Just a))

readSymbol :: (Prelude.Ord q) => NFA q a -> Set.Set q -> a -> Set.Set q
readSymbol fa qins a = readEmptySymbolStar fa (readNonEmptySymbol fa qins a)

read :: (Prelude.Ord q) => NFA q a -> [a] -> Set.Set q
read fa w = rd w (readEmptySymbolStar fa (Set.singleton (initialState fa)))
    where
        rd [] qins = qins
        rd (w:ws) qins = rd ws (readSymbol fa qins w)

accepts :: (Prelude.Ord q) => NFA q a -> [a] -> Prelude.Bool
accepts fa w = Set.isSubsetOf (read fa w) (acceptStates fa)
