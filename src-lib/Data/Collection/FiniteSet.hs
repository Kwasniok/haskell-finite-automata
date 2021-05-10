{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Data.Collection.FiniteSet
-- Description : Sets of instances from `Finite` data types.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definition of a set based on a `Finite` data type.
--
-- @since 0.1.0.0

module Data.Collection.FiniteSet (
    FiniteSet (Empty),
    elementOf,
    containsAnyOf,
    size,
    fold,
    union,
    flatten,
    toList,
    toSet,
    fromList,
    fromSet,
    singleton,
    map,
    powerSet,
    filter,
) where

import Prelude hiding (map, filter)
import qualified Prelude
import qualified Data.Set as Set
import Data.Collection.Finite

-- | Represents a set of elements from which only `Finite`-ly many exist.
--
-- @since 0.1.0.0
data FiniteSet a where
    -- | Token for empty sets.
    --
    -- @since 0.1.0.0
    Empty :: (Finite a) => FiniteSet a
    -- | Linked-list style constructor for non-empty sets.
    --
    -- @since 0.1.0.0
    Set :: (Finite a) => FiniteSet a -> a -> FiniteSet a

-- | Checks if an instance of @a@ is element of the given set.
--
-- >>> True `elementOf` (singleton True)
-- True
--
-- >>> False `elementOf` (singleton True)
-- False
--
-- @since 0.1.0.0
elementOf :: (Eq a) => a -> FiniteSet a -> Bool
elementOf _ Empty = False
elementOf x (Set s y) = (x == y) || (elementOf x s)

-- | Checks if a set contains any of the listed items.
--
-- >>> containsAnyOf (singleton True) [True, False]
-- True
--
-- >>> containsAnyOf (singleton True) [False]
-- False
--
-- @since 0.1.0.0
containsAnyOf :: (Eq a) => FiniteSet a -> [a] -> Bool
containsAnyOf s xs = any (\x -> elementOf x s) xs

-- | Checks for equality (like `(Prelude.==)`).
--
-- >>> equal (singleton True) (singleton True)
-- True
--
-- >>> equal (singleton True) (singleton False)
-- False
--
-- @since 0.1.0.0
equal :: (Eq a) => FiniteSet a -> FiniteSet a -> Bool
equal Empty Empty = True
equal Empty (Set _ _) = False
equal (Set _ _) Empty = False
equal (Set s1 x1) (Set s2 x2) = (x1 == x2) && (equal s1 s2)

-- | Checks for equality (like `(Prelude.<=)`).
--
-- >>> equal (singleton True) (singleton True)
-- True
--
-- >>> equal (singleton True) (singleton False)
-- False
--
-- @since 0.1.0.0
leq :: (Eq a) => FiniteSet a -> FiniteSet a -> Bool
leq Empty _ = True
leq (Set _ _) Empty = False
leq (Set s1 x1) (Set s2 x2) = (x1 == x2) && (leq s1 s2)

-- | Size of the set.
--
-- >>> size (Empty :: FiniteSet Bool)
-- 0
--
-- >>> size (singleton True)
-- 1
--
-- @since 0.1.0.0
size :: (Finite a) => FiniteSet a -> Int
size  Empty = 0
size (Set s _) = 1 + (size s)

-- | Reduce a set with a binary operation to one element (right-associative).
--
-- >>> fold (&&) True Empty
-- True
--
-- >>> fold (&&) True (singleton False)
-- True
--
-- @since 0.1.0.0
fold :: (Finite a) => (a -> b -> b) -> b -> FiniteSet a -> b
fold _ e Empty = e
fold f e (Set s x) = f x (fold f e s)

-- | Unify sets.
--
-- >>> union Empty (singleton True)
-- {True}
--
-- >>> union (singleton True) (singleton True)
-- {True}
--
-- @since 0.1.0.0
union :: (Finite a, Eq a) => FiniteSet a ->  FiniteSet a ->  FiniteSet a
union s1 s2 = fromList ((toList s1) ++ (toList s2))

-- | Flatten a set of sets into a set. All sets will be unified.
--
-- >>> flatten Empty
-- {}
--
-- >>> flatten (singleton (singleton True))
-- {True}
--
-- @since 0.1.0.0
flatten :: (Finite a, Eq a) => FiniteSet (FiniteSet a) -> FiniteSet a
flatten = fold union Empty

-- | Convert a `FiniteSet` into a `Prelude.List`
--
-- >>> toList Empty
-- []
--
-- >>> toList (singleton True)
-- [True]
--
-- @since 0.1.0.0
toList :: (Finite a) => FiniteSet a -> [a]
toList Empty = []
toList (Set s x) = x : (toList s)

-- | Convert a `FiniteSet` into a `Data.Set.Set`
--
-- >>> toSet Empty
-- fromList []
--
-- >>> toSet (singleton True)
-- fromList [True]
--
-- @since 0.1.0.0
toSet :: (Finite a, Ord a) => FiniteSet a -> Set.Set a
toSet = Set.fromList . toList

-- | Filter all subsets which contain at least all the elements of the given list - i.e. where the list represents a subset of them.
--
-- >>> filterListSubsetOf [True] (elements :: [FiniteSet Bool])
-- [{True},{False,True}]
--
-- @since 0.1.0.0
filterListSubsetOf :: forall a. (Finite a, Eq a) => [a] -> [FiniteSet a] -> [FiniteSet a]
filterListSubsetOf xs ss = Prelude.filter (\s -> all (\x -> elementOf x s) xs) ss

-- | Filter all subsets wwhich contain no elements other than any element from the given list - i.e. where the list represents a superset of them.
--
-- >>> filterListSupersetOf [True] (elements :: [FiniteSet Bool])
-- [{},{True}]
--
-- @since 0.1.0.0
filterListSupersetOf :: forall a. (Finite a, Eq a) => [a] -> [FiniteSet a] -> [FiniteSet a]
filterListSupersetOf xs ss = Prelude.filter (\s -> all (\y -> elem y xs) (toList s)) ss

-- | __internal only:__ Bring representation of a set into normal ordering. (Required by some operations.) Normal order is defined by occurrence in `elements`
--
-- >>>elements :: [FiniteSet Bool]
-- [{},{True},{False},{False,True}]
-- >>> s = (Set (Set Empty False) True)
-- >>> s
-- {True,False}
-- >>> normalOrdered s
-- {False,True}
--
-- @since 0.1.0.0
normalOrdered :: forall a. (Finite a, Eq a) => FiniteSet a -> FiniteSet a
-- select an equivalent set from the element list
normalOrdered s = head . (filterListSubsetOf xs) . (filterListSupersetOf xs) $ (elements :: [FiniteSet a]) where
    xs = toList s

-- | Construct a set from a list.
--
-- >>> fromList []
-- {}
--
-- >>> fromList [True]
-- {True}
--
-- >>> fromList [True, True]
-- {True}
--
-- @since 0.1.0.0
fromList :: forall a. (Finite a, Eq a) => [a] -> FiniteSet a
fromList xs = head . (filterListSubsetOf xs) . (filterListSupersetOf xs) $ (elements :: [FiniteSet a])

-- | Construct a set from a set.
--
-- >>> import Data.Set as Set
-- >>> fromSet Set.empty
-- {}
-- >>> fromSet (Set.singleton True)
-- {True}
--
-- @since 0.1.0.0
fromSet :: forall a. (Finite a, Eq a) => Set.Set a -> FiniteSet a
fromSet = fromList . Set.toList

-- | Construct a set from a single element.
--
-- >>> singleton True
-- {True}
--
-- @since 0.1.0.0
singleton :: (Finite a, Eq a) => a -> FiniteSet a
singleton x = fromList [x]

-- | Apply a function to each element of the set and collect the results in a set.
--
-- >>> map not (singleton True)
-- {False}
--
-- >>> s = fromList [False,True]
-- >>> s
-- {False,True}
-- >>> map not s
-- {False,True}
--
-- >>> map (\b -> False) (fromList [False,True])
-- {False}
--
-- @since 0.1.0.0
map :: (Finite a, Finite b, Eq b) => (a -> b) -> FiniteSet a -> FiniteSet b
map f s = (normalOrdered . fromList) (Prelude.map f (toList s))

-- | __internal only__
-- Construct the set of all possible subsets from the elements of a given set.
-- Assertion: Each element occurs exactly once in the list.
-- Warning: The resulting sets are not normal ordered!
--
-- >>> powerList []
-- [{}]
--
-- >>> powerList [True]
-- [{},{True}]
--
-- @since 0.1.0.0
powerList :: (Finite a) => [a] -> [FiniteSet a]
powerList [] = [Empty]
powerList (x:ys) = (powerList ys) ++ [Set y x | y <- (powerList ys)]

-- | Construct the set of all possible subsets of a given set.
--
-- >>> powerSet Empty
-- {{}}
--
-- >>> powerSet (singleton True)
--- {{},{True}}
--
-- @since 0.1.0.0
powerSet :: (Finite a, Eq a) => FiniteSet a -> FiniteSet (FiniteSet a)
powerSet Empty = Set Empty Empty
powerSet (Set s x) = union (powerSet s) (map (\y -> Set y x) (powerSet s))

-- | Filter a set with a given predicate.
--
-- >>> filter not Empty
-- {}
--
-- >>> s = fromList [False, True]
-- >>> s
-- {False,True}
-- >>> filter id s
-- {True}
--
-- @since 0.1.0.0
filter :: (Finite a, Eq a) => (a -> Bool) -> FiniteSet a -> FiniteSet a
filter f = fromList . (Prelude.filter f) . toList

-- | __internal only__: Auxiliar for `show`.
-- Shows the elements of a set seperated by ",".
-- @since 0.1.0.0
powerSetShowCore :: (Finite a, Show a) => FiniteSet a -> String
powerSetShowCore Empty = ""
powerSetShowCore (Set Empty x) = (show x)
powerSetShowCore (Set s x) = (show x) ++ "," ++ (powerSetShowCore s)

-- | @since 0.1.0.0
instance (Finite a, Show a) => Show (FiniteSet a) where
    show s = "{" ++ (powerSetShowCore s) ++ "}"

-- | @since 0.1.0.0
instance (Finite a, Eq a) => Eq (FiniteSet a) where
    (==) = equal

-- | @since 0.1.0.0
instance (Finite a) => Finite (FiniteSet a) where
    elements = powerList (elements :: [a])

-- | @since 0.1.0.0
instance (Finite a, Ord a) => Ord (FiniteSet a) where
    (<=) = leq
