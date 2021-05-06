{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

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

data FiniteSet a where
    Empty :: (Finite a) => FiniteSet a
    Set :: (Finite a) => FiniteSet a -> a -> FiniteSet a

elementOf :: (Eq a) => a -> FiniteSet a -> Bool
elementOf _ Empty = False
elementOf x (Set s y) = (x == y) || (elementOf x s)

containsAnyOf :: (Eq a) => FiniteSet a -> [a] -> Bool
containsAnyOf s xs = any (\x -> elementOf x s) xs

equal :: (Eq a) => FiniteSet a -> FiniteSet a -> Bool
equal Empty Empty = True
equal Empty (Set _ _) = False
equal (Set _ _) Empty = False
equal (Set s1 x1) (Set s2 x2) = (x1 == x2) && (equal s1 s2)

leq :: (Eq a) => FiniteSet a -> FiniteSet a -> Bool
leq Empty _ = True
leq (Set _ _) Empty = False
leq (Set s1 x1) (Set s2 x2) = (x1 == x2) && (leq s1 s2)

size :: (Finite a) => FiniteSet a -> Int
size  Empty = 0
size (Set s _) = 1 + (size s)

fold :: (Finite a) => (a -> b -> b) -> b -> FiniteSet a -> b
fold _ e Empty = e
fold f e (Set s x) = f x (fold f e s)

union :: (Finite a, Eq a) => FiniteSet a ->  FiniteSet a ->  FiniteSet a
union s1 s2 = fromList ((toList s1) ++ (toList s2))

flatten :: (Finite a, Eq a) => FiniteSet (FiniteSet a) -> FiniteSet a
flatten = fold union Empty

toList :: (Finite a) => FiniteSet a -> [a]
toList Empty = []
toList (Set s x) = x : (toList s)

toSet :: (Finite a, Ord a) => FiniteSet a -> Set.Set a
toSet = Set.fromList . toList

-- filter all subsets which contain at least all the elements of the list
filterListFiniteSetOf :: forall a. (Finite a, Eq a) => [a] -> [FiniteSet a] -> [FiniteSet a]
filterListFiniteSetOf xs ss = Prelude.filter (\s -> all (\x -> elementOf x s) xs) ss

-- filter all subsets which contain no elements other than any element from the list
filterListSupersetOf :: forall a. (Finite a, Eq a) => [a] -> [FiniteSet a] -> [FiniteSet a]
filterListSupersetOf xs ss = Prelude.filter (\s -> all (\y -> elem y xs) (toList s)) ss

normalOrdered :: forall a. (Finite a, Eq a) => FiniteSet a -> FiniteSet a
normalOrdered s = head . (filterListFiniteSetOf xs) . (filterListSupersetOf xs) $ (elements :: [FiniteSet a]) where
    xs = toList s

-- select the subset which has exactly the same elements as the list (repitition and order does not matter)
fromList :: forall a. (Finite a, Eq a) => [a] -> FiniteSet a
fromList xs = head . (filterListFiniteSetOf xs) . (filterListSupersetOf xs) $ (elements :: [FiniteSet a])

-- select the subset which has exactly the same elements as the set
fromSet :: forall a. (Finite a, Eq a) => Set.Set a -> FiniteSet a
fromSet = fromList . Set.toList

-- select the subset which has exactly the element given
singleton :: (Finite a, Eq a) => a -> FiniteSet a
singleton x = fromList [x]

map :: (Finite a, Finite b, Eq b) => (a -> b) -> FiniteSet a -> FiniteSet b
map f s = (normalOrdered . fromList) (Prelude.map f (toList s))

powerlist :: (Finite a) => [a] -> [FiniteSet a]
powerlist [] = [Empty]
powerlist (x:ys) = (powerlist ys) ++ [Set y x | y <- (powerlist ys)]

powerSet :: (Finite a, Eq a) => FiniteSet a -> FiniteSet (FiniteSet a)
powerSet Empty = Set Empty Empty
powerSet (Set s x) = union (powerSet s) (map (\y -> Set y x) (powerSet s))

filter :: (Finite a, Eq a) => (a -> Bool) -> FiniteSet a -> FiniteSet a
filter f = fromList . (Prelude.filter f) . toList

powerSetShowCore :: (Finite a, Show a) => FiniteSet a -> String
powerSetShowCore Empty = ""
powerSetShowCore (Set Empty x) = (show x)
powerSetShowCore (Set s x) = (show x) ++ "," ++ (powerSetShowCore s)

instance (Finite a, Show a) => Show (FiniteSet a) where
    show s = "{" ++ (powerSetShowCore s) ++ "}"

instance (Finite a, Eq a) => Eq (FiniteSet a) where
    (==) = equal

instance (Finite a) => Finite (FiniteSet a) where
    elements = powerlist (elements :: [a])

instance (Finite a, Ord a) => Ord (FiniteSet a) where
    (<=) = leq
