{-# LANGUAGE ScopedTypeVariables #-}

module Data.Collection.Finite (
    Finite,
    elements,
) where

class Finite a where
    elements :: [a]

instance Finite () where
    elements = [()]

instance Finite Bool where
    elements = [True, False]

instance (Finite a, Finite b) => Finite (a, b) where
    elements = [(x, y) | x <- (elements :: [a]), y <- elements :: [b]]

instance (Show a, Show b, Finite a) => Show (a -> b) where
    show f = show [(x, f x) | x <- elements :: [a]]
