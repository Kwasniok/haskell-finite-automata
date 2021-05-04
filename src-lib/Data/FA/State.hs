{-# LANGUAGE ScopedTypeVariables #-}

module Data.FA.State (
    State,
    states,
) where

import Data.Collection.Finite

class (Finite a) => State a where
    states :: [a]
    states = elements

instance State () where

instance State Bool where

instance (State a, State b) => State (a,b)
