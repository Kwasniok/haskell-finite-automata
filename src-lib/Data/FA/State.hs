{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.FA.State
-- Description : Class for state types.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Each state type must have finitely many instances which can be accessed via @states@.
--
-- @since 0.1.0.0

module Data.FA.State (
    State,
    states,
) where

import Data.Collection.Finite
import Data.Collection.FiniteSet

-- | Type class to mark a type as a collection of states.
--
-- @since 0.1.0.0
class (Finite a, Eq a) => State a where
    -- | List of all possible states.
    --
    -- >>> states :: [Bool]
    -- [False,True]
    --
    -- @since 0.1.0.0
    states :: [a]
    states = elements

instance State () where

instance State Bool where

instance (State a, State b) => State (a,b)

instance (State a) => State (FiniteSet a) where
