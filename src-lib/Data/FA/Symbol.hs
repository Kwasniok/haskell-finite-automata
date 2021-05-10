{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.FA.Symbol
-- Description : Class for symbol types.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Each symbol type must have finitely many instances which can be accessed via @alphabet@.
--
-- @since 0.1.0.0

module Data.FA.Symbol (
    Symbol,
    alphabet,
) where

import Data.Collection.Finite
import Data.Collection.FiniteSet

-- | Type class to mark a type as a collection of symbols.
--
-- @since 0.1.0.0
class (Finite a, Eq a) => Symbol a where
    -- | List of all possible symbols.
    --
    -- >>> alphabet :: [Bool]
    -- [False,True]
    --
    -- @since 0.1.0.0
    alphabet :: [a]
    alphabet = elements

instance Symbol () where

instance Symbol Bool where

instance (Symbol a, Symbol b) => Symbol (a,b)

instance (Symbol a) => Symbol (FiniteSet a) where
