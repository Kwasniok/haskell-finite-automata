{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Collection.Finite
-- Description : Finite data type.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definition of a class for data types with finitely many members and basic instances of it.
--
-- @since 0.1.0.0

module Data.Collection.Finite (
    Finite,
    elements,
) where

import Data.Maybe

-- | Class used to mark data types to have finitely many members (there exist finitely many objects of the given type).
--
-- __WARNING__: Marking a data type as `Finite` is done by the user and must be true in order to avoid undefined behaviour!
--
-- @since 0.1.0.0
class Finite a where
    -- | List of all members of the finite type @a@.
    --
    -- __WARNING__: It is essential that __each possible instance of the type @a@ is listed exactly once!__ If an instance of @a@ is constructable but not included in this list, __undefined behaviour__ might occur. Data types with infinitely many instances should __never__ be marked as `Finite`.
    --
    -- __INFO__: The given order of elements will be considered to define an order relation like `(Prelude.<=)`.
    --
    -- __ASSERTION__: Each possible instance of @a@ is listed exactly once.
    --
    -- @since 0.1.0.0
    elements :: [a]

-- | @since 0.1.0.0
instance Finite () where
    elements = [()]

-- | @since 0.1.0.0
instance Finite Bool where
    --  order in accordance with (<=)
    elements = [False, True]

-- | @since 0.1.0.0
instance (Finite a, Finite b) => Finite (a, b) where
    elements = [(x, y) | x <- (elements :: [a]), y <- elements :: [b]]

-- | @since 0.1.0.0
instance (Show a, Show b, Finite a) => Show (a -> b) where
    show f = show [(x, f x) | x <- elements :: [a]]

-- | @since 0.1.0.0
instance (Finite a) => Finite (Maybe a) where
    elements = [Nothing] ++ [Just x | x <- elements :: [a]]
