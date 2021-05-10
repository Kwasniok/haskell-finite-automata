{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.FA.Base
-- Description : __(internal only)__ Fundamental module of Data.FA.
-- Copyright   : (c) Kwasniok, 2021
-- License     : BSD-3-Clause
-- Maintainer  : Kwasniok@users.noreply.github.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains fundamental type classes of finite automata.
--
-- @since 0.1.0.0

module Data.FA.Base (
    FARead,
    FAAccept,
    readSymbol,
    readWord,
    accepts,
) where

import Data.FA.Symbol
import Data.FA.State

-- | Finite automata with capability to read a word or a single symbol and return the state.
--
-- @since 0.1.0.0
class (State a, Symbol b) => FARead a b c where
    -- | Obtains the state of a finite automaton after reading a single symbol based on an arbitrary initial state.
    --
    -- @since 0.1.0.0
    readSymbol :: c -- ^ finite automaton
               -> a -- ^ initial state
               -> b -- ^ symbol to read
               -> a -- ^ state after reading the symbol

    -- | Obtains the state of a finite automaton after reading an entire word.
    --
    -- @since 0.1.0.0
    readWord :: c   -- ^ finite automaton
             -> [b] -- ^ word to read
             -> a   -- ^ state after reading the word

-- | Finite automata with capability to accept (or reject) words.
--
-- @since 0.1.0.0
class (Symbol b) => FAAccept b c where
    -- | Checks if a finite automaton accepts a word. Acceptance of a word is equivalent to the word being part of the regular language described by the automaton.
    --
    -- @since 0.1.0.0
    accepts :: c    -- ^ finite automaton
            -> [b]  -- ^ word to read
            -> Bool
