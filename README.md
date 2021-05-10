# haskell-finite-automata
A cabal package for finite automata in [haskell](https://www.haskell.org).

## Dependencies
- Haskell Compiler/Interactive Interpreter: [GHC/GHCi](https://www.haskell.org/ghc)
- Package Manager for Haskell: [Cabal](https://www.haskell.org/cabal)

## Usage
Use `cabal repl` to initiate the interactive REPL (Read Evaluate Print Loop).

## Examples

There are two types of finite automata implemented:
* deterministic finite automata (DFAs)
* non-deterministic finite automata (NFAs)

### Accepting Words
Determines if a word is accepted by the automaton or not.

Automaton accepts words ending with `True`:
```
>>> -- Deterministic finite automaton with two states labeled `False` and `True`. It starts in state `False` and accepts a word if and only if it ends up in state `True`. Words are of the type [Bool].
>>> -- transition function: Go from state `q` by reading a symbol `s` to state `s`.
>>> -- t :: Bool -> Bool -> Bool
>>> t q s = s
>>> singleton creates a set with one element
>>> dfa = MkDFA t False (singleton True)
>>> -- accepts
>>> accepts dfa [False]
False
>>> accepts dfa [False, True]
True
```

Automaton accepts **any** word from the alphabet `{False, True}`:
```
>>> -- Deterministic finite automaton with one state `()` accepting any word from the alphabet {False, True}.
>>> -- transition function: Go from the unit state `q = ()` by reading a symbol `s` to the unit state.
>>> -- t :: () -> Bool -> ()
>>> t q s = ()
>>> dfa :: DFA () Bool
>>> dfa = MkDFA t () (singleton ())
>>> -- accepts
>>> accepts dfa [False]
True
>>> accepts dfa [False, True]
True
>>> -- empty word
>>> e = [] :: [Bool]
>>> accepts dfa e
True
```

### Reading Symbols
Gives an insight on the state transfer when reading a single symbol.

Automaton accepts words ending with `True`:
```
>>> -- Deterministic finite automaton with two states labeled `False` and `True`. It starts in state `False` and accepts a word if and only if it ends up in state `True`. Words are of the type [Bool].
>>> -- transition function: Go from state `q` by reading a symbol `s` to state `s`.
>>> -- t :: Bool -> Bool -> Bool
>>> t q s = s
>>> singleton creates a set with one element
>>> dfa = MkDFA t False (singleton True)
>>> -- from a given initial state read a symbol and determine the new state
>>> -- staring in state `False` read `True`
>>> readSymbol dfa False True
True
>>> readSymbol dfa True True
True
>>> readSymbol dfa True False
False
```

### Reading (Partial) Words
Gives an insight on the state transfer when reading a word

Automaton accepts words ending with `True`:
```
>>> -- GHC extension needed:
>>> :set -XFlexibleContexts
>>> -- Deterministic finite automaton with two states labeled `False` and `True`. It starts in state `False` and accepts a word if and only if it ends up in state `True`. Words are of the type [Bool].
>>> -- transition function: Go from state `q` by reading a symbol `s` to state `s`.
>>> -- t :: Bool -> Bool -> Bool
>>> t q s = s
>>> singleton creates a set with one element
>>> dfa = MkDFA t False (singleton True)
>>> -- from a given initial state read a symbol and determine the new state
>>> -- staring in state `False` read `True`
>>> readWord dfa [True] :: Bool
True
>>> readWord dfa [True, False] :: Bool
False
>>> -- empty word
>>> e = [] :: [Bool]
>>> readWord dfa e :: Bool
False
```

### DFA to NFA Conversion
Convert between equivalent DFAs and NFAs which accept exactly the same words.

Automaton accepts words ending with `True`:
```
>>> t q s = s
>>> dfa = MkDFA t False (singleton True)
>>> :t dfa
dfa :: DFA Bool Bool
>>> nfa = dfaToNfa dfa
>>> :t nfa
nfa :: NFA Bool Bool
>>> -- accepts
>>> accepts nfa [False]
False
>>> accepts nfa [False, True]
True
```
Use `dfa' = nfaToDfa nfa` for an equivalent inverse conversion.

NFAs behave like DFAs where the states become sets of states:
```
>>> t q s = s
>>> dfa = MkDFA t False (singleton True)
>>> :t dfa
dfa :: DFA Bool Bool
>>> nfa = dfaToNfa dfa
>>> :t nfa
nfa :: NFA Bool Bool
>>> -- read a symbol starting at the given initial states
>>> -- fromList converts a list to a set
>>> readSymbol nfa (fromList [False, True]) True
{True}
```
