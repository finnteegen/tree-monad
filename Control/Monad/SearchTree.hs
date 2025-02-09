{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Control.Monad.SearchTree
-- Copyright   : Sebastian Fischer
-- License     : BSD3
--
-- Maintainer  : Niels Bunkenburg (nbu@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
--
-- This Haskell library provides an implementation of the MonadPlus
-- type class that represents the search space as a tree whose
-- constructors represent mzero, return, and mplus.
--
-- Such a tree can be used to implement different search strategies,
-- e.g., by using a queue. It can also be used as a basis for parallel
-- search strategies that evaluate different parts of the search space
-- concurrently.
module Control.Monad.SearchTree ( SearchTree(..), Search, searchTree ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix

-- |
-- The type @SearchTree a@ represents non-deterministic computations
-- as a tree structure.
data SearchTree a = None | One a | Choice (SearchTree a) (SearchTree a)
 deriving Show

instance Functor SearchTree where
  fmap _ None         = None
  fmap f (One x)      = One (f x)
  fmap f (Choice s t) = Choice (fmap f s) (fmap f t)

instance Applicative SearchTree where
  pure  = return

  (<*>) = ap

instance Alternative SearchTree where
  empty = mzero

  (<|>) = mplus

instance Monad SearchTree where
  return           = One

  None >>= _       = None
  One x >>= f      = f x
  Choice s t >>= f = Choice (s >>= f) (t >>= f)

instance MonadFail SearchTree where
  fail _ = None

instance MonadPlus SearchTree where
  mzero = None

  mplus = Choice

instance MonadFix SearchTree where
  mfix f = case fix (f . unOne) of
             None       -> None
             One x      -> One x
             Choice _ _ -> Choice (mfix (leftChoice . f)) (mfix (rightChoice . f))
    where
      unOne (One x) = x
      unOne _       = error "mfix SearchTree: not One"
      leftChoice (Choice s _) = s
      leftChoice _            = error "mfix SearchTree: not Choice"
      rightChoice (Choice _ t) = t
      rightChoice _            = error "mfix SearchTree: not Choice"

-- |
-- Another search monad based on continuations that produce search
-- trees.
newtype Search a = Search
  { -- | Passes a continuation to a monadic search action.
    search :: forall r. (a -> SearchTree r) -> SearchTree r
  }

-- | Computes the @SearchTree@ representation of a @Search@ action.
searchTree :: Search a -> SearchTree a
searchTree a = search a One

instance Functor Search where
  fmap f a = Search (\k -> search a (k . f))

instance Applicative Search where
  pure  = return

  (<*>) = ap

instance Alternative Search where
  empty = mzero

  (<|>) = mplus

instance Monad Search where
  return x = Search ($ x)

  a >>= f = Search (\k -> search a (\x -> search (f x) k))

instance MonadFail Search where
  fail _ = mzero

instance MonadPlus Search where
  mzero       = Search (const mzero)

  a `mplus` b = Search (\k -> search a k `mplus` search b k)

instance MonadFix Search where
  mfix f = Search (\k -> mfix (searchTree . f) >>= k)
