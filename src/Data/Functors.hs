{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functors
  ( V(..), Variance(..)
  , Functor(..), Bifunctor(..), Trifunctor(..)
  , fmap, contramap, invmap
  , bimap, first, second, dimap, lmap, rmap, invmap2
  ) where

import Prelude hiding (Functor, fmap)

import Data.Functors.Types

-- ** Functor

fmap :: Functor f Covariant => (a -> b) -> f a -> f b
fmap = mapf

contramap :: Functor f Contravariant => (b -> a) -> f a -> f b
contramap = mapf

invmap :: Functor f Invariant => (a -> b) -> (b -> a) -> f a -> f b
invmap f f' = mapf (f, f')

-- ** Bifunctor

bimap :: Bifunctor f Covariant Covariant => (a -> b) -> (c -> d) -> f a c -> f b d
bimap = mapbi

first :: Bifunctor f Covariant Covariant => (a -> b) -> f a c -> f b c
first f = mapbi f id

second :: Bifunctor f Covariant Covariant => (c -> d) -> f a c -> f a d
second = mapbi id

dimap :: Bifunctor f Contravariant Covariant => (b -> a) -> (c -> d) -> f a c -> f b d
dimap = mapbi

lmap :: Bifunctor f Contravariant Covariant => (b -> a) -> f a c -> f b c
lmap f = mapbi f id

rmap :: Bifunctor f Contravariant Covariant => (c -> d) -> f a c -> f a d
rmap = mapbi id

invmap2 :: Bifunctor f Invariant Invariant => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> f a b -> f c d
invmap2 f f' g g' = mapbi (f, f') (g, g')

-- ** Trifunctors

--trimap
