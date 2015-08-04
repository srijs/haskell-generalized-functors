{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functors where

import Prelude hiding (Functor, fmap)

import Data.Functor.Sum (Sum(..))
import Data.Functor.Product (Product(..))
import Data.Proxy

import Control.Applicative (Const(..))

-- * Morphisms

data Variance = Covariant | Contravariant | Invariant | Bivariant

type family Morphism (v :: Variance) a b
type instance Morphism Covariant a b = a -> b
type instance Morphism Contravariant a b = b -> a
type instance Morphism Invariant a b = (a -> b, b -> a)
type instance Morphism Bivariant a b = Proxy b

-- * Functors

-- ** Functor

class Functor f v | f -> v where
  mapf :: Morphism v a b -> f a -> f b

fmap :: Functor f Covariant => (a -> b) -> f a -> f b
fmap = mapf

contramap :: Functor f Contravariant => (b -> a) -> f a -> f b
contramap = mapf

invmap :: Functor f Invariant => (a -> b) -> (b -> a) -> f a -> f b
invmap f f' = mapf (f, f')

instance Functor ((->) r) Covariant where
  mapf f g = f . g

instance Functor ((,) a) Covariant where
  mapf f (a, b) = (a, f b)

instance Functor [] Covariant where
  mapf = map

instance Functor (Either a) Covariant where
  mapf _ (Left a) = Left a
  mapf f (Right b) = Right (f b)

instance Functor (Const a) Bivariant where
  mapf _ (Const a) = Const a

instance Functor Proxy Bivariant where
  mapf _ _ = Proxy

instance (Functor f v, Functor g v) => Functor (Sum f g) v where
  mapf f (InL a) = InL (mapf f a)
  mapf f (InR b) = InR (mapf f b)

instance (Functor f v, Functor g v) => Functor (Product f g) v where
  mapf f (Pair a b) = Pair (mapf f a) (mapf f b)

-- ** Bifunctor

class Bifunctor f v w | f -> v, f -> w where
  mapbi :: Morphism v a c -> Morphism w b d -> f a b -> f c d

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

instance Bifunctor (->) Contravariant Covariant where
  mapbi f g h = g . h . f

instance Bifunctor (,) Covariant Covariant where
  mapbi f g (a, b) = (f a, g b)

instance Bifunctor Either Covariant Covariant where
  mapbi f _ (Left a) = Left (f a)
  mapbi _ g (Right b) = Right (g b)

instance Bifunctor Const Covariant Bivariant where
  mapbi f _ (Const a) = Const (f a)

-- ** Trifunctors

class Trifunctor f v w x | f -> v, f -> w, f -> x where
  maptri :: Morphism v a a' -> Morphism w b b' -> Morphism x c c' -> f a b c -> f a' b' c'

instance Trifunctor (,,) Covariant Covariant Covariant where
  maptri f g h (a, b, c) = (f a, g b, h c)
