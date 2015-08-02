{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functors where

import Prelude hiding (Functor, fmap)
import qualified Prelude

import Data.Functor.Sum (Sum(..))
import Data.Functor.Product (Product(..))

import Control.Applicative (Const(..))

-- * Functors

-- ** Functor

class Functor f v | f -> v where
  fmap :: Morphism v a b -> f a -> f b

instance Functor ((->) r) Covariant where fmap = Prelude.fmap

instance Functor ((,) a) Covariant where fmap = Prelude.fmap

instance Functor [] Covariant where fmap = Prelude.fmap

instance Functor (Either a) Covariant where fmap = Prelude.fmap

instance Functor (Const a) Bivariant where fmap _ (Const a) = Const a

instance (Functor f v, Functor g v) => Functor (Sum f g) v where
  fmap f (InL a) = InL (fmap f a)
  fmap f (InR b) = InR (fmap f b)

instance (Functor f v, Functor g v) => Functor (Product f g) v where
  fmap f (Pair a b) = Pair (fmap f a) (fmap f b)

-- ** Bifunctor

class Bifunctor f v w | f -> v, f -> w where
  bimap :: Morphism v a c -> Morphism w b d -> f a b -> f c d

instance Bifunctor (->) Contravariant Covariant where
  bimap f g h = g . h . f

instance Bifunctor (,) Covariant Covariant where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor Either Covariant Covariant where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

instance Bifunctor Const Covariant Bivariant where
  bimap f _ (Const a) = Const (f a)

-- * Morphisms

data Covariant a b
data Contravariant a b
data Invariant a b
data Bivariant a b

type family Morphism (v :: * -> * -> *) a b
type instance Morphism Covariant a b = a -> b
type instance Morphism Contravariant a b = b -> a
type instance Morphism Invariant a b = (a -> b, b -> a)
type instance Morphism Bivariant a b = Either (a -> b) (b -> a)
