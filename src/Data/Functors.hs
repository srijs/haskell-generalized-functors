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
import Data.Proxy

import Control.Applicative (Const(..))

-- * Functors

-- ** Functor

class Functor f v | f -> v where
  mapf :: Morphism v a b -> f a -> f b

instance Functor ((->) r) Covariant where mapf = Prelude.fmap

instance Functor ((,) a) Covariant where mapf = Prelude.fmap

instance Functor [] Covariant where mapf = Prelude.fmap

instance Functor (Either a) Covariant where mapf = Prelude.fmap

instance Functor (Const a) Bivariant where mapf _ (Const a) = Const a

instance Functor Proxy Bivariant where mapf _ _ = Proxy

instance (Functor f v, Functor g v) => Functor (Sum f g) v where
  mapf f (InL a) = InL (mapf f a)
  mapf f (InR b) = InR (mapf f b)

instance (Functor f v, Functor g v) => Functor (Product f g) v where
  mapf f (Pair a b) = Pair (mapf f a) (mapf f b)

-- ** Bifunctor

class Functor (f v) w => Bifunctor f v w | f -> v, f -> w where
  mapg :: Morphism v a c -> f a b -> f c b

instance Bifunctor (->) Contravariant Covariant where
  mapg f h = h . f

instance Bifunctor (,) Covariant Covariant where
  mapg f (a, b) = (f a, b)

instance Bifunctor Either Covariant Covariant where
  mapg f (Left a) = Left (f a)
  mapg _ (Right b) = Right b

instance Bifunctor Const Covariant Bivariant where
  mapg f (Const a) = Const (f a)

-- * Morphisms

data Covariant
data Contravariant
data Invariant
data Bivariant

type family Morphism v a b
type instance Morphism Covariant a b = a -> b
type instance Morphism Contravariant a b = b -> a
type instance Morphism Invariant a b = (a -> b, b -> a)
type instance Morphism Bivariant a b = Proxy b
