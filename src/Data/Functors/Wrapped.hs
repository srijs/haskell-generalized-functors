{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Functors.Wrapped where

import Prelude hiding (Functor, fmap)
import qualified Prelude

import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Functor.Invariant as Invariant
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Profunctor as Profunctor

import Data.Functors

newtype WrappedFunctor f a = WrappedFunctor { unwrapFunctor :: f a }

instance Prelude.Functor f => Functor (WrappedFunctor f) Covariant where
  fmap f (WrappedFunctor a) = WrappedFunctor (Prelude.fmap f a)

newtype WrappedContravariant f a = WrappedContravariant { unwrapCovariant :: f a }

instance Contravariant.Contravariant f => Functor (WrappedContravariant f) Contravariant where
  fmap f (WrappedContravariant a) = WrappedContravariant (Contravariant.contramap f a)

newtype WrappedInvariant f a = WrappedInvariant { unwrapInvariant :: f a }

instance Invariant.Invariant f => Functor (WrappedInvariant f) Invariant where
  fmap (f, f') (WrappedInvariant a) = WrappedInvariant (Invariant.invmap f f' a)

newtype WrappedInvariant2 f a b = WrappedInvariant2 { unwrapInvariant2 :: f a b }

instance Invariant.Invariant2 f => Functor (WrappedInvariant2 f a) Invariant where
  fmap (f, f') (WrappedInvariant2 a) = WrappedInvariant2 (Invariant.invmap2 id id f f' a)

instance Invariant.Invariant2 f => Bifunctor (WrappedInvariant2 f) Invariant Invariant where
  bimap (f, f') (g, g') (WrappedInvariant2 a) = WrappedInvariant2 (Invariant.invmap2 f f' g g' a)

newtype WrappedBifunctor f a b = WrappedBifunctor { unwrapBifunctor :: f a b }

instance Bifunctor.Bifunctor f => Functor (WrappedBifunctor f a) Covariant where
  fmap f (WrappedBifunctor a) = WrappedBifunctor (Bifunctor.bimap id f a)

instance Bifunctor.Bifunctor f => Bifunctor (WrappedBifunctor f) Covariant Covariant where
  bimap f g (WrappedBifunctor a) = WrappedBifunctor (Bifunctor.bimap f g a)

newtype WrappedProfunctor f a b = WrappedProfunctor { unwrapProfunctor :: f a b }

instance Profunctor.Profunctor f => Functor (WrappedProfunctor f a) Covariant where
  fmap f (WrappedProfunctor a) = WrappedProfunctor (Profunctor.dimap id f a)

instance Profunctor.Profunctor f => Bifunctor (WrappedProfunctor f) Contravariant Covariant where
  bimap f g (WrappedProfunctor a) = WrappedProfunctor (Profunctor.dimap f g a)
