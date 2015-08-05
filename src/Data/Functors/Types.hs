{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functors.Types where

import Prelude hiding (Functor, fmap)

import Data.Bifunctor.Biff
import Data.Bifunctor.Clown
import Data.Bifunctor.Flip
import Data.Bifunctor.Joker
import Data.Functor.Sum (Sum(..))
import Data.Functor.Product (Product(..))
import Data.Profunctor.Ran (Ran(..))
import Data.Proxy

import Control.Applicative (Const(..))

-- * Morphisms

data V = Covariant | Contravariant | Invariant | Bivariant

type family Morphism v a b where
  Morphism Covariant a b = a -> b
  Morphism Contravariant a b = b -> a
  Morphism Invariant a b = (a -> b, b -> a)
  Morphism Bivariant a b = Proxy b

covariantId :: Morphism Covariant a a
covariantId = id

contravariantId :: Morphism Contravariant a a
contravariantId = id

invariantId :: Morphism Invariant a a
invariantId = (id, id)

bivariantId :: Morphism Bivariant a a
bivariantId = Proxy

-- * Functors

-- ** Functor

class Functor f v | f -> v where
  mapf :: Morphism v a b -> f a -> f b

-- ** Bifunctor

class Bifunctor p v w | p -> v, p -> w where
  mapbi :: Morphism v a c -> Morphism w b d -> p a b -> p c d

-- ** Trifunctors

class Trifunctor f v w x | f -> v, f -> w, f -> x where
  maptri :: Morphism v a a' -> Morphism w b b' -> Morphism x c c' -> f a b c -> f a' b' c'

-- * Instances

-- ** Function

instance Functor ((->) r) Covariant where
  mapf f g = f . g

instance Bifunctor (->) Contravariant Covariant where
  mapbi f g h = g . h . f

-- ** Tuple

instance Functor ((,) a) Covariant where
  mapf f (a, b) = (a, f b)

instance Bifunctor (,) Covariant Covariant where
  mapbi f g (a, b) = (f a, g b)

-- ** List

instance Functor [] Covariant where
  mapf = map

-- ** Either

instance Functor (Either a) Covariant where
  mapf _ (Left a) = Left a
  mapf f (Right b) = Right (f b)

instance Bifunctor Either Covariant Covariant where
  mapbi f _ (Left a) = Left (f a)
  mapbi _ g (Right a) = Right (g a)

-- ** Const

instance Functor (Const a) Bivariant where
  mapf _ (Const a) = Const a

instance Bifunctor Const Covariant Bivariant where
  mapbi f _ (Const a) = Const (f a)

-- ** Proxy

instance Functor Proxy Bivariant where
  mapf _ _ = Proxy

-- ** Joker

instance Functor g v => Functor (Joker g a) v where
  mapf f (Joker a) = Joker (mapf f a)

instance Functor g v => Bifunctor (Joker g) Bivariant v where
  mapbi _ f (Joker a) = Joker (mapf f a)

-- ** Clown

instance Functor (Clown g a) Bivariant where
  mapf _ (Clown a) = Clown a

instance Functor g v => Bifunctor (Clown g) v Bivariant where
  mapbi f _ (Clown a) = Clown (mapf f a)

-- ** Sum

instance (Functor f v, Functor g v) => Functor (Sum f g) v where
  mapf f (InL a) = InL (mapf f a)
  mapf f (InR b) = InR (mapf f b)

-- ** Product

instance (Functor f v, Functor g v) => Functor (Product f g) v where
  mapf f (Pair a b) = Pair (mapf f a) (mapf f b)

-- ** Ran

instance (v ~ Contravariant, w ~ Covariant, Bifunctor p v w, Bifunctor q v w) =>
  Bifunctor (Ran p q) v w where
    mapbi f g (Ran a) = Ran $ mapbi (mapbi id f) (mapbi id g) a

-- ** Biff

instance (Bifunctor p Covariant Covariant, Functor f v, Functor g w) =>
  Bifunctor (Biff p f g) v w where
    mapbi f g (Biff a) = Biff $ mapbi (mapf f) (mapf g) a

-- ** Flip

instance Bifunctor p v w => Bifunctor (Flip p) w v where
  mapbi f g (Flip a) = Flip $ mapbi g f a

-- ** Triple

instance Trifunctor (,,) Covariant Covariant Covariant where
  maptri f g h (a, b, c) = (f a, g b, h c)
