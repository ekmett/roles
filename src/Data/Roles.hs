{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Roles
  ( Representational(rep)
  , Phantom(phantom)
  , new, eta
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens hiding (coerce)
import "mtl" Control.Monad.Reader
import Control.Monad.ST
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import Data.Complex
import Data.Functor.Compose
import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Type.Coercion
import GHC.Prim (Coercible, coerce)
import Unsafe.Coerce

coerce1 :: Coercible a b => Coercion a c -> Coercion b c
coerce1 = coerce

coerce2 :: Coercible b c => Coercion a b -> Coercion a c
coerce2 = coerce

new :: (Rewrapping s t, Coercible (Unwrapped s) s, Coercible (Unwrapped t) t) => Coercion (Unwrapped s) (Unwrapped t) -> Coercion s t
new = coerce1 . coerce2

eta :: forall (f :: x -> y) (g :: x -> y) (a :: x). Coercion f g -> Coercion (f a) (g a)
eta = unsafeCoerce

class Representational (t :: k1 -> k2) where
  -- | An argument is representational if you can lift a coercion of the argument into one of the whole
  rep :: Coercion a b -> Coercion (t a) (t b)
  default rep :: Phantom t => Coercion a b -> Coercion (t a) (t b)
  rep _ = phantom

class Representational t => Phantom t where
  -- | An argument is phantom if you can coerce the whole ignoring the argument
  phantom :: Coercion (t a) (t b)
  default phantom :: Coercible (t a) (t b) => Coercion (t a) (t b)
  phantom = Coercion

instance Representational Proxy
instance Phantom Proxy

instance Representational Tagged
instance Phantom Tagged

instance Representational (Tagged a) where rep Coercion = Coercion

instance Representational (->)       where rep Coercion = Coercion
instance Representational ((->) a)   where rep Coercion = Coercion

instance Representational Either     where rep Coercion = Coercion
instance Representational (Either a) where rep Coercion = Coercion

instance Representational (,)     where rep Coercion = Coercion
instance Representational ((,) a) where rep Coercion = Coercion

instance Representational (,,)       where rep Coercion = Coercion
instance Representational ((,,) a)   where rep Coercion = Coercion
instance Representational ((,,) a b) where rep Coercion = Coercion

instance Representational (,,,)         where rep Coercion = Coercion
instance Representational ((,,,) a)     where rep Coercion = Coercion
instance Representational ((,,,) a b)   where rep Coercion = Coercion
instance Representational ((,,,) a b c) where rep Coercion = Coercion

instance Representational (,,,,)           where rep Coercion = Coercion
instance Representational ((,,,,) a)       where rep Coercion = Coercion
instance Representational ((,,,,) a b)     where rep Coercion = Coercion
instance Representational ((,,,,) a b c)   where rep Coercion = Coercion
instance Representational ((,,,,) a b c d) where rep Coercion = Coercion

instance Representational (,,,,,)             where rep Coercion = Coercion
instance Representational ((,,,,,) a)         where rep Coercion = Coercion
instance Representational ((,,,,,) a b)       where rep Coercion = Coercion
instance Representational ((,,,,,) a b c)     where rep Coercion = Coercion
instance Representational ((,,,,,) a b c d)   where rep Coercion = Coercion
instance Representational ((,,,,,) a b c d e) where rep Coercion = Coercion

instance Representational []      where rep Coercion = Coercion
instance Representational Complex where rep Coercion = Coercion
instance Representational Maybe   where rep Coercion = Coercion
instance Representational IO      where rep Coercion = Coercion
instance Representational (ST s)  where rep Coercion = Coercion
instance Representational STM     where rep Coercion = Coercion
instance Representational Sum     where rep Coercion = Coercion
instance Representational Product where rep Coercion = Coercion
instance Representational Dual    where rep Coercion = Coercion
instance Representational Endo    where rep Coercion = Coercion
instance Representational First   where rep Coercion = Coercion
instance Representational Last    where rep Coercion = Coercion

instance Representational WrappedMonad where rep Coercion = Coercion
instance Representational m => Representational (WrappedMonad m) where rep = new.rep

instance Representational WrappedArrow where rep Coercion = Coercion
instance Representational p => Representational (WrappedArrow p) where rep = unsafeCoerce
instance Representational (p a) => Representational (WrappedArrow p a) where rep = unsafeCoerce

instance Representational f => Representational (Compose f) where rep = unsafeCoerce
instance (Representational f, Representational g) => Representational (Compose f g) where rep = new.rep.rep

instance Representational m => Representational (StateT s m) where rep = new.rep.rep.eta.rep
instance Representational m => Representational (ReaderT e m) where rep = new.rep.rep
instance Representational m => Representational (WriterT w m) where rep = new.rep.eta.rep
