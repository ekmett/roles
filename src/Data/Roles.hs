{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Roles
  ( Representational(..)
  ) where

import Control.Monad.ST
import Control.Concurrent.STM
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import Data.Complex
import Data.Functor.Compose
import Data.Proxy
import Data.Type.Coercion
import GHC.Prim (Coercible, coerce)
import Unsafe.Coerce

repOf :: Representational f => proxy a -> Coercion a b -> Coercion (f a) (f b)
repOf _ = rep

coerce1 :: Coercible a b => Coercion a c -> Coercion b c
coerce1 = coerce

coerce2 :: Coercible b c => Coercion a b -> Coercion a c
coerce2 = coerce

eta :: Coercion f g -> Coercion (f a) (g a)
eta = unsafeCoerce

class Representational (t :: k1 -> k2) where
  rep :: Coercion a b -> Coercion (t a) (t b)

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

instance (Representational f, Representational g) => Representational (Compose f g) where
  rep p = case rep $ rep p of
    (Coercion :: Coercion (f (g a)) (f (g b))) -> Coercion

instance Representational m => Representational (StateT s m) where
  rep p = case rep $ rep $ eta $ rep p of
    (Coercion :: Coercion (s -> m (a, s)) (s -> m (b, s))) -> Coercion

instance Representational m => Representational (ReaderT e m) where
  rep p = case rep $ rep p of
    (Coercion :: Coercion (e -> m a) (e -> m b)) -> Coercion

instance Representational m => Representational (WriterT w m) where
  rep p = case rep $ eta $ rep p of
    (Coercion :: Coercion (m (a, w)) (m (b, w))) -> Coercion
