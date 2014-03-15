{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Roles
  ( Representational(rep)
  , new, eta
  ) where

import Control.Concurrent.STM
import Control.Lens hiding (coerce)
import "mtl" Control.Monad.Reader
import Control.Monad.ST
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import Data.Complex
import Data.Functor.Compose
import Data.Proxy
import Data.Type.Coercion
import GHC.Prim (Coercible, coerce)
import Unsafe.Coerce

coerce1 :: Coercible a b => Coercion a c -> Coercion b c
coerce1 = coerce

coerce2 :: Coercible b c => Coercion a b -> Coercion a c
coerce2 = coerce

new :: (Rewrapping s t, Coercible (Unwrapped s) s, Coercible (Unwrapped t) t) => Coercion (Unwrapped s) (Unwrapped t) -> Coercion s t
new = coerce1 . coerce2

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
  rep = new.rep.rep

instance Representational m => Representational (StateT s m) where
  rep = new.rep.rep.eta.rep

instance Representational m => Representational (ReaderT e m) where
  rep = new.rep.rep

instance Representational m => Representational (WriterT w m) where
  rep = new.rep.eta.rep
