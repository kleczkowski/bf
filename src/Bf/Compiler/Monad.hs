module Bf.Compiler.Monad
  ( Codegen (..)
  , withCodegen
  , Mgen (..)
  ) where

import Control.Monad.Fix
import LLVM.IRBuilder

-- | Code generation monad with an environment.
newtype Codegen r a = Codegen
  { runCodegen :: ReaderT r (IRBuilderT ModuleBuilder) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadReader r
    , MonadIRBuilder
    , MonadModuleBuilder
    )

-- | Maps contravariantly an environment of code generation monad.
withCodegen :: (r -> s) -> Codegen s a -> Codegen r a
withCodegen f = Codegen . withReaderT f . runCodegen
{-# INLINE withCodegen #-}

-- | Module builder with an environment.
newtype Mgen r a
  = Mgen
  { runMgen :: ReaderT r ModuleBuilder a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadModuleBuilder
    )
