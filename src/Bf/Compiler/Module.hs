module Bf.Compiler.Module
  ( createModule
  , createFunction
  ) where

import Bf.Compiler.Monad
import Bf.Compiler.Runtime (Runtime)
import qualified Bf.Compiler.Runtime as Rt

import LLVM.AST.Name
import qualified LLVM.AST.Type as Ty
import LLVM.IRBuilder

-- | Creates module by applying runtime environment.
createModule :: Mgen Runtime () -> ModuleBuilder ()
createModule m = Rt.createRuntime >>= runReaderT (runMgen m)
{-# INLINE createModule #-}

-- | Creates a function in module builder.
createFunction :: Name -> Codegen Runtime () -> Mgen Runtime ()
createFunction fnName codegen = Mgen . ReaderT $ \rt ->
  void $ function fnName [] Ty.void $ \_ ->
    usingReaderT rt (runCodegen codegen)
{-# INLINE createFunction #-}
