{-# LANGUAGE RecordWildCards #-}
module Bf.Compiler.Emit
  ( compileCmds
  ) where

import Bf.Compiler.Monad
import Bf.Compiler.Runtime (Runtime)
import qualified Bf.Compiler.Runtime as Rt
import Bf.Syntax
import Bf.Compiler.Combinators

import LLVM.AST.Operand
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

-- | Compiled code context with memory and index pointers.
data Memory = Memory
  { memRuntime :: !Runtime
  , memData    :: !Operand
  , memIndex   :: !Operand
  }
  deriving stock (Eq, Show)
    
-- | Compiles the code into the sequence of LLVM IR code.
compileCmds :: Integer -> [Cmd] -> Codegen Runtime ()
compileCmds size = createMemory size . go
  where
    go = traverse_ $ \case
      IncPtr    -> upd (`add` int32 1) =<< getIndexPtr
      DecPtr    -> upd (`sub` int32 1) =<< getIndexPtr
      IncDat    -> upd (`add` int32 1) =<< getValPtr
      DecDat    -> upd (`sub` int32 1) =<< getValPtr
      ChrGet    -> join (st <$> getValPtr <*> callGetChar)
      ChrPut    -> getValPtr >>= ld >>= callPutChar
      Loop cmds -> whileLoop (getValPtr >>= ld >>= isNonZero) $ go cmds

-- | Creates compiler context with memory region and index, lowering
--   it to the 'Codegen' 'Runtime' monad.
createMemory :: Integer -> Codegen Memory () -> Codegen Runtime ()
createMemory size m = do
  memData <- Rt.allocateOnHeap size
  memIndex <- Rt.allocateOnStack 0
  withCodegen (\rt -> Memory { memRuntime = rt, .. }) m
  Rt.callFree memData
  retVoid

-- | Gets pointer where buffer pointer lives.
getIndexPtr :: Codegen Memory Operand
getIndexPtr = asks memIndex
{-# INLINE getIndexPtr #-}

-- | Gets an effective address to the current value being focused.
getValPtr :: Codegen Memory Operand
getValPtr = do
  dataPtr <- asks memData
  idxPtr <- getIndexPtr
  idx <- ld idxPtr
  gep dataPtr [idx]

-- | Calls @getchar@ from runtime.
callGetChar :: Codegen Memory Operand
callGetChar = withCodegen memRuntime Rt.callGetChar
{-# INLINE callGetChar #-}

-- | Calls @putchar@ from runtime.
callPutChar :: Operand -> Codegen Memory ()
callPutChar op = withCodegen memRuntime (Rt.callPutChar op)
{-# INLINE callPutChar #-}