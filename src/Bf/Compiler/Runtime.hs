{-# LANGUAGE RecordWildCards #-}
module Bf.Compiler.Runtime
  ( -- * Types
    Runtime
  , createRuntime

    -- * Emitting
  , callGetChar
  , callPutChar
  , allocateOnStack
  , allocateOnHeap
  , callFree
  ) where

import Bf.Compiler.Monad

import LLVM.AST.Operand
import LLVM.AST.Type hiding (void)
import qualified LLVM.AST.Type as Ty
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module

-- | Runtime environment.
data Runtime = Runtime
  { rtGetChar :: !Operand
  , rtPutChar :: !Operand
  , rtMalloc  :: !Operand
  , rtFree    :: !Operand
  , rtMemset  :: !Operand
  }
  deriving stock (Eq, Show)

-- | Emits an external definitions returning operands with
--   function pointers.
createRuntime :: MonadModuleBuilder m => m Runtime
createRuntime = do
  rtGetChar <- extern "getchar" [] i32
  rtPutChar <- extern "putchar" [i32] i32
  rtMalloc  <- extern "malloc" [i32] (ptr i8)
  rtFree    <- extern "free" [ptr i8] Ty.void
  rtMemset  <- extern "llvm.memset.p0i8.i32" [ptr i8, i8, i32, i1] Ty.void
  pure Runtime {..}

-- | Emits @getchar@ function call and returns
--   register containing a result.
callGetChar :: Codegen Runtime Operand
callGetChar = do
  getchar <- asks rtGetChar
  call getchar []

-- | Emits @putchar@ function call with register
--   containing character to put.
callPutChar :: Operand -> Codegen Runtime ()
callPutChar ch = do
  putchar <- asks rtPutChar
  void $ call putchar [(ch, [])]

-- | Allocates @i32@ array on heap, initializing it to zeros.
allocateOnHeap :: Integer -> Codegen Runtime Operand
allocateOnHeap size = do
  let size' = size * 4
  basePtr <- callMalloc size'
  callMemset basePtr 0 size'
  bitcast basePtr (ptr i32)

-- | Allocates @i32@ variable on stack, initializing with
--   provided default.
allocateOnStack :: Integer -> Codegen Runtime Operand
allocateOnStack initial = do
  valPtr <- alloca i32 (Just (int32 1)) 0
  store valPtr 0 (int32 initial)
  pure valPtr

-- | Emits @malloc@ function call.
callMalloc
  :: Integer
  -- ^ Size of allocated block
  -> Codegen Runtime Operand
  -- ^ @Codegen@ with resulting @ptr i8@
callMalloc size = do
  malloc <- asks rtMalloc
  call malloc [(int32 size, [])]

-- | Emits @free@ function call on provided @i8@ pointer.
callFree :: Operand -> Codegen Runtime ()
callFree p = do
  free <- asks rtFree
  void $ call free [(p, [])]

-- | Emits @memset@ function call.
callMemset
  :: Operand
  -- ^ Base @i8@ pointer
  -> Integer
  -- ^ Value to be set
  -> Integer
  -- ^ Region size (in bytes).
  -> Codegen Runtime ()
callMemset dest val size = do
  memset <- asks rtMemset
  void $ call memset [(dest, []), (int8 val, []), (int32 size, []), (bit 0, [])]
