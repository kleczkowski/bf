{-# LANGUAGE RecursiveDo #-}
module Bf.Compiler.Combinators
  ( ld
  , st
  , upd
  , whileLoop
  , isNonZero
  ) where

import Bf.Compiler.Monad

import LLVM.AST.Operand
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Monad
import LLVM.AST.IntegerPredicate
import LLVM.IRBuilder.Constant

-- | Simply loads value under pointer.
ld :: Operand -> Codegen r Operand
ld op = load op 0
{-# INLINE ld #-}

-- | Simply stores value.
st :: Operand -> Operand -> Codegen r ()
st dest = store dest 0
{-# INLINE st #-}

-- | Emits a code that loads value, passes it to other emitting
--   function and stores the result of emitted code.
upd :: (Operand -> Codegen r Operand) -> Operand -> Codegen r ()
upd f addr = ld addr >>= f >>= st addr
{-# INLINE upd #-}

-- | Emits a while loop with provided condition and loop body.
whileLoop :: Codegen r Operand -> Codegen r () -> Codegen r ()
whileLoop cond cmds = mdo
  br begin
  begin <- block `named` "while.begin"
  r <- cond
  condBr r body end
  body <- block `named` "while.body"
  cmds
  br begin
  end <- block  `named` "while.end"
  pass

-- | Checkes if operand is non-zero integer.
isNonZero :: Operand -> Codegen r Operand
isNonZero op = icmp NE op (int32 0)
{-# INLINE isNonZero #-}
