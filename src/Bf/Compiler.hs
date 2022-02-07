module Bf.Compiler
  ( runCmds
  ) where

import Bf.Compiler.Emit
import Bf.Compiler.Module
import Bf.Opts
import Bf.Syntax

import Foreign.Ptr
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.IRBuilder
import LLVM.Module
import LLVM.PassManager

-- | Wrapper function that turns function pointer to runnable Haskell function.
foreign import ccall "dynamic" toHaskFun :: FunPtr (IO ()) -> IO ()

-- | Runs a Brainfuck program using JIT compiler.
runCmds :: [Cmd] -> Opts -> IO ()
runCmds cmds opts = liftIO $
  withContext $ \ctx -> do
    let
      passSet = defaultCuratedPassSetSpec
        { optLevel = optsOptLevel opts
        }
    withJIT ctx opts $ \jit -> do
      let
        modName = "bf"
        fnName  = "wrapper"
        ast     = cmds
                & buildModule modName
                . createModule
                . createFunction fnName
                . compileCmds (optsMemSize opts)
      withModuleFromAST ctx ast $ \compiledMod ->
        withPassManager passSet $ \pm -> do
          _ <- runPassManager pm compiledMod
          when (optsPrintIR opts) $ do
            bs <- moduleLLVMAssembly compiledMod
            putBS bs
          unless (optsPrintIR opts) $
            EE.withModuleInEngine jit compiledMod $ \ee ->
              maybe (fail $ "function not found: " ++ show fnName)
                    (toHaskFun . castFunPtr) =<< EE.getFunction ee fnName

-- | Creates an MC JIT instance within LLVM context.
withJIT :: Context -> Opts -> (EE.MCJIT -> IO a) -> IO a
withJIT ctx opts = EE.withMCJIT ctx optlevel model ptrelim fastins
  where
    optlevel  = optsOptLevel opts
    model     = Nothing
    ptrelim   = Nothing
    fastins   = Nothing
