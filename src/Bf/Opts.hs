module Bf.Opts
  ( Opts (..)
  , runWithOpts
  ) where

import Options.Applicative

-- | Options passed to the program.
data Opts = Opts
  { -- | Input file name to interpret
    --   or @Nothing@ if source will be read
    --   from standard input
    optsInput    :: Maybe FilePath
    -- | An optimization level of generated code
  , optsOptLevel :: Maybe Word
    -- | Size of allocated memory for program
  , optsMemSize  :: Integer
    -- | Prints a LLVM IR to standard output
  , optsPrintIR  :: Bool
  }
  deriving stock (Eq, Show)

-- | Runs CLI options parser.
runWithOpts :: (Opts -> IO a) -> IO a
runWithOpts = (execParser optsParserInfo >>=)

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Run Brainfuck source file"
  , header "bf - just-in-time Brainfuck compiler"
  ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> optional inputParser
    <*> optional optLevelParser
    <*> memSize
    <*> printIR
  where
    inputParser = argument str $ mconcat
      [ metavar "FILE"
      , help "Input file path (empty = read from stdin)"
      ]
    optLevelParser = option auto $ mconcat
      [ short 'O'
      , metavar "n"
      , help "Optimization level of compiled code"
      ]
    memSize = option auto $ mconcat
      [ long "size"
      , short 's'
      , metavar "k"
      , help "Size of memory allocated for program"
      ]
    printIR = switch $ mconcat
      [ long "ir"
      , help "Prints a LLVM IR assembly and exits"
      ]
