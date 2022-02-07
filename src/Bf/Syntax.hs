module Bf.Syntax
  ( Cmd (..)
  , parseProgram
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

-- | Brainfuck command data type.
data Cmd
  -- | Increases data pointer by one.
  = IncPtr
  -- | Decreases data pointer by one.
  | DecPtr
  -- | Increases data by one.
  | IncDat
  -- | Decreases data by one.
  | DecDat
  -- | Gets a character from standard input.
  | ChrGet
  -- | Puts a character to standard output.
  | ChrPut
  -- | Loops until data is not equal to zero.
  | Loop [Cmd]
  deriving stock (Eq, Show)

-- | Parses a text to program, or returns error message.
parseProgram :: Text -> Either String [Cmd]
parseProgram = P.parseOnly cmdBlock

cmdBlock :: Parser [Cmd]
cmdBlock = comments *> (simpleCmd <|> loopCmd) `P.sepBy` comments <* comments
  where comments = P.takeWhile (`notElem` cmdChrs)

simpleCmd :: Parser Cmd
simpleCmd = toCmd <$> P.satisfy (`elem` simpleCmdChrs)
  where
    toCmd = \case
      '>' -> IncPtr
      '<' -> DecPtr
      '+' -> IncDat
      '-' -> DecDat
      ',' -> ChrGet
      '.' -> ChrPut
      _   -> error "impossible"

loopCmd :: Parser Cmd
loopCmd = Loop <$> (P.char '[' *> cmdBlock <* P.char ']')

cmdChrs :: [Char]
cmdChrs = simpleCmdChrs ++ loopChrs

simpleCmdChrs :: [Char]
simpleCmdChrs = ['>', '<', '+', '-', ',', '.']

loopChrs :: [Char]
loopChrs = ['[', ']']
