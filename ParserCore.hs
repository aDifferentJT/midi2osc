{-# LANGUAGE DeriveAnyClass #-}

module ParserCore
  ( integer
  , sign
  , natural
  , decimal
  , hexadecimal
  , octal
  , sep
  , midiButton
  , midiFader
  , midiControl
  , parseString
  , parseFile
  ) where

import MidiCore

import Control.Exception (Exception, throw, throwIO)
import Control.Monad (void)
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec

newtype ParseException = ParseException ParseError
  deriving (Show, Exception)

integer :: Parser Integer
integer = sign <*> natural

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate)
   <|> (char '+' >> return id)
   <|> return id

natural :: Parser Integer
natural = zeroNumber <|> decimal

zeroNumber :: Parser Integer
zeroNumber = do
  _ <- char '0'
  hexadecimal <|> octal <|> decimal <|> return 0

decimal :: Parser Integer
decimal = number 10 digit

hexadecimal :: Parser Integer
hexadecimal = do
  _ <- oneOf "xX"
  number 16 hexDigit

octal :: Parser Integer
octal = do
  _ <- oneOf "oO"
  number 8 octDigit

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

sep :: Parser ()
sep = void . char $ ':'

midiButton :: Parser MidiControl
midiButton = do
  _ <- char 'B'
  MidiButton . MidiId . fromIntegral <$> natural

midiFader :: Parser MidiControl
midiFader = do
  _ <- char 'F'
  MidiFader . MidiId . fromIntegral <$> natural

midiControl :: Parser MidiControl
midiControl = try midiButton <|> try midiFader

parseString :: Parser a -> String -> a
parseString p' str =
  case parse p "" str of
    Left e  -> throw $ ParseException e
    Right r -> r
  where p = p' >>= (eof >>) . return

parseFile :: Parser a -> String -> IO a
parseFile p file = do
  program  <- readFile file
  case parse p "" program of
    Left e  -> throwIO $ ParseException e
    Right r -> return r
