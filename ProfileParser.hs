{-# LANGUAGE TupleSections #-}

module ProfileParser
  ( Control (Control)
  , Profile
  , FeedbackProfile
  , openProfile
  ) where

import MidiCore
import ParserCore

import Data.Bimap (Bimap)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Text.ParserCombinators.Parsec

type Profile = Bimap MidiControl Control
type FeedbackProfile = Map Control Control

profileLine :: Parser (MidiControl, Control, Maybe Control)
profileLine = do
  control <- midiControl
  sep
  name <- many1 (noneOf ['\n',':'])
  fbName <- (sep >> Just <$> many1 (noneOf ['\n'])) <|> return Nothing
  return (control, Control name, Control <$> fbName)

profile :: Parser (Profile, FeedbackProfile)
profile = do
  xs <- sepEndBy profileLine newline
  return (fromList . map (\(x,y,_) -> (x,y)) $ xs, fromList . mapMaybe (\(_,x,y) -> (x,) <$> y) $ xs)

openProfile :: FilePath -> IO (Profile, FeedbackProfile)
openProfile = parseFile profile

