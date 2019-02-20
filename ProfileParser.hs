module ProfileParser
  ( Control (Control)
  , Profile
  , openProfile
  ) where

import MidiCore
import ParserCore

import Data.Bimap
import Text.ParserCombinators.Parsec

type Profile = Bimap MidiControl Control

profileLine :: Parser (MidiControl, Control)
profileLine = do
  control <- midiControl
  sep
  name <- many1 (noneOf ['\n'])
  return (control, Control name)

profile :: Parser Profile
profile = fromList <$> sepEndBy profileLine newline

openProfile :: FilePath -> IO Profile
openProfile = parseFile profile

