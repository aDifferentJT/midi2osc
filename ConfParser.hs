{-# LANGUAGE DeriveGeneric #-}

module ConfParser
  ( Connection (Connection)
  , Channel
  , Action
  , Conf (..)
  , openConf
  ) where

import MidiCore
import ParserCore

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.ParserCombinators.Parsec

data Stmt = Comment String
          | OSCAddress String String Int Int [String]
          | Profile FilePath
          | MidiDevice String
          | PollRate Int
          | Bank FilePath
          | BankLeft String
          | BankRight String
          | ChannelGroup String [String]
          | ActionGroup String [String]
  deriving Show

makeStmtParser :: String -> Parser Stmt -> Parser Stmt
makeStmtParser t argParse = do
  _ <- string t
  sep
  argParse

comment :: Parser Stmt
comment = string "//" >> Comment <$> many1 (noneOf ['\n'])

oscAddressStmt :: Parser Stmt
oscAddressStmt = makeStmtParser
  "a"
  (do
    l <- many1 (noneOf [':','\n'])
    sep
    a <- many1 (alphaNum <|> char '.')
    _ <- char ':'
    pO <- fromInteger <$> natural
    sep
    pF <- fromInteger <$> natural
    regs <- try (sep >> sepBy1 (many1 (noneOf ['\n'])) (char ',')) <|> return []
    return $ OSCAddress l a pO pF regs
    )

profileStmt :: Parser Stmt
profileStmt = makeStmtParser
  "p"
  (Profile <$> many1 (noneOf ['\n']))

midiDeviceStmt :: Parser Stmt
midiDeviceStmt = makeStmtParser
  "d"
  (MidiDevice <$> many1 (noneOf ['\n']))

pollRateStmt :: Parser Stmt
pollRateStmt = makeStmtParser
  "pr"
  (PollRate . fromInteger <$> natural)

bankStmt :: Parser Stmt
bankStmt = makeStmtParser
  "b"
  (Bank <$> many1 (noneOf ['\n']))

bankLeftStmt :: Parser Stmt
bankLeftStmt = makeStmtParser
  "bl"
  (BankLeft <$> many1 alphaNum)

bankRightStmt :: Parser Stmt
bankRightStmt = makeStmtParser
  "br"
  (BankRight <$> many1 alphaNum)

groupArg :: Parser (String, [String])
groupArg = do
  name <- many1 alphaNum
  sep
  controls <- sepBy1 (many1 alphaNum) (char ',')
  return (name, controls)

channelGroupStmt :: Parser Stmt
channelGroupStmt = makeStmtParser
  "cg"
  (uncurry ChannelGroup <$> groupArg)

actionGroupStmt :: Parser Stmt
actionGroupStmt = makeStmtParser
  "ag"
  (uncurry ActionGroup <$> groupArg)

statement :: Parser Stmt
statement = try comment
        <|> try oscAddressStmt
        <|> try profileStmt
        <|> try midiDeviceStmt
        <|> try pollRateStmt
        <|> try bankStmt
        <|> try bankLeftStmt
        <|> try bankRightStmt
        <|> try channelGroupStmt
        <|> try actionGroupStmt

stmtsParser :: Parser [Stmt]
stmtsParser = do
  _ <- many newline
  stmts <- sepEndBy statement (many1 newline)
  eof
  return stmts

newtype Connection = Connection String
  deriving (Eq, Ord, Generic)
instance Serialize Connection
instance Show Connection where
  show (Connection s) = s

newtype Channel = Channel String
  deriving (Eq, Ord, Generic)
instance Serialize Channel
instance Show Channel where
  show (Channel s) = s

newtype Action = Action String
  deriving (Eq, Ord, Generic)
instance Serialize Action
instance Show Action where
  show (Action s) = s

data Conf = Conf
  { confOSCAddresses :: [(Connection, (String, Int, Int, [String]))]
  , confProfile :: FilePath
  , confMidiDevice :: String
  , confPollRate :: Int
  , confBanks :: [FilePath]
  , confBankLefts :: [Control]
  , confBankRights :: [Control]
  , confChannelGroups :: [(Channel, [Control])]
  , confActionGroups :: [(Action, [Control])]
  }
  deriving Show

processStmt :: Stmt -> Conf -> Conf
processStmt (Comment _)                 conf = conf
processStmt (OSCAddress l a pO pF regs) conf = conf { confOSCAddresses = (Connection l, (a,pO,pF,regs)) : confOSCAddresses conf }
processStmt (Profile fn)                conf = conf { confProfile = fn }
processStmt (MidiDevice d)              conf = conf { confMidiDevice = d }
processStmt (PollRate r)                conf = conf { confPollRate = r }
processStmt (Bank fn)                   conf = conf { confBanks = fn : confBanks conf }
processStmt (BankLeft  x)               conf = conf { confBankLefts  = Control x : confBankLefts  conf }
processStmt (BankRight x)               conf = conf { confBankRights = Control x : confBankRights conf }
processStmt (ChannelGroup n cs)         conf = conf { confChannelGroups = (Channel n, map Control cs) : confChannelGroups conf }
processStmt (ActionGroup  n cs)         conf = conf { confActionGroups  = (Action  n, map Control cs) : confActionGroups  conf }

emptyConf :: Conf
emptyConf = Conf [] "" "" 0 [] [] [] [] []

confParser :: Parser Conf
confParser = foldr processStmt emptyConf <$> stmtsParser

openConf :: FilePath -> IO Conf
openConf = parseFile confParser

