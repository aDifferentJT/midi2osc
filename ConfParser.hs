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

data Stmt = OSCAddress (String, String, Int)
          | Profile FilePath
          | MidiDevice String
          | Bank FilePath
          | BankLeft String
          | BankRight String
          | ChannelGroup String [String]
          | ActionGroup String [String]
  deriving Show

makeStmtParser :: String -> Parser a -> (a -> Stmt) -> Parser Stmt
makeStmtParser t argParse f = do
  _ <- string t
  sep
  f <$> argParse

oscAddressStmt :: Parser Stmt
oscAddressStmt = makeStmtParser
  "a"
  (do
    l <- many1 (noneOf [':','\n'])
    sep
    a <- many1 (alphaNum <|> char '.')
    _ <- char ':'
    p <- fromInteger <$> natural
    return (l, a, p)
    )
  OSCAddress

profileStmt :: Parser Stmt
profileStmt = makeStmtParser
  "p"
  (many1 (noneOf ['\n']))
  Profile

midiDeviceStmt :: Parser Stmt
midiDeviceStmt = makeStmtParser
  "d"
  (many1 (noneOf ['\n']))
  MidiDevice

bankStmt :: Parser Stmt
bankStmt = makeStmtParser
  "b"
  (many1 (noneOf ['\n']))
  Bank

bankLeftStmt :: Parser Stmt
bankLeftStmt = makeStmtParser
  "bl"
  (many1 alphaNum)
  BankLeft

bankRightStmt :: Parser Stmt
bankRightStmt = makeStmtParser
  "br"
  (many1 alphaNum)
  BankRight

groupArg :: Parser (String, [String])
groupArg = do
  name <- many1 alphaNum
  sep
  controls <- sepBy1 (many1 alphaNum) (char ',')
  return (name, controls)

channelGroupStmt :: Parser Stmt
channelGroupStmt = makeStmtParser
  "cg"
  groupArg
  (uncurry ChannelGroup)

actionGroupStmt :: Parser Stmt
actionGroupStmt = makeStmtParser
  "ag"
  groupArg
  (uncurry ActionGroup)

statement :: Parser Stmt
statement = try oscAddressStmt
        <|> try profileStmt
        <|> try midiDeviceStmt
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
  { confOSCAddresses :: [(Connection, (String, Int))]
  , confProfile :: FilePath
  , confMidiDevice :: String
  , confBanks :: [FilePath]
  , confBankLefts :: [Control]
  , confBankRights :: [Control]
  , confChannelGroups :: [(Channel, [Control])]
  , confActionGroups :: [(Action, [Control])]
  }
  deriving Show

processStmt :: Stmt -> Conf -> Conf
processStmt (OSCAddress (l,a,p)) conf = conf { confOSCAddresses = (Connection l, (a,p)) : confOSCAddresses conf }
processStmt (Profile fn)         conf = conf { confProfile = fn }
processStmt (MidiDevice d)       conf = conf { confMidiDevice = d }
processStmt (Bank fn)            conf = conf { confBanks = fn : confBanks conf }
processStmt (BankLeft  x)        conf = conf { confBankLefts  = Control x : confBankLefts  conf }
processStmt (BankRight x)        conf = conf { confBankRights = Control x : confBankRights conf }
processStmt (ChannelGroup n cs)  conf = conf { confChannelGroups = (Channel n, map Control cs) : confChannelGroups conf }
processStmt (ActionGroup  n cs) conf = conf { confActionGroups  = (Action  n, map Control cs) : confActionGroups  conf }

emptyConf :: Conf
emptyConf = Conf [] "" "" [] [] [] [] []

confParser :: Parser Conf
confParser = foldr processStmt emptyConf <$> stmtsParser

openConf :: FilePath -> IO Conf
openConf = parseFile confParser

