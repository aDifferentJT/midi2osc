{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module OutputCore (Output (Print, OSC, BankSwitch)) where

import Data.Serialize (Serialize)
import GHC.Generics

data Output = Print String | OSC String | BankSwitch Int
  deriving (Show, Read, Generic)

instance Serialize Output

