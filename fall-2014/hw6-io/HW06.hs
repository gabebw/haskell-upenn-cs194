{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

----------------------------------------------------
-- Exercise 1: Convert (String "Y") -> (Bool True) and
-- (String "N") -> (Bool False)

ynToBool :: Value -> Value
ynToBool (Bool True) = String "Y"
ynToBool (Bool False) = String "N"
ynToBool v = v

ynToBoolWorks :: Bool
ynToBoolWorks =
    ynToBool (Bool True) == String "Y" &&
    ynToBool (Bool False) == String "N" &&
    ynToBool (Number 3) == Number 3
