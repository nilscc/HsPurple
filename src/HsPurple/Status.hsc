-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module HsPurple.Status
    (
    -- * Status types
      StatusPrimitive (unStatusPrimitive)
    , statusUnset
    , statusOffline
    , statusAvailable
    , statusUnavailable
    , statusInvisible
    , statusAway
    , statusExtended_away
    , statusMobile
    , statusTune
    , statusNumPrimitives
    ) where

import Foreign
-- import Foreign.C

--------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

newtype StatusPrimitive = StatusPrimitive
    { unStatusPrimitive :: Word32
    } deriving (Show, Eq, Num, Bits)

#{enum StatusPrimitive, StatusPrimitive
  , statusUnset = 0
  , statusOffline = 1
  , statusAvailable = 2
  , statusUnavailable = 3
  , statusInvisible = 4
  , statusAway = 5
  , statusExtended_away = 6
  , statusMobile = 7
  , statusTune = 8
  , statusNumPrimitives = 9
  }
