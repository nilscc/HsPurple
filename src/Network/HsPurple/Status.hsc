-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Status
    (

    -- * Status API
      statusGetType

    -- * StatusType API
    , statusTypeGetPrimitive

    -- * StatusPrimitive API
    , primitiveGetIdFromType
    , primitiveGetNameFromType
    , primitiveGetTypeFromId

    -- * Enum: Status types
    , StatusPrimitive
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
import Foreign.C

#include <purple.h>
#include <bindings.dsl.h>



--------------------------------------------------------------------------------
-- Status API
--------------------------------------------------------------------------------

type Status = Ptr()

#ccall purple_status_get_type                       , Ptr () -> IO (Ptr ())
-- | Returns the status's type.
statusGetType :: Status -> IO StatusType
statusGetType = c'purple_status_get_type



--------------------------------------------------------------------------------
-- StatusType API
--------------------------------------------------------------------------------

type StatusType = Ptr ()

#ccall purple_status_type_get_primitive             , StatusType -> IO CInt
-- | Returns the primitive type of a status type.
statusTypeGetPrimitive :: StatusType -> IO StatusPrimitive
statusTypeGetPrimitive = fmap fi . c'purple_status_type_get_primitive



--------------------------------------------------------------------------------
-- StatusPrimitive API
--------------------------------------------------------------------------------

#ccall purple_primitive_get_id_from_type            , CInt -> IO CString
-- | Lookup the id of a primitive status type based on the type. 
primitiveGetIdFromType :: StatusPrimitive -> IO String
primitiveGetIdFromType t = c'purple_primitive_get_id_from_type (fi t) >>= peekCString

#ccall purple_primitive_get_name_from_type          , CInt -> IO CString
-- | Lookup the name of a primitive status type based on the type. 
primitiveGetNameFromType :: StatusPrimitive -> IO String
primitiveGetNameFromType t = c'purple_primitive_get_name_from_type (fi t) >>= peekCString

#ccall purple_primitive_get_type_from_id            , CString -> IO CInt
-- | Lookup the value of a primitive status type based on the id.
primitiveGetTypeFromId :: String -> IO StatusPrimitive
primitiveGetTypeFromId s = newCString s >>= fmap fi . c'purple_primitive_get_type_from_id



--------------------------------------------------------------------------------
-- Enums
-------------------------------------------------------------------------------

type StatusPrimitive = Int

#num PURPLE_STATUS_UNSET
#num PURPLE_STATUS_OFFLINE
#num PURPLE_STATUS_AVAILABLE
#num PURPLE_STATUS_UNAVAILABLE
#num PURPLE_STATUS_INVISIBLE
#num PURPLE_STATUS_AWAY
#num PURPLE_STATUS_EXTENDED_AWAY
#num PURPLE_STATUS_MOBILE
#num PURPLE_STATUS_TUNE
-- #num PURPLE_STATUS_MOOD
#num PURPLE_STATUS_NUM_PRIMITIVES

statusUnset         :: StatusPrimitive
statusOffline       :: StatusPrimitive
statusAvailable     :: StatusPrimitive
statusUnavailable   :: StatusPrimitive
statusInvisible     :: StatusPrimitive
statusAway          :: StatusPrimitive
statusExtended_away :: StatusPrimitive
statusMobile        :: StatusPrimitive
statusTune          :: StatusPrimitive
-- statusMood          :: StatusPrimitive
statusNumPrimitives :: StatusPrimitive

statusUnset         = c'PURPLE_STATUS_UNSET
statusOffline       = c'PURPLE_STATUS_OFFLINE
statusAvailable     = c'PURPLE_STATUS_AVAILABLE
statusUnavailable   = c'PURPLE_STATUS_UNAVAILABLE
statusInvisible     = c'PURPLE_STATUS_INVISIBLE
statusAway          = c'PURPLE_STATUS_AWAY
statusExtended_away = c'PURPLE_STATUS_EXTENDED_AWAY
statusMobile        = c'PURPLE_STATUS_MOBILE
statusTune          = c'PURPLE_STATUS_TUNE
-- statusMood          = c'PURPLE_STATUS_MOOD
statusNumPrimitives = c'PURPLE_STATUS_NUM_PRIMITIVES

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
