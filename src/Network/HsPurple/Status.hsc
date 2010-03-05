{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Status
    (

    -- * Status API
      statusNew
    , statusDestroy
    , statusSetActive
    , statusSetActiveWithAttrsList
    , statusSetAttrBoolean
    , statusGetType
    , statusGetPresence
    , statusGetId
    , statusGetName
    , statusIsIndependent
    , statusIsExclusive
    , statusIsAvailable
    , statusIsActive
    , statusIsOnline
    , statusGetAttrValue
    , statusGetAttrBoolean
    , statusCompare

    -- * StatusType API
    , statusTypeNewFull
    , statusTypeNew
    , statusTypeNewWithAttrs
    , statusTypeDestroy
    , statusTypeAddAttr
    , statusTypeAddAttrs
    , statusTypeGetPrimitive
    , statusTypeGetId
    , statusTypeGetName
    , statusTypeIsSaveable
    , statusTypeIsUserSettable
    , statusTypeIsIndependent
    , statusTypeIsExclusive
    , statusTypeIsAvailable
    , statusTypeGetPrimaryAttr
    , statusTypeGetAttr
    , statusTypeGetAttrs
    , statusTypeFindWithId

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

import Network.HsPurple.GLib.GList

#include <purple.h>
#include <bindings.dsl.h>


type Value = Ptr ()
type StatusAttribute = Ptr ()
type Presence = Ptr ()
type StatusType = Ptr ()
type Status = Ptr()
type StatusPrimitive = Int
type Attribute = Ptr ()

bool :: Bool -> CInt
bool b = if b then 1 else 0


--------------------------------------------------------------------------------
-- Status API
--------------------------------------------------------------------------------

#ccall purple_status_new , StatusType -> Presence -> IO Status
-- | Creates a new status. 
statusNew :: StatusType -> Presence -> IO Status
statusNew = c'purple_status_new

#ccall purple_status_destroy , Status -> IO ()
-- | Destroys a status. 
statusDestroy :: Status -> IO ()
statusDestroy = c'purple_status_destroy

#ccall purple_status_set_active , Status -> CInt -> IO ()
-- | Sets whether or not a status is active. 
statusSetActive :: Status -> Bool -> IO ()
statusSetActive s b = c'purple_status_set_active s (bool b)

{- WTF is a va_list?
void 	purple_status_set_active_with_attrs (PurpleStatus *status, gboolean active, va_list args)
-- | Sets whether or not a status is active. 
statusSetActiveWithAttrs :: Status -> Bool
statusSetActiveWithAttrs (PurpleStatus *status, gboolean active, vaList args)
-}

#ccall purple_status_set_active_with_attrs_list , Status -> CInt -> Ptr <GList> -> IO ()
-- | Sets whether or not a status is active. 
statusSetActiveWithAttrsList :: Status -> Bool -> [Attribute] -> IO ()
statusSetActiveWithAttrsList s b lis = listToGList lis >>= c'purple_status_set_active_with_attrs_list s (bool b)

#ccall purple_status_set_attr_boolean , Status -> CString -> CInt -> IO ()
-- | Sets the boolean value of an attribute in a status with the specified ID. 
statusSetAttrBoolean :: Status
                     -> String  -- ^ Id
                     -> Bool
                     -> IO ()
statusSetAttrBoolean st s b = do
    cs <- newCString s
    c'purple_status_set_attr_boolean st cs (bool b)

#ccall purple_status_get_type , Status -> IO StatusType
-- | Returns the status's type. 
statusGetType :: Status -> IO StatusType
statusGetType = c'purple_status_get_type

#ccall purple_status_get_presence , Status -> IO Presence
-- | Returns the status's presence. 
statusGetPresence :: Status -> IO Presence
statusGetPresence = c'purple_status_get_presence

#ccall purple_status_get_id , Status -> IO CString
-- | Returns the status's type ID. 
statusGetId :: Status -> IO String
statusGetId st = c'purple_status_get_id st >>= peekCString

#ccall purple_status_get_name , Status -> IO CString
-- | Returns the status's name. 
statusGetName :: Status -> IO String
statusGetName st = c'purple_status_get_name st >>= peekCString

#ccall purple_status_is_independent , Status -> IO CInt
-- | Returns whether or not a status is independent. 
statusIsIndependent :: Status -> IO Bool
statusIsIndependent = fmap (1 ==) . c'purple_status_is_independent

#ccall purple_status_is_exclusive , Status -> IO CInt
-- | Returns whether or not a status is exclusive. 
statusIsExclusive :: Status -> IO Bool
statusIsExclusive = fmap (1 ==) . c'purple_status_is_exclusive

#ccall purple_status_is_available , Status -> IO CInt
-- | Returns whether or not a status is available. 
statusIsAvailable :: Status -> IO Bool
statusIsAvailable = fmap (1 ==) . c'purple_status_is_available

#ccall purple_status_is_active , Status -> IO CInt
-- | Returns the active state of a status. 
statusIsActive :: Status -> IO Bool
statusIsActive = fmap (1 ==) . c'purple_status_is_active

#ccall purple_status_is_online , Status -> IO CInt
-- | Returns whether or not a status is considered 'online'. 
statusIsOnline :: Status -> IO Bool
statusIsOnline = fmap (1 ==) . c'purple_status_is_online

#ccall purple_status_get_attr_value , Status -> CString -> IO Value
-- | Returns the value of an attribute in a status with the specified ID. 
statusGetAttrValue :: Status
                   -> String -- ^ Id
                   -> IO Value
statusGetAttrValue st s = newCString s >>= c'purple_status_get_attr_value st

#ccall purple_status_get_attr_boolean , Status -> CString -> IO CInt
-- | Returns the boolean value of an attribute in a status with the specified ID. 
statusGetAttrBoolean :: Status
                     -> String -- ^ Id
                     -> IO Bool
statusGetAttrBoolean st s = newCString s >>= fmap (1 ==) . c'purple_status_get_attr_boolean st

#ccall purple_status_compare , Status -> Status -> IO CInt
-- | Compares two statuses for availability. 
statusCompare :: Status -> Status -> IO Int
statusCompare st1 st2 = fromIntegral `fmap` c'purple_status_compare st1 st2


--------------------------------------------------------------------------------
-- StatusType API
--------------------------------------------------------------------------------

#ccall purple_status_type_new_full , CInt -> CString -> CString -> CInt -> CInt -> CInt -> IO StatusType
-- | Creates a new status type. 
statusTypeNewFull :: StatusPrimitive
                  -> String             -- ^ Id
                  -> String             -- ^ Name
                  -> Bool               -- ^ Saveable
                  -> Bool               -- ^ User settable
                  -> Bool               -- ^ Independent
                  -> IO StatusType
statusTypeNewFull p s1 s2 b1 b2 b3 = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c'purple_status_type_new_full (fromIntegral p) cs1 cs2 (bool b1) (bool b2) (bool b3)

#ccall purple_status_type_new , CInt -> CString -> CString -> CInt -> IO StatusType
-- | Creates a new status type with some default values ( saveable and not independent). 
statusTypeNew :: StatusPrimitive
              -> String             -- ^ Id
              -> String             -- ^ Name
              -> Bool               -- ^ User settable
              -> IO StatusType
statusTypeNew p s1 s2 b = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c'purple_status_type_new (fromIntegral p) cs1 cs2 (bool b)

#ccall purple_status_type_new_with_attrs , CInt -> CString -> CString -> CInt -> CInt -> CInt -> CString -> CString -> Value -> IO StatusType
-- | Creates a new status type with attributes. Todo: Add G_GNUC_NULL_TERMINATED arguments
statusTypeNewWithAttrs :: StatusPrimitive
                       -> String        -- ^ Id
                       -> String        -- ^ Name
                       -> Bool          -- ^ Saveable
                       -> Bool          -- ^ User settable
                       -> Bool          -- ^ Independent
                       -> String        -- ^ Attribute Id
                       -> String        -- ^ Attribute name
                       -> Value         -- ^ Attribute value
                       -> IO StatusType
statusTypeNewWithAttrs p s1 s2 b1 b2 b3 s3 s4 v = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    c'purple_status_type_new_with_attrs (fromIntegral p) cs1 cs2 (bool b1) (bool b2) (bool b3) cs3 cs4 v
    
#ccall purple_status_type_destroy , StatusType -> IO ()
-- | Destroys a status type. 
statusTypeDestroy :: StatusType -> IO ()
statusTypeDestroy = c'purple_status_type_destroy

#ccall purple_status_type_set_primary_attr , StatusType -> CString -> IO ()
-- | Sets a status type's primary attribute. 
statusTypeSetPrimaryAttr :: StatusType
                         -> String      -- Attribute Id
                         -> IO ()
statusTypeSetPrimaryAttr t s = newCString s >>= c'purple_status_type_set_primary_attr t

#ccall purple_status_type_add_attr , StatusType -> CString -> CString -> Value -> IO ()
-- | Adds an attribute to a status type. 
statusTypeAddAttr :: StatusType
                  -> String     -- ^ Id
                  -> String     -- ^ Name
                  -> Value
                  -> IO ()
statusTypeAddAttr t s1 s2 v = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c'purple_status_type_add_attr t cs1 cs2 v

#ccall purple_status_type_add_attrs , StatusType -> CString -> CString -> Value -> IO ()
-- | Adds multiple attributes to a status type. 
statusTypeAddAttrs :: StatusType
                   -> String     -- ^ Id
                   -> String     -- ^ Name
                   -> Value
                   -> IO ()
statusTypeAddAttrs t s1 s2 v = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c'purple_status_type_add_attrs t cs1 cs2 v

{- WTF is a va_list?

void 	purple_status_type_add_attrs_vargs (PurpleStatusType *status_type, va_list args)
-- | Adds multiple attributes to a status type using a va_list. 
statusTypeAddAttrsVargs :: StatusType -> 
statusTypeAddAttrsVargs (PurpleStatusType *statusType, vaList args)
-}

#ccall purple_status_type_get_primitive , StatusType -> IO CInt
-- | Returns the primitive type of a status type. 
statusTypeGetPrimitive :: StatusType -> IO StatusPrimitive
statusTypeGetPrimitive = fmap fromIntegral . c'purple_status_type_get_primitive

#ccall purple_status_type_get_id , StatusType -> IO CString
-- | Returns the ID of a status type. 
statusTypeGetId :: StatusType -> IO String
statusTypeGetId t = c'purple_status_type_get_id t >>= peekCString

#ccall purple_status_type_get_name , StatusType -> IO CString
-- | Returns the name of a status type. 
statusTypeGetName :: StatusType -> IO String
statusTypeGetName t = c'purple_status_type_get_name t >>= peekCString

#ccall purple_status_type_is_saveable , StatusType -> IO CInt
-- | Returns whether or not the status type is saveable. 
statusTypeIsSaveable :: StatusType -> IO Bool
statusTypeIsSaveable = fmap (1 ==) . c'purple_status_type_is_saveable

#ccall purple_status_type_is_user_settable , StatusType -> IO CInt
-- | Returns whether or not the status type can be set or modified by the user. 
statusTypeIsUserSettable :: StatusType -> IO Bool
statusTypeIsUserSettable = fmap (1==) . c'purple_status_type_is_user_settable

#ccall purple_status_type_is_independent , StatusType -> IO CInt
-- | Returns whether or not the status type is independent. 
statusTypeIsIndependent :: StatusType -> IO Bool
statusTypeIsIndependent = fmap (1 ==) . c'purple_status_type_is_independent

#ccall purple_status_type_is_exclusive , StatusType -> IO CInt
-- | Returns whether the status type is exclusive. 
statusTypeIsExclusive :: StatusType -> IO Bool
statusTypeIsExclusive = fmap (1 ==) . c'purple_status_type_is_exclusive

#ccall purple_status_type_is_available , StatusType -> IO CInt
-- | Returns whether or not a status type is available. 
statusTypeIsAvailable :: StatusType -> IO Bool
statusTypeIsAvailable = fmap (1 ==) . c'purple_status_type_is_available

#ccall purple_status_type_get_primary_attr , StatusType -> IO CString
-- | Returns a status type's primary attribute ID. 
statusTypeGetPrimaryAttr :: StatusType -> IO String
statusTypeGetPrimaryAttr t = c'purple_status_type_get_primary_attr t >>= peekCString

#ccall purple_status_type_get_attr , StatusType -> CString -> IO StatusAttribute
-- | Returns the attribute with the specified ID. 
statusTypeGetAttr :: StatusType
                  -> String         -- ^ Id
                  -> IO StatusAttribute
statusTypeGetAttr t s = newCString s >>= c'purple_status_type_get_attr t

#ccall purple_status_type_get_attrs , StatusType -> IO (Ptr <GList>)
-- | Returns a list of all attributes in a status type. 
statusTypeGetAttrs :: StatusType -> IO [StatusAttribute]
statusTypeGetAttrs t = c'purple_status_type_get_attrs t >>= fmap (map castPtr) . gListToList

#ccall purple_status_type_find_with_id , Ptr <GList> -> CString -> IO StatusType
-- | Find the PurpleStatusType with the given id.
statusTypeFindWithId :: [StatusType]
                     -> String          -- ^ Id
                     -> IO StatusType
statusTypeFindWithId lis s = do
    gl <- listToGList lis
    cs <- newCString s
    c'purple_status_type_find_with_id gl cs



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
