-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Structs.Status
    (
    ) where

import Foreign
import Foreign.C
import Foreign.Ptr

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

data StatusPrimitive
    = StatusUnset
    | StatusOffline
    | StatusAvailable
    | StatusUnavailable
    | StatusInvisible
    | StatusAway
    | StatusExtendedAway
    | StatusMobile
    | StatusTune
    | StatusNumPrimitives

hStatusPrimitive :: StatusPrimitive -> CInt
hStatusPrimitive StatusUnset = 0
hStatusPrimitive StatusOfflin = 1
hStatusPrimitive StatusAvailable = 2
hStatusPrimitive StatusUnavailable = 3
hStatusPrimitive StatusInvisible = 4
hStatusPrimitive StatusAway = 5
hStatusPrimitive StatusExtendedAway = 6
hStatusPrimitive StatusMobile = 7
hStatusPrimitive StatusTune = 8
hStatusPrimitive StatusNumPrimitives = 9

cStatusPrimitive :: CInt -> StatusPrimitive
cStatusPrimitive 0 = StatusUnset
cStatusPrimitive 1 = StatusOfflin
cStatusPrimitive 2 = StatusAvailable
cStatusPrimitive 3 = StatusUnavailable
cStatusPrimitive 4 = StatusInvisible
cStatusPrimitive 5 = StatusAway
cStatusPrimitive 6 = StatusExtendedAway
cStatusPrimitive 7 = StatusMobile
cStatusPrimitive 8 = StatusTune
cStatusPrimitive 9 = StatusNumPrimitives

data StatusType = StatusType
    { statusPrimitive       :: StatusPrimitive
    , statusId              :: String
    , statusName            :: String
    , statusPrimaryAttrId   :: String
    , statusSaveable        :: Bool
    , statusUserSettable    :: Bool
    , statusIndependent     :: Bool
    , statusAttrs           :: [StatusAttr]
    }

instance Storable StatusType where
    sizeOf _    = #size    PurpleStatusType
    alignment _ = #alignof PurpleStatusType
    peek ptr    = do

        prim    <- (#peek PurpleStatusType, primitive) ptr
        id      <- (#peek PurpleStatusType, id) ptr                 >>= peekCString
        name    <- (#peek PurpleStatusType, name) ptr               >>= peekCString
        prim_i  <- (#peek PurpleStatusType, primary_attr_id) ptr    >>= peekCString

        sav     <- (#peek PurpleStatusType, saveable) ptr
        set     <- (#peek PurpleStatusType, user_settable) ptr
        ind     <- (#peek PurpleStatusType, independent) ptr

        lis     <- (#peek PurpleStatusType, attrs) ptr

        return $ StatusType (cStatusPrimitive prim)
                            id
                            name
                            prim_i
                            (sav == 1)
                            (set == 1)
                            (ind == 1)
                            lis

data StatusAttr = StatusAttr
    { statusAttrId          :: String
    , statusAttrName        :: String
    , statusAttrValue       :: ValueType
    }
