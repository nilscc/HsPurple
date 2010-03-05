{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.SavedStatuses
    (
    -- * SavedStatuses Subsystem
      initSavedStatuses
    , uninitSavedStatuses
    , savedstatusGetHandle

    -- * SavedStatuses API
    , savedstatusNew
    , savedstatusSetTitle
    , savedstatusSetType
    , savedstatusSetMessage
    , savedstatusSetSubstatus
    , savedstatusUnsetSubstatus
    , savedstatusDelete
    , savedstatusDeleteByStatus
    , savedstatusGetAll
    , savedstatusGetPopular
    , savedstatusGetCurrent
    , savedstatusGetDefault
    , savedstatusGetIdleaway
    , savedstatusIsIdleaway
    , savedstatusGetStartup
    , savedstatusFind
    , savedstatusFindByCreationTime
    , savedstatusFindTransientByTypeAndMessage
    , savedstatusIsTransient
    , savedstatusGetTitle
    , savedstatusGetType
    , savedstatusGetMessage
    , savedstatusGetCreationTime
    , savedstatusHasSubstatuses
    , savedstatusGetSubstatus
    , savedstatusSubstatusGetType
    , savedstatusSubstatusGetMessage
    , savedstatusActivate
    , savedstatusActivateForAccount
    ) where

import Foreign
import Foreign.C
import Network.HsPurple.GLib.GList

import Data.Time
import Data.Time.Clock.POSIX

#include <purple.h>
#include <bindings.dsl.h>


type SavedStatus    = Ptr ()
type SavedStatusSub = Ptr ()
type StatusType     = Ptr ()
type Account        = Ptr ()
type UIHandle       = Ptr ()
type StatusPrimitive = Int


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


--------------------------------------------------------------------------------
-- SavedStatus subsystem
--------------------------------------------------------------------------------

#ccall purple_savedstatuses_init , IO ()
-- | Initializes the status subsystem. 
initSavedStatuses :: IO ()
initSavedStatuses = c'purple_savedstatuses_init

#ccall purple_savedstatuses_uninit , IO ()
-- | Uninitializes the status subsystem. 
uninitSavedStatuses :: IO ()
uninitSavedStatuses = c'purple_savedstatuses_uninit

#ccall purple_savedstatuses_get_handle , IO UIHandle
-- | Get the handle for the status subsystem. 
savedstatusGetHandle :: IO UIHandle
savedstatusGetHandle = c'purple_savedstatuses_get_handle



--------------------------------------------------------------------------------
-- SavedStatus API
--------------------------------------------------------------------------------

#ccall purple_savedstatus_new , CString -> CInt -> IO SavedStatus
-- | Create a new saved status. 
savedstatusNew :: String        -- ^ Title
               -> StatusPrimitive
               -> IO SavedStatus
savedstatusNew s p = newCString s >>= flip c'purple_savedstatus_new (fi p)

#ccall purple_savedstatus_set_title , SavedStatus -> CString -> IO ()
-- | Set the title for the given saved status. 
savedstatusSetTitle :: SavedStatus
                    -> String       -- ^ Title
                    -> IO ()
savedstatusSetTitle ss s = newCString s >>= c'purple_savedstatus_set_title ss

#ccall purple_savedstatus_set_type , SavedStatus -> CInt -> IO ()
-- | Set the type for the given saved status. 
savedstatusSetType :: SavedStatus -> StatusPrimitive -> IO ()
savedstatusSetType ss sp = c'purple_savedstatus_set_type ss (fi sp)

#ccall purple_savedstatus_set_message , SavedStatus -> CString -> IO ()
-- | Set the message for the given saved status. 
savedstatusSetMessage :: SavedStatus -> String -> IO ()
savedstatusSetMessage ss s = newCString s >>= c'purple_savedstatus_set_message ss

#ccall purple_savedstatus_set_substatus , SavedStatus -> Account -> StatusType -> CString -> IO ()
-- | Set a substatus for an account in a saved status. 
savedstatusSetSubstatus :: SavedStatus
                        -> Account
                        -> StatusType
                        -> String
                        -> IO ()
savedstatusSetSubstatus ss a t s = newCString s >>= c'purple_savedstatus_set_substatus ss a t

#ccall purple_savedstatus_unset_substatus , SavedStatus -> Account -> IO ()
-- | Unset a substatus for an account in a saved status. 
savedstatusUnsetSubstatus :: SavedStatus -> Account -> IO ()
savedstatusUnsetSubstatus = c'purple_savedstatus_unset_substatus

#ccall purple_savedstatus_delete , CString -> IO CInt
-- | Delete a saved status. 
savedstatusDelete :: String     -- ^ Title
                  -> IO Bool
savedstatusDelete s = newCString s >>= fmap (1 ==) . c'purple_savedstatus_delete

#ccall purple_savedstatus_delete_by_status , SavedStatus -> IO ()
-- | Delete a saved status. 
savedstatusDeleteByStatus :: SavedStatus -> IO ()
savedstatusDeleteByStatus = c'purple_savedstatus_delete_by_status

#ccall purple_savedstatuses_get_all , IO (Ptr <GList>)
-- | Returns all saved statuses. 
savedstatusGetAll :: IO [SavedStatus]
savedstatusGetAll = c'purple_savedstatuses_get_all >>= fmap (map castPtr) . gListToList

#ccall purple_savedstatuses_get_popular , CUInt -> IO (Ptr <GList>)
-- | Returns the n most popular saved statuses. 
savedstatusGetPopular :: Int    -- ^ how many
                      -> IO [SavedStatus]
savedstatusGetPopular i
    | i < 0 = return []
    | otherwise = c'purple_savedstatuses_get_popular (fi i) >>= fmap (map castPtr) . gListToList

#ccall purple_savedstatus_get_current , IO SavedStatus
-- | Returns the currently selected saved status. 
savedstatusGetCurrent :: IO SavedStatus
savedstatusGetCurrent = c'purple_savedstatus_get_current

#ccall purple_savedstatus_get_default , IO SavedStatus
-- | Returns the default saved status that is used when our accounts are not idle-away. 
savedstatusGetDefault :: IO SavedStatus
savedstatusGetDefault = c'purple_savedstatus_get_default

#ccall purple_savedstatus_get_idleaway , IO SavedStatus
-- | Returns the saved status that is used when your accounts become idle-away. 
savedstatusGetIdleaway :: IO SavedStatus
savedstatusGetIdleaway = c'purple_savedstatus_get_idleaway

#ccall purple_savedstatus_is_idleaway , IO CInt
-- | Return TRUE if we are currently idle-away. 
savedstatusIsIdleaway :: IO Bool
savedstatusIsIdleaway = fmap (1 ==) c'purple_savedstatus_is_idleaway

#ccall purple_savedstatus_set_idleaway , CInt -> IO ()
-- | Set whether accounts in Purple are idle-away or not. 
savedstatusSetIdleaway :: Bool -> IO ()
savedstatusSetIdleaway b = c'purple_savedstatus_set_idleaway (if b then 1 else 0)

#ccall purple_savedstatus_get_startup , IO SavedStatus
-- | Returns the status to be used when purple is starting up. 
savedstatusGetStartup :: IO SavedStatus
savedstatusGetStartup = c'purple_savedstatus_get_startup

#ccall purple_savedstatus_find , CString -> IO SavedStatus
-- | Finds a saved status with the specified title. 
savedstatusFind :: String -> IO SavedStatus
savedstatusFind s = newCString s >>= c'purple_savedstatus_find

#ccall purple_savedstatus_find_by_creation_time , CTime -> IO SavedStatus
-- | Finds a saved status with the specified creation time. 
savedstatusFindByCreationTime :: UTCTime -> IO SavedStatus
savedstatusFindByCreationTime utctime = c'purple_savedstatus_find_by_creation_time time
  where time = fromIntegral . (round :: (RealFrac a) => a -> Int) $ utcTimeToPOSIXSeconds utctime

#ccall purple_savedstatus_find_transient_by_type_and_message , CInt -> CString -> IO SavedStatus
-- | Finds a saved status with the specified primitive and message. 
savedstatusFindTransientByTypeAndMessage :: StatusPrimitive
                                         -> String
                                         -> IO SavedStatus
savedstatusFindTransientByTypeAndMessage t s = newCString s >>= c'purple_savedstatus_find_transient_by_type_and_message (fi t)

#ccall purple_savedstatus_is_transient , SavedStatus -> IO CInt
-- | Determines if a given saved status is "transient." A transient saved status is one that was not explicitly added by the user. 
savedstatusIsTransient :: SavedStatus -> IO Bool
savedstatusIsTransient = fmap (1 ==) . c'purple_savedstatus_is_transient

#ccall purple_savedstatus_get_title , SavedStatus -> IO CString
-- | Return the name of a given saved status. 
savedstatusGetTitle :: SavedStatus -> IO String
savedstatusGetTitle ss = c'purple_savedstatus_get_title ss >>= peekCString

#ccall purple_savedstatus_get_type , SavedStatus -> IO CInt
-- | Return the type of a given saved status. 
savedstatusGetType :: SavedStatus -> IO StatusPrimitive
savedstatusGetType = fmap fi . c'purple_savedstatus_get_type

#ccall purple_savedstatus_get_message , SavedStatus -> IO CString
-- | Return the default message of a given saved status. 
savedstatusGetMessage :: SavedStatus -> IO String
savedstatusGetMessage ss = c'purple_savedstatus_get_message ss >>= peekCString

#ccall purple_savedstatus_get_creation_time , SavedStatus -> IO CTime
-- | Return the time in seconds-since-the-epoch when this saved status was created. 
savedstatusGetCreationTime :: SavedStatus -> IO UTCTime
savedstatusGetCreationTime = fmap toUtc . c'purple_savedstatus_get_creation_time
  where toUtc = posixSecondsToUTCTime . realToFrac

#ccall purple_savedstatus_has_substatuses , SavedStatus -> IO CInt
-- | Determine if a given saved status has "substatuses," or if it is a simple status (the same for all accounts). 
savedstatusHasSubstatuses :: SavedStatus -> IO Bool
savedstatusHasSubstatuses = fmap (1 ==) . c'purple_savedstatus_has_substatuses

#ccall purple_savedstatus_get_substatus , SavedStatus -> Account -> IO SavedStatusSub
-- | Get the substatus for an account in a saved status. 
savedstatusGetSubstatus :: SavedStatus -> Account -> IO SavedStatusSub
savedstatusGetSubstatus = c'purple_savedstatus_get_substatus

#ccall purple_savedstatus_substatus_get_type , SavedStatusSub -> IO StatusType
-- | Get the status type of a given substatus. 
savedstatusSubstatusGetType :: SavedStatusSub -> IO StatusType
savedstatusSubstatusGetType = c'purple_savedstatus_substatus_get_type

#ccall purple_savedstatus_substatus_get_message , SavedStatusSub -> IO CString
-- | Get the message of a given substatus. 
savedstatusSubstatusGetMessage :: SavedStatusSub -> IO String
savedstatusSubstatusGetMessage ss = c'purple_savedstatus_substatus_get_message ss >>= peekCString

#ccall purple_savedstatus_activate , SavedStatus -> IO ()
-- | Sets the statuses for all your accounts to those specified by the given saved_status. 
savedstatusActivate :: SavedStatus -> IO ()
savedstatusActivate = c'purple_savedstatus_activate

#ccall purple_savedstatus_activate_for_account , SavedStatus -> Account -> IO ()
-- | Sets the statuses for a given account to those specified by the given saved_status. 
savedstatusActivateForAccount :: SavedStatus -> Account -> IO ()
savedstatusActivateForAccount = c'purple_savedstatus_activate_for_account
