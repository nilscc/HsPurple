-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Structs.AccountUiOps
    (
      AccountUiOps
    -- * Function types
    , NotifyAdded
    , StatusChanged
    , RequestAdd
    , RequestAuthorize
    , CloseAccountRequest
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

-- | Account UI operations, used to notify the user of status changes and when
-- buddies add this account to their buddy lists.
data AccountUiOps = AccountUiOps
    { notifyAdded           :: NotifyAdded
    , statusChanged         :: StatusChanged
    , requestAdd            :: RequestAdd
    , requestAuthorize      :: RequestAuthorize
    , closeAccountRequest   :: CloseAccountRequest
    }

type Account = Ptr ()
type Status = Ptr ()

instance Storable AccountUiOps where
    sizeOf _    = #size    PurpleAccountUiOps
    alignment _ = #alignof PurpleAccountUiOps
    peek ptr    = do

        notify   <- c_get_notify_added `fmap` (#peek PurpleAccountUiOps, notify_added) ptr
        status   <- c_get_status_changed `fmap` (#peek PurpleAccountUiOps, status_changed) ptr
        req_add  <- c_get_request_add `fmap` (#peek PurpleAccountUiOps, request_add) ptr
        req_auth <- c_get_request_authorize `fmap` (#peek PurpleAccountUiOps, request_authorize) ptr
        close    <- c_get_close_account_request `fmap` (#peek PurpleAccountUiOps, close_account_request) ptr

        return $ AccountUiOps (cNotifyAdded notify)
                              status
                              (cRequestAdd req_add)
                              (cRequestAuthorize req_auth)
                              close

    poke ptr (AccountUiOps notify status req_add req_auth close) = do

        c_mk_notify_added (hNotifyAdded notify)             >>= (#poke PurpleAccountUiOps, notify_added) ptr
        c_mk_status_changed status                          >>= (#poke PurpleAccountUiOps, status_changed) ptr
        c_mk_request_add (hRequestAdd req_add)              >>= (#poke PurpleAccountUiOps, request_add) ptr
        c_mk_request_authorize (hRequestAuthorize req_auth) >>= (#poke PurpleAccountUiOps, request_authorize) ptr
        c_mk_close_account_request close                    >>= (#poke PurpleAccountUiOps, close_account_request) ptr

        (#poke PurpleAccountUiOps, _purple_reserved1) ptr nullPtr
        (#poke PurpleAccountUiOps, _purple_reserved2) ptr nullPtr
        (#poke PurpleAccountUiOps, _purple_reserved3) ptr nullPtr
        (#poke PurpleAccountUiOps, _purple_reserved4) ptr nullPtr

type RemoteUser = String
type ID         = String
type Alias      = String
type Message    = String

-- | A buddy who is already on this account's buddy list added this account to
-- their buddy list.
type NotifyAdded = Account
                -> RemoteUser
                -> ID
                -> Alias
                -> Message
                -> IO ()

type CNotifyAdded = Account
                 -> CString
                 -> CString
                 -> CString
                 -> CString
                 -> IO ()

cNotifyAdded :: CNotifyAdded -> NotifyAdded
cNotifyAdded f = \acc s1 s2 s3 s4 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    f acc cs1 cs2 cs3 cs4

hNotifyAdded :: NotifyAdded -> CNotifyAdded
hNotifyAdded f = \ptr cs1 cs2 cs3 cs4 -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f ptr s1 s2 s3 s4

foreign import ccall "wrapper"
    c_mk_notify_added :: CNotifyAdded -> IO (FunPtr CNotifyAdded)

foreign import ccall "dynamic"
    c_get_notify_added :: FunPtr CNotifyAdded -> CNotifyAdded


-- | This account's status changed
type StatusChanged = Account
                  -> Status
                  -> IO ()

foreign import ccall "wrapper"
    c_mk_status_changed :: StatusChanged -> IO (FunPtr StatusChanged)

foreign import ccall "dynamic"
    c_get_status_changed :: FunPtr StatusChanged -> StatusChanged


-- | Someone we don't have on our list added us; prompt to add them.
type RequestAdd = Account
               -> RemoteUser
               -> ID
               -> Alias
               -> Message
               -> IO ()

type CRequestAdd = Account
                -> CString
                -> CString
                -> CString
                -> CString
                -> IO ()

cRequestAdd :: CRequestAdd -> RequestAdd
cRequestAdd f = \acc s1 s2 s3 s4 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    f acc cs1 cs2 cs3 cs4

hRequestAdd :: RequestAdd -> CRequestAdd
hRequestAdd f = \ptr cs1 cs2 cs3 cs4 -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f ptr s1 s2 s3 s4

foreign import ccall "wrapper"
    c_mk_request_add :: CRequestAdd -> IO (FunPtr CRequestAdd)

foreign import ccall "dynamic"
    c_get_request_add :: FunPtr CRequestAdd -> CRequestAdd

type OnList      = Bool
type UserData    = Ptr ()

-- typedef void (*PurpleAccountRequestAuthorizationCb)(void *);
type AccountRequestAuthorizationCb = UIHandle -> IO ()

type AuthorizeCb = UIHandle -> IO ()
type DenyCb      = UIHandle -> IO ()

foreign import ccall "dynamic"
    c_get_account_request_autorization_cb :: FunPtr AccountRequestAuthorizationCb
                                          -> AccountRequestAuthorizationCb

foreign import ccall "wrapper"
    c_mk_account_request_autorization_cb :: AccountRequestAuthorizationCb
                                         -> IO (FunPtr AccountRequestAuthorizationCb)

-- | Prompt for authorization when someone adds this account to their buddy
-- list. To authorize them to see this account's presence, call a @AuthorizeCb
-- UserData@, otherwise call @DenyCb UserData@, returns a UI-specific handle,
-- as passed to "CloseAccountRequest".
type RequestAuthorize = Account
                     -> RemoteUser
                     -> ID
                     -> Alias
                     -> Message
                     -> OnList
                     -> AuthorizeCb
                     -> DenyCb
                     -> UserData
                     -> IO ()

type CRequestAuthorize = Account
                      -> CString
                      -> CString
                      -> CString
                      -> CString
                      -> CInt
                      -> FunPtr AuthorizeCb
                      -> FunPtr DenyCb
                      -> UserData
                      -> IO ()

cRequestAuthorize :: CRequestAuthorize -> RequestAuthorize
hRequestAuthorize f = \ptr cs1 cs2 cs3 cs4 ci fp1 fp2 ud -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    let i = 1 == ci
        f1 = c_get_account_request_autorization_cb fp1
        f2 = c_get_account_request_autorization_cb fp2
    f ptr s1 s2 s3 s4 i f1 f2 ud

hRequestAuthorize :: RequestAuthorize -> CRequestAuthorize
cRequestAuthorize f = \acc s1 s2 s3 s4 lis f1 f2 ud -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    let ci = if lis then 1 else 0
    fp1 <- c_mk_account_request_autorization_cb f1
    fp2 <- c_mk_account_request_autorization_cb f2
    f acc cs1 cs2 cs3 cs4 ci fp1 fp2 ud

foreign import ccall "wrapper"
    c_mk_request_authorize :: CRequestAuthorize -> IO (FunPtr CRequestAuthorize)

foreign import ccall "dynamic"
    c_get_request_authorize :: FunPtr CRequestAuthorize -> CRequestAuthorize


-- | Close a pending request for authorization. @UIHandle@ is a handle as
-- returned by "RequestAuthorize".
type CloseAccountRequest = UIHandle -> IO ()

type UIHandle = Ptr ()

foreign import ccall "wrapper"
    c_mk_close_account_request :: CloseAccountRequest -> IO (FunPtr CloseAccountRequest)

foreign import ccall "dynamic"
    c_get_close_account_request :: FunPtr CloseAccountRequest -> CloseAccountRequest
