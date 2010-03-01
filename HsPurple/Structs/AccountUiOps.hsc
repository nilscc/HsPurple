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

import HsPurple.Structs.Account
import HsPurple.Structs.Status

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

instance Storable AccountUiOps where
    sizeOf _    = #size    PurpleAccountUiOps
    alignment _ = #alignof PurpleAccountUiOps
    peek ptr    = do

        notify   <- (#peek PurpleAccountUiOps, notify_added) ptr            >>= c_get_notify_added
        status   <- (#peek PurpleAccountUiOps, status_changed) ptr          >>= c_get_status_changed
        req_add  <- (#peek PurpleAccountUiOps, request_add) ptr             >>= c_get_request_add
        req_auth <- (#peek PurpleAccountUiOps, request_authorize) ptr       >>= c_get_request_authorize
        close    <- (#peek PurpleAccountUiOps, close_account_request) ptr   >>= c_get_close_account_request

        return $ AccountUiOps (cNotifyAdded notify)
                              (cStatusChanged status)
                              (cRequestAdd req_add)
                              (cRequestAuthorize req_auth)
                              close

    poke ptr (AccountUiOps notify status req_add req_auth close) = do

        c_mk_notify_added (hNotifyAdded notify)             >>= (#poke PurpleAccountUiOps, notify_added) ptr
        c_mk_status_changed (hStatusChanged status)         >>= (#poke PurpleAccountUiOps, status_changed) ptr
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

type CNotifyAdded = Ptr Account
                 -> CString
                 -> CString
                 -> CString
                 -> CString
                 -> IO ()

cNotifyAdded :: CNotifyAdded -> NotifyAdded
cNotifyAdded f = \ptr cs1 cs2 cs3 cs4 -> do
    acc <- peek ptr
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f acc s1 s2 s3 s4

hNotifyAdded :: NotifyAdded -> CNotifyAdded
hNotifyAdded f = \acc s1 s2 s3 s4 -> alloca $ \ptr -> do
    poke ptr acc
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    f ptr cs1 cs2 cs3 cs4

foreign import ccall "wrapper"
    c_mk_notify_added :: CNotifyAdded -> IO (FunPtr CNotifyAdded)

foreign import ccall "dynamic"
    c_get_notify_added :: FunPtr CNotifyAdded -> IO CNotifyAdded


-- | This account's status changed
type StatusChanged = Account
                  -> Status
                  -> IO ()

type CStatusChanged = Ptr Account
                   -> Ptr Status
                   -> IO ()

cStatusChanged :: CStatusChanged -> StatusChanged
cStatusChanged f = \ptr1 ptr2 -> do
    acc   <- peek ptr1
    state <- peek ptr2
    f acc state

hStatusChanged :: CStatusChanged -> StatusChanged
hStatusChanged f = \acc state ->
    alloca $ \ptr1 ->
        alloca $ \ptr2 -> do
            poke ptr1 acc
            poke ptr2 state
            f ptr1 ptr2

foreign import ccall "wrapper"
    c_mk_status_changed :: CStatusChanged -> IO (FunPtr CStatusChanged)

foreign import ccall "dynamic"
    c_get_status_changed :: FunPtr CStatusChanged -> IO CStatusChanged


-- | Someone we don't have on our list added us; prompt to add them.
type RequestAdd = Account
               -> RemoteUser
               -> ID
               -> Alias
               -> Message
               -> IO ()

type CRequestAdd = Ptr Account
                -> CString
                -> CString
                -> CString
                -> CString
                -> IO ()

cRequestAdd :: CRequestAdd -> RequestAdd
cRequestAdd f = \ptr cs1 cs2 cs3 cs4 -> do
    acc <- peek ptr
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f acc s1 s2 s3 s4

hRequestAdd :: RequestAdd -> CRequestAdd
hRequestAdd f = \acc s1 s2 s3 s4 -> alloca $ \ptr -> do
    poke ptr acc
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    f ptr cs1 cs2 cs3 cs4

foreign import ccall "wrapper"
    c_mk_request_add :: CRequestAdd -> IO (FunPtr CRequestAdd)

foreign import ccall "dynamic"
    c_get_request_add :: FunPtr CRequestAdd -> IO CRequestAdd

{-

	/** Prompt for authorization when someone adds this account to their buddy
	 * list.  To authorize them to see this account's presence, call \a
	 * authorize_cb (\a user_data); otherwise call \a deny_cb (\a user_data);
	 * @return a UI-specific handle, as passed to #close_account_request.
	 */
	void *(*request_authorize)(PurpleAccount *account,
	                           const char *remote_user,
	                           const char *id,
	                           const char *alias,
	                           const char *message,
	                           gboolean on_list,
	                           PurpleAccountRequestAuthorizationCb authorize_cb,
	                           PurpleAccountRequestAuthorizationCb deny_cb,
	                           void *user_data);

-}

type OnList      = Bool
type UserData    = Ptr ()

-- typedef void (*PurpleAccountRequestAuthorizationCb)(void *);
type AccountRequestAuthorizationCb = UIHandle -> IO ()

type AuthorizeCb = UIHandle -> IO ()
type DenyCb      = UIHandle -> IO ()

foreign import ccall "dynamic"
    c_get_account_request_autorization_cb :: FunPtr AccountRequestAuthorizationCb
                                          -> IO AccountRequestAuthorizationCb

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

type CRequestAuthorize = Ptr Account
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
cRequestAuthorize f = \ptr cs1 cs2 cs3 cs4 ci fp1 fp2 ud -> do
    acc <- peek ptr
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    let i = fromIntegral ci
    f1 <- c_get_account_request_autorization_cb fp1
    f2 <- c_get_account_request_autorization_cb fp2
    f acc s1 s2 s3 s4 i f1 f2 ud

hRequestAuthorize :: RequestAuthorize -> CRequestAuthorize
hRequestAuthorize f = \acc s1 s2 s3 s4 i f1 f2 ud -> alloca $ \ptr -> do
    poke ptr acc
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    let ci = fromIntegral i
    fp1 <- c_mk_account_request_autorization_cb f1
    fp2 <- c_mk_account_request_autorization_cb f2
    f ptr cs1 cs2 cs3 cs4 ci fp1 fp2 ud

foreign import ccall "wrapper"
    c_mk_request_authorize :: CRequestAuthorize -> IO (FunPtr CRequestAuthorize)

foreign import ccall "dynamic"
    c_get_request_authorize :: FunPtr CRequestAuthorize -> IO CRequestAuthorize


-- | Close a pending request for authorization. @UIHandle@ is a handle as
-- returned by "RequestAuthorize".
type CloseAccountRequest = UIHandle -> IO ()

type UIHandle = Ptr ()

foreign import ccall "wrapper"
    c_mk_close_account_request :: CloseAccountRequest -> IO (FunPtr CloseAccountRequest)

foreign import ccall "dynamic"
    c_get_close_account_request :: FunPtr CloseAccountRequest -> IO CloseAccountRequest
