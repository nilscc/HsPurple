-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.AccountUiOps where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <bindings.dsl.h>
#include <purple.h>


type UIHandle   = Ptr ()
type Account    = Ptr ()
type Status     = Ptr ()


--------------------------------------------------------------------------------
-- The struct
--------------------------------------------------------------------------------


-- | Account UI operations, used to notify the user of status changes and when
-- buddies add this account to their buddy lists.
data AccountUiOps = AccountUiOps
    { notifyAdded           :: AccountNotifyAdded
    , statusChanged         :: AccountStatusChanged
    , requestAdd            :: AccountRequestAdd
    , requestAuthorize      :: AccountRequestAuthorize
    , closeAccountRequest   :: AccountCloseAccountRequest
    }

instance Storable AccountUiOps where
    sizeOf _    = #size    PurpleAccountUiOps
    alignment _ = #alignof PurpleAccountUiOps
    peek ptr    = mK_AccountUiOps <$> peek (castPtr ptr)
    poke ptr a  = mk_AccountUiOps a >>= poke (castPtr ptr)

mK_AccountUiOps :: C'PurpleAccountUiOps -> AccountUiOps
mK_AccountUiOps (C'PurpleAccountUiOps notify
                                      stat
                                      r_add
                                      r_auth
                                      close
                                      _ _ _ _) =

    AccountUiOps ( mK_NotifyAdded           notify )
                 ( mK_StatusChanged         stat   )
                 ( mK_RequestAdd            r_add  )
                 ( mK_RequestAuthorize      r_auth )
                 ( mK_CloseAccountRequest   close  )

mk_AccountUiOps :: AccountUiOps -> IO C'PurpleAccountUiOps 
mk_AccountUiOps (AccountUiOps notify
                              stat
                              r_add
                              r_auth
                              close) =

    C'PurpleAccountUiOps <$> mk_NotifyAdded           notify
                         <*> mk_StatusChanged         stat
                         <*> mk_RequestAdd            r_add
                         <*> mk_RequestAuthorize      r_auth
                         <*> mk_CloseAccountRequest   close
                         <*> return nullPtr
                         <*> return nullPtr
                         <*> return nullPtr
                         <*> return nullPtr

#starttype PurpleAccountUiOps
#field notify_added             , <NotifyAdded>
#field status_changed           , <StatusChanged>
#field request_add              , <RequestAdd>
#field request_authorize        , <RequestAuthorize>
#field close_account_request    , <CloseAccountRequest>
#field _purple_reserved1        , Ptr ()
#field _purple_reserved2        , Ptr ()
#field _purple_reserved3        , Ptr ()
#field _purple_reserved4        , Ptr ()
#stoptype

--------------------------------------------------------------------------------
-- Function types
--------------------------------------------------------------------------------

#callback NotifyAdded , Account -> CString -> CString -> CString -> CString -> IO ()
-- | A buddy who is already on this account's buddy list added this account to
-- their buddy list.
type AccountNotifyAdded = Account
                -> String       -- ^ RemoteUser
                -> String       -- ^ ID
                -> String       -- ^ Alias
                -> String       -- ^ Message
                -> IO ()


mK_NotifyAdded :: C'NotifyAdded -> AccountNotifyAdded
mK_NotifyAdded f = \acc s1 s2 s3 s4 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    (mK'NotifyAdded f) acc cs1 cs2 cs3 cs4

mk_NotifyAdded :: AccountNotifyAdded -> IO C'NotifyAdded
mk_NotifyAdded f = mk'NotifyAdded $ \ptr cs1 cs2 cs3 cs4 -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f ptr s1 s2 s3 s4


#callback StatusChanged , Account -> Status -> IO ()
-- | This account's status changed
type AccountStatusChanged = Account -> Status -> IO ()

mk_StatusChanged :: AccountStatusChanged -> IO C'StatusChanged
mk_StatusChanged f = mk'StatusChanged $ \acc st -> f acc st

mK_StatusChanged :: C'StatusChanged -> AccountStatusChanged
mK_StatusChanged f = mK'StatusChanged f

#callback RequestAdd , Account -> CString -> CString -> CString -> CString -> IO ()
-- | Someone we don't have on our list added us; prompt to add them.
type AccountRequestAdd = Account
               -> String        -- ^ RemoteUser
               -> String        -- ^ ID
               -> String        -- ^ Alias
               -> String        -- ^ Message
               -> IO ()

mK_RequestAdd :: C'RequestAdd -> AccountRequestAdd
mK_RequestAdd f = \acc s1 s2 s3 s4 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    (mK'RequestAdd f) acc cs1 cs2 cs3 cs4

mk_RequestAdd :: AccountRequestAdd -> IO C'RequestAdd
mk_RequestAdd f = mk'RequestAdd $ \ptr cs1 cs2 cs3 cs4 -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    f ptr s1 s2 s3 s4

type UserData    = Ptr ()

#callback RequestAuthorizationCb , UIHandle -> IO ()
type AccountRequestAuthorizationCb = C'RequestAuthorizationCb

type AccountAuthorizeCb = UIHandle -> IO ()
type AccountDenyCb      = UIHandle -> IO ()

#callback RequestAuthorize , Account -> CString -> CString -> CString -> CString -> CInt -> <RequestAuthorizationCb> -> <RequestAuthorizationCb> -> UserData -> IO ()
-- | Prompt for authorization when someone adds this account to their buddy
-- list. To authorize them to see this account's presence, call a @AuthorizeCb
-- UserData@, otherwise call @DenyCb UserData@, returns a UI-specific handle,
-- as passed to "CloseAccountRequest".
type AccountRequestAuthorize = Account
                     -> String      -- ^ RemoteUser
                     -> String      -- ^ ID
                     -> String      -- ^ Alias
                     -> String      -- ^ Message
                     -> Bool        -- ^ On list?
                     -> AccountAuthorizeCb
                     -> AccountDenyCb
                     -> UserData
                     -> IO ()

mk_RequestAuthorize :: AccountRequestAuthorize -> IO C'RequestAuthorize
mk_RequestAuthorize f = mk'RequestAuthorize $ \ptr cs1 cs2 cs3 cs4 ci fp1 fp2 ud -> do
    s1  <- peekCString cs1
    s2  <- peekCString cs2
    s3  <- peekCString cs3
    s4  <- peekCString cs4
    let i = 1 == ci
        f1 = mK'RequestAuthorizationCb fp1
        f2 = mK'RequestAuthorizationCb fp2
    f ptr s1 s2 s3 s4 i f1 f2 ud

mK_RequestAuthorize :: C'RequestAuthorize -> AccountRequestAuthorize
mK_RequestAuthorize f = \acc s1 s2 s3 s4 lis f1 f2 ud -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    let ci = if lis then 1 else 0
    fp1 <- mk'RequestAuthorizationCb f1
    fp2 <- mk'RequestAuthorizationCb f2
    (mK'RequestAuthorize f) acc cs1 cs2 cs3 cs4 ci fp1 fp2 ud

#callback CloseAccountRequest , UIHandle -> IO ()
-- | Close a pending request for authorization. @UIHandle@ is a handle as
-- returned by "RequestAuthorize".
type AccountCloseAccountRequest = UIHandle -> IO ()

mk_CloseAccountRequest :: AccountCloseAccountRequest -> IO C'CloseAccountRequest
mk_CloseAccountRequest = mk'CloseAccountRequest

mK_CloseAccountRequest :: C'CloseAccountRequest -> AccountCloseAccountRequest
mK_CloseAccountRequest = mK'CloseAccountRequest
