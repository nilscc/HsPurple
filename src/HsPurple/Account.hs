{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Account
    (
    -- * UiOps
      AccountUiOps (..)

    -- * Account functions
    , accountNew
    , accountDestroy
    , accountConnect
    , accountDisconnect
    , accountRegister
    , accountUnregister
    , accountNotifyAdded
    , accountRequestAdd
    , accountRequestAuthorization
    , accountRequestCloseWithAccount
    , accountRequestClose
    , accountRequestChangePassword
    , accountRequestChangeUserinfo
    , accountSetUsername
    , accountSetPassword
    , accountSetAlias
    , accountSetUserinfo
    , accountSetBuddyIconPath
    , accountSetProtocolId
    , accountSetConnection
    , accountSetRememberPassword
    , accountSetCheckMail
    , accountSetEnabled
    , accountSetStatus
    , accountSetStatusTypes
    , accountSetStatusList
    , accountSetRegisterCallback
    , accountSetProxyInformation
    , accountClearSettings
    , accountIsConnected
    , accountIsConnecting
    , accountIsDisconnected
    , accountGetUsername
    , accountGetPassword
    , accountGetAlias
    , accountGetUserinfo
    , accountGetBuddyIconPath
    , accountGetProtocolId
    , accountGetProtocolName
    , accountGetConnection
    , accountGetRememberPassword
    , accountGetCheckMail
    , accountGetEnabled
    , accountGetProxyInfo
    , accountGetActiveStatus
    , accountGetStatus
    , accountGetStatusType
    , accountGetStatusTypeWithPrimitive
    , accountGetPresence
    , accountIsStatusActive
    , accountGetStatusTypes
    , accountGetLog
    , accountDestroyLog
    , accountAddBuddy
    , accountAddBuddies
    , accountRemoveBuddy
    , accountRemoveBuddies
    , accountRemoveGroup
    , accountChangePassword
    , accountSupportsOfflineMessage
    , accountGetCurrentError
    , accountClearCurrentError

    -- * Accounts functions
    , accountsAdd
    , accountsRemove
    , accountsDelete
    , accountsReorder
    , accountsGetAll
    , accountsGetAllActive
    , accountsFind
    , accountsRestoreCurrentStatuses
    ) where

import Foreign
import Foreign.C

import Bindings.GLib

import HsPurple.UiOps.AccountUiOps
import HsPurple.Util
import HsPurple.Status



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | We dont have a decent Account representation yet
type Account        = Ptr ()
type Connection     = Ptr ()
type UserData       = Ptr ()
type Status         = Ptr ()
type StatusType     = Ptr ()
type ProxyInfo      = Ptr ()
type Attr           = Ptr ()
type Presence       = Ptr ()
type Log            = Ptr ()
type Buddy          = Ptr ()
type Group          = Ptr ()
type PurpleError    = Ptr ()



--------------------------------------------------------------------------------
-- Function types
--------------------------------------------------------------------------------

-- typedef void (*PurpleAccountRegistrationCb)(PurpleAccount *account, gboolean succeeded, void *user_data);
type AccountRegistrationCb  = Account -> Bool -> UserData -> IO ()
type CAccountRegistrationCb = Account -> CInt -> UserData -> IO ()

-- cAccountRegistrationCb :: CAccountRegistrationCb -> AccountRegistrationCb
-- cAccountRegistrationCb f = \acc b ud -> f acc (if b then 1 else 0) ud
hAccountRegistrationCb :: AccountRegistrationCb -> CAccountRegistrationCb
hAccountRegistrationCb f = \acc ci ud -> f acc (ci == 1) ud

-- typedef void (*PurpleAccountUnregistrationCb)(PurpleAccount *account, gboolean succeeded, void *user_data);
type AccountUnregistrationCb  = Account -> Bool -> UserData -> IO ()
type CAccountUnregistrationCb = Account -> CInt -> UserData -> IO ()

foreign import ccall "wrapper"
    c_mk_account_unregistration_cb :: CAccountUnregistrationCb -> IO (FunPtr CAccountUnregistrationCb)

-- foreign import ccall "dynamic"
    -- c_get_account_unregistration_cb :: FunPtr CAccountUnregistrationCb -> CAccountUnregistrationCb

-- cAccountUnregistrationCb :: CAccountUnregistrationCb -> AccountUnregistrationCb
-- cAccountUnregistrationCb f = \acc b ud -> f acc (if b then 1 else 0) ud
hAccountUnregistrationCb :: AccountUnregistrationCb -> CAccountUnregistrationCb
hAccountUnregistrationCb f = \acc ci ud -> f acc (ci == 1) ud




--------------------------------------------------------------------------------
-- Account functions
--------------------------------------------------------------------------------

-- PurpleAccount *purple_account_new(const char *username, const char *protocol_id);

foreign import ccall "purple_account_new"
    c_purple_account_new :: CString -> CString -> IO Account

-- | Creates a new account. 
accountNew :: String    -- ^ Username
           -> String    -- ^ Protocol Id
           -> IO Account
accountNew un p = do
    cs1 <- newCString un
    cs2 <- newCString p
    c_purple_account_new cs1 cs2

-- void purple_account_destroy(PurpleAccount *account);

foreign import ccall "purple_account_destroy"
    c_purple_account_destroy :: Account -> IO ()

-- | Destroys an account. 
accountDestroy :: Account -> IO ()
accountDestroy = c_purple_account_destroy

-- void purple_account_connect(PurpleAccount *account);

foreign import ccall "purple_account_connect"
    c_purple_account_connect :: Account -> IO ()

-- | Connects to an account.
accountConnect :: Account -> IO ()
accountConnect = c_purple_account_connect

-- void purple_account_set_register_callback(PurpleAccount *account, PurpleAccountRegistrationCb cb, void *user_data);

foreign import ccall "purple_account_set_register_callback"
    c_purple_account_set_register_callback :: Account -> FunPtr CAccountRegistrationCb -> UserData -> IO ()

foreign import ccall "wrapper"
    c_mk_account_registration_cb :: CAccountRegistrationCb -> IO (FunPtr CAccountRegistrationCb)

-- foreign import ccall "dynamic"
    -- c_get_account_registration_cb :: FunPtr CAccountRegistrationCb -> CAccountRegistrationCb

-- | Sets the callback for successful registration.
accountSetRegisterCallback :: Account -> AccountRegistrationCb -> UserData -> IO ()
accountSetRegisterCallback a cb ud = do
    fp <- c_mk_account_registration_cb (hAccountRegistrationCb cb)
    c_purple_account_set_register_callback a fp ud


-- void purple_account_register(PurpleAccount *account);

foreign import ccall "purple_account_register"
    c_purple_account_register :: Account -> IO ()

-- | Registers an account. 
accountRegister :: Account -> IO ()
accountRegister = c_purple_account_register

-- void purple_account_unregister(PurpleAccount *account, PurpleAccountUnregistrationCb cb, void *user_data);

foreign import ccall "purple_account_unregister"
    c_purple_account_unregister :: Account -> FunPtr CAccountUnregistrationCb -> UserData -> IO ()

-- | Unregisters an account (deleting it from the server). 
accountUnregister :: Account -> AccountUnregistrationCb -> UserData -> IO ()
accountUnregister a cb ud = do
    fp <- c_mk_account_unregistration_cb (hAccountUnregistrationCb cb)
    c_purple_account_unregister a fp ud

-- void purple_account_disconnect(PurpleAccount *account);

foreign import ccall "purple_account_disconnect"
    c_purple_account_disconnect :: Account -> IO ()

-- | Disconnects from an account. 
accountDisconnect :: Account -> IO ()
accountDisconnect = c_purple_account_disconnect

-- void purple_account_notify_added(PurpleAccount *account, const char *remote_user,
--                                const char *id, const char *alias,
--                                const char *message);

foreign import ccall "purple_account_notify_added"
    c_purple_account_notify_added :: Account -> CString -> CString -> CString -> CString -> IO ()

-- | Notifies the user that the account was added to a remote user's buddy list. 
accountNotifyAdded :: Account
                   -> String    -- ^ Remote user
                   -> String    -- ^ Id
                   -> String    -- ^ Alias
                   -> String    -- ^ Message
                   -> IO ()
accountNotifyAdded a s1 s2 s3 s4 = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    c_purple_account_notify_added a cs1 cs2 cs3 cs4

-- void purple_account_request_add(PurpleAccount *account, const char *remote_user,
--                               const char *id, const char *alias,
--                               const char *message);

foreign import ccall "purple_account_request_add"
    c_purple_account_request_add :: Account -> CString -> CString -> CString -> CString -> IO ()

-- | Notifies the user that the account was addded to a remote user's buddy
-- list and asks ther user if they want to add the remote user to their buddy
-- list. 
accountRequestAdd :: Account
                  -> String    -- ^ Remote user
                  -> String    -- ^ Id
                  -> String    -- ^ Alias
                  -> String    -- ^ Message
                  -> IO ()
accountRequestAdd a s1 s2 s3 s4 = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    c_purple_account_request_add a cs1 cs2 cs3 cs4


-- void *purple_account_request_authorization(PurpleAccount *account, const char *remote_user,
--                     const char *id, const char *alias, const char *message, gboolean on_list,
--                     PurpleAccountRequestAuthorizationCb auth_cb, PurpleAccountRequestAuthorizationCb deny_cb, void *user_data);

foreign import ccall "purple_account_request_authorization"
    c_purple_account_request_authorization :: Account
                                           -> CString
                                           -> CString
                                           -> CString
                                           -> CString
                                           -> CInt
                                           -> FunPtr AccountRequestAuthorizationCb
                                           -> FunPtr AccountRequestAuthorizationCb
                                           -> UserData
                                           -> IO (Ptr ())

foreign import ccall "wrapper"
    c_mk_account_request_authorization_cb :: AccountRequestAuthorizationCb -> IO (FunPtr AccountRequestAuthorizationCb)

-- foreign import ccall "dynamic"
    -- c_get_account_request_authorization_cb :: FunPtr AccountRequestAuthorizationCb -> AccountRequestAuthorizationCb

-- | Notifies the user that a remote user has wants to add the local user to
-- his or her buddy list and requires authorization to do so. 
accountRequestAuthorization :: Account
                            -> String    -- ^ Remote user
                            -> String    -- ^ Id
                            -> String    -- ^ Alias
                            -> String    -- ^ Message
                            -> Bool      -- ^ On list
                            -> AccountRequestAuthorizationCb -- Auth callback
                            -> AccountRequestAuthorizationCb -- Deny callback
                            -> UserData
                            -> IO UIHandle
accountRequestAuthorization a s1 s2 s3 s4 b cb1 cb2 ud = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    cs4 <- newCString s4
    fp1 <- c_mk_account_request_authorization_cb cb1
    fp2 <- c_mk_account_request_authorization_cb cb2
    c_purple_account_request_authorization a cs1 cs2 cs3 cs4 (if b then 1 else 0) fp1 fp2 ud

-- void purple_account_request_close_with_account(PurpleAccount *account);

foreign import ccall "purple_account_request_close_with_account"
    c_purple_account_request_close_with_account :: Account -> IO ()

-- | Close account requests registered for the given PurpleAccount. 
accountRequestCloseWithAccount :: Account -> IO ()
accountRequestCloseWithAccount = c_purple_account_request_close_with_account


-- void purple_account_request_close(void *ui_handle);

foreign import ccall "purple_account_request_close"
    c_purple_account_request_close :: UIHandle -> IO ()

-- | Close the account request for the given ui handle. 
accountRequestClose :: UIHandle -> IO ()
accountRequestClose = c_purple_account_request_close

-- TODO:
--
-- void purple_account_request_password(PurpleAccount *account, GCallback ok_cb,
--                      GCallback cancel_cb, void *user_data);

-- void purple_account_request_change_password(PurpleAccount *account);

foreign import ccall "purple_account_request_change_password"
    c_purple_account_request_change_password :: Account -> IO ()

-- | Requests information from the user to change the account's password. 
accountRequestChangePassword :: Account -> IO ()
accountRequestChangePassword = c_purple_account_request_change_password

-- void purple_account_request_change_user_info(PurpleAccount *account);

foreign import ccall "purple_account_request_change_user_info"
    c_purple_account_request_change_user_info :: Account -> IO ()

-- | Requests information from the user to change the account's user information. 
accountRequestChangeUserinfo :: Account -> IO ()
accountRequestChangeUserinfo = c_purple_account_request_change_user_info

-- void purple_account_set_username(PurpleAccount *account, const char *username);

foreign import ccall "purple_account_set_username"
    c_purple_account_set_username :: Account -> CString -> IO ()

-- | Sets the account's username. 
accountSetUsername :: Account
                   -> String    -- ^ Username
                   -> IO ()
accountSetUsername a s = do
    cs <- newCString s
    c_purple_account_set_username a cs

-- void purple_account_set_password(PurpleAccount *account, const char *password);

foreign import ccall "purple_account_set_password"
    c_purple_account_set_password :: Account -> CString -> IO ()

-- | Sets the account's password. 
accountSetPassword :: Account
                   -> String    -- ^ Password
                   -> IO ()
accountSetPassword a s = do
    cs <- newCString s
    c_purple_account_set_password a cs

-- void purple_account_set_alias(PurpleAccount *account, const char *alias);

foreign import ccall "purple_account_set_alias"
    c_purple_account_set_alias :: Account -> CString -> IO ()

-- | Sets the account's alias. 
accountSetAlias :: Account
                -> String   -- ^ Alias
                -> IO ()
accountSetAlias a s = do
    cs <- newCString s
    c_purple_account_set_alias a cs

-- void purple_account_set_user_info(PurpleAccount *account, const char *user_info);

foreign import ccall "purple_account_set_user_info"
    c_purple_account_set_user_info :: Account -> CString -> IO ()

-- | Sets the account's user information. 
accountSetUserinfo :: Account -> String -> IO ()
accountSetUserinfo a s = do
    cs <- newCString s
    c_purple_account_set_user_info a cs

-- void purple_account_set_buddy_icon_path(PurpleAccount *account, const char *path);

foreign import ccall "purple_account_set_buddy_icon_path"
    c_purple_account_set_buddy_icon_path :: Account -> CString -> IO ()

-- | Sets the account's buddy icon path. 
accountSetBuddyIconPath :: Account -> FilePath -> IO ()
accountSetBuddyIconPath a s = do
    cs <- newCString s
    c_purple_account_set_buddy_icon_path a cs

-- void purple_account_set_protocol_id(PurpleAccount *account,
--                                   const char *protocol_id);

foreign import ccall "purple_account_set_protocol_id"
    c_purple_account_set_protocol_id :: Account -> CString -> IO ()

-- | Sets the account's protocol ID. 
accountSetProtocolId :: Account -> String -> IO ()
accountSetProtocolId a s = do
    cs <- newCString s
    c_purple_account_set_protocol_id a cs

-- void purple_account_set_connection(PurpleAccount *account, PurpleConnection *gc);

foreign import ccall "purple_account_set_connection"
    c_purple_account_set_connection :: Account -> Connection -> IO ()

-- | Sets the account's connection. 
accountSetConnection :: Account -> Connection -> IO ()
accountSetConnection = c_purple_account_set_connection

-- void purple_account_set_remember_password(PurpleAccount *account, gboolean value);

foreign import ccall "purple_account_set_remember_password"
    c_purple_account_set_remember_password :: Account -> CInt -> IO ()

-- | Sets whether or not this account should save its password. 
accountSetRememberPassword :: Account -> Bool -> IO ()
accountSetRememberPassword a b =
    c_purple_account_set_remember_password a (if b then 1 else 0)

-- void purple_account_set_check_mail(PurpleAccount *account, gboolean value);

foreign import ccall "purple_account_set_check_mail"
    c_purple_account_set_check_mail :: Account -> CInt -> IO ()

-- | Sets whether or not this account should check for mail. 
accountSetCheckMail :: Account -> Bool -> IO ()
accountSetCheckMail a b =
    c_purple_account_set_check_mail a (if b then 1 else 0)

-- void purple_account_set_enabled(PurpleAccount *account, const char *ui,
--                   gboolean value);

foreign import ccall "purple_account_set_enabled"
    c_purple_account_set_enabled :: Account -> CString -> CInt -> IO ()

-- | Sets whether or not this account is enabled for the specified UI. 
accountSetEnabled :: Account
                  -> String     -- UI
                  -> Bool
                  -> IO ()
accountSetEnabled a s b = do
    cs <- newCString s
    c_purple_account_set_enabled a cs (if b then 1 else 0)

-- void purple_account_set_proxy_info(PurpleAccount *account, PurpleProxyInfo *info);

foreign import ccall "purple_account_set_proxy_info"
    c_purple_account_set_proxy_info :: Account -> ProxyInfo -> IO ()

-- | Sets the account's proxy information. 
accountSetProxyInformation :: Account -> ProxyInfo -> IO ()
accountSetProxyInformation = c_purple_account_set_proxy_info

-- void purple_account_set_status_types(PurpleAccount *account, GList *status_types);

foreign import ccall "purple_account_set_status_types"
    c_purple_account_set_status_types :: Account -> Ptr C'GList -> IO ()

-- | Sets the account's status types. 
accountSetStatusTypes :: Account -> [StatusType] -> IO ()
accountSetStatusTypes a lis = alloca $ \ptr -> do
    ptr' <- listToGList ptr lis
    c_purple_account_set_status_types a ptr'

-- void purple_account_set_status(PurpleAccount *account, const char *status_id,
--     gboolean active, ...) G_GNUC_NULL_TERMINATED; -- WTF?

foreign import ccall "purple_account_set_status"
    c_purple_account_set_status :: Account -> CString -> CInt -> Ptr () -> IO ()

-- | Variadic version of purple_account_set_status_list(); the variadic list
-- replaces attrs, and should be NULL-terminated. 
accountSetStatus :: Account
                 -> String  -- ^ Status ID
                 -> Bool    -- ^ Active
                 -> IO ()
accountSetStatus a s b = do
    cs <- newCString s
    c_purple_account_set_status a cs (if b then 1 else 0) nullPtr

-- void purple_account_set_status_list(PurpleAccount *account,
--     const char *status_id, gboolean active, GList *attrs);

foreign import ccall "purple_account_set_status_list"
    c_purple_account_set_status_list :: Account -> CString -> CInt -> Ptr C'GList -> IO ()

-- | Activates or deactivates a status. 
accountSetStatusList :: Account
                     -> String      -- ^ Status Id
                     -> Bool        -- ^ Active
                     -> [Attr]      -- ^ Attributes (?)
                     -> IO ()
accountSetStatusList a s b lis = alloca $ \ptr -> do
    cs <- newCString s
    ptr' <- listToGList ptr lis
    c_purple_account_set_status_list a cs (if b then 1 else 0) ptr'

-- void purple_account_clear_settings(PurpleAccount *account);

foreign import ccall "purple_account_clear_settings"
    c_purple_account_clear_settings :: Account -> IO ()

-- | Clears all protocol-specific settings on an account. 
accountClearSettings :: Account -> IO ()
accountClearSettings = c_purple_account_clear_settings

-- gboolean purple_account_is_connected(const PurpleAccount *account);

foreign import ccall "purple_account_is_connected"
    c_purple_account_is_connected :: Account -> IO CInt

-- | Returns whether or not the account is connected. 
accountIsConnected :: Account -> IO Bool
accountIsConnected a = (1 ==) `fmap` c_purple_account_is_connected a

-- gboolean purple_account_is_connecting(const PurpleAccount *account);

foreign import ccall "purple_account_is_connecting"
    c_purple_account_is_connecting :: Account -> IO CInt

-- | Returns whether or not the account is connecting. 
accountIsConnecting :: Account -> IO Bool
accountIsConnecting a = (1 ==) `fmap` c_purple_account_is_connecting a

-- gboolean purple_account_is_disconnected(const PurpleAccount *account);

foreign import ccall "purple_account_is_disconnected"
    c_purple_account_is_disconnected :: Account -> IO CInt

-- | Returns whether or not the account is disconnected. 
accountIsDisconnected :: Account -> IO Bool
accountIsDisconnected a = (1 ==) `fmap` c_purple_account_is_disconnected a

-- const char *purple_account_get_username(const PurpleAccount *account);

foreign import ccall "purple_account_get_username"
    c_purple_account_get_username :: Account -> IO CString

-- | Returns the account's username. 
accountGetUsername :: Account -> IO String
accountGetUsername a = do
    c_purple_account_get_username a >>= peekCString

-- const char *purple_account_get_password(const PurpleAccount *account);

foreign import ccall "purple_account_get_password"
    c_purple_account_get_password :: Account -> IO CString

-- |Returns the account's password. 
accountGetPassword :: Account -> IO String
accountGetPassword a = do
    c_purple_account_get_password a >>= peekCString

-- const char *purple_account_get_alias(const PurpleAccount *account);

foreign import ccall "purple_account_get_alias"
    c_purple_account_get_alias :: Account -> IO CString

-- | Returns the account's alias. 
accountGetAlias :: Account -> IO String
accountGetAlias a = do
    c_purple_account_get_alias a >>= peekCString

-- const char *purple_account_get_user_info(const PurpleAccount *account);

foreign import ccall "purple_account_get_user_info"
    c_purple_account_get_user_info :: Account -> IO CString

-- | Returns the account's user information. 
accountGetUserinfo :: Account -> IO String
accountGetUserinfo a = do
    c_purple_account_get_user_info a >>= peekCString

-- const char *purple_account_get_buddy_icon_path(const PurpleAccount *account);

foreign import ccall "purple_account_get_buddy_icon_path"
    c_purple_account_get_buddy_icon_path :: Account -> IO CString

-- | Gets the account's buddy icon path. 
accountGetBuddyIconPath :: Account -> IO FilePath
accountGetBuddyIconPath a = do
    c_purple_account_get_buddy_icon_path a >>= peekCString

-- const char *purple_account_get_protocol_id(const PurpleAccount *account);

foreign import ccall "purple_account_get_protocol_id"
    c_purple_account_get_protocol_id :: Account -> IO CString

-- | Returns the account's protocol ID. 
accountGetProtocolId :: Account -> IO String
accountGetProtocolId a = do
    c_purple_account_get_protocol_id a >>= peekCString

-- const char *purple_account_get_protocol_name(const PurpleAccount *account);

foreign import ccall "purple_account_get_protocol_name"
    c_purple_account_get_protocol_name :: Account -> IO CString

-- | Returns the account's protocol name. 
accountGetProtocolName :: Account -> IO String
accountGetProtocolName a = do
    c_purple_account_get_protocol_name a >>= peekCString

-- PurpleConnection *purple_account_get_connection(const PurpleAccount *account);

foreign import ccall "purple_account_get_connection"
    c_purple_account_get_connection :: Account -> IO Connection

-- | Returns the account's connection. 
accountGetConnection :: Account -> IO Connection
accountGetConnection = c_purple_account_get_connection

-- gboolean purple_account_get_remember_password(const PurpleAccount *account);

foreign import ccall "purple_account_get_remember_password"
    c_purple_account_get_remember_password :: Account -> IO CInt

-- | Returns whether or not this account should save its password. 
accountGetRememberPassword :: Account -> IO Bool
accountGetRememberPassword a =
    (1 ==) `fmap` c_purple_account_get_remember_password a

-- gboolean purple_account_get_check_mail(const PurpleAccount *account);

foreign import ccall "purple_account_get_check_mail"
    c_purple_account_get_check_mail :: Account -> IO CInt

-- | Returns whether or not this account should check for mail. 
accountGetCheckMail :: Account -> IO Bool
accountGetCheckMail a =
    (1 ==) `fmap` c_purple_account_get_check_mail a

-- gboolean purple_account_get_enabled(const PurpleAccount *account,
--                   const char *ui);

foreign import ccall "purple_account_get_enabled"
    c_purple_account_get_enabled :: Account -> CString -> IO CInt

-- | Returns whether or not this account is enabled for the specified UI. 
accountGetEnabled :: Account
                  -> String     -- ^ UI
                  -> IO Bool
accountGetEnabled a s = do
    cs <- newCString s
    (1 ==) `fmap` c_purple_account_get_enabled a cs

-- PurpleProxyInfo *purple_account_get_proxy_info(const PurpleAccount *account);

foreign import ccall "purple_account_get_proxy_info"
    c_purple_account_get_proxy_info :: Account -> IO ProxyInfo

-- | Returns the account's proxy information. 
accountGetProxyInfo :: Account -> IO ProxyInfo
accountGetProxyInfo = c_purple_account_get_proxy_info

-- PurpleStatus *purple_account_get_active_status(const PurpleAccount *account);

foreign import ccall "purple_account_get_active_status"
    c_purple_account_get_active_status :: Account -> IO Status

-- | Returns the active status for this account. 
accountGetActiveStatus :: Account -> IO Status
accountGetActiveStatus = c_purple_account_get_active_status

-- PurpleStatus *purple_account_get_status(const PurpleAccount *account,
--                                     const char *status_id);

foreign import ccall "purple_account_get_status"
    c_purple_account_get_status :: Account -> CString -> IO Status

-- | Returns the account status with the specified ID. 
accountGetStatus :: Account
                 -> String  -- ^ Status Id
                 -> IO Status
accountGetStatus a s = do
    cs <- newCString s
    c_purple_account_get_status a cs

-- PurpleStatusType *purple_account_get_status_type(const PurpleAccount *account,
--                                              const char *id);

foreign import ccall "purple_account_get_status_type"
    c_purple_account_get_status_type :: Account -> CString -> IO StatusType

-- | Returns the account status type with the specified ID. 
accountGetStatusType :: Account
                     -> String      -- ^ Id
                     -> IO StatusType
accountGetStatusType a s = do
    cs <- newCString s
    c_purple_account_get_status_type a cs

-- PurpleStatusType *purple_account_get_status_type_with_primitive(
--                             const PurpleAccount *account,
--                             PurpleStatusPrimitive primitive);

foreign import ccall "purple_account_get_status_type_with_primitive"
    c_purple_account_get_status_type_with_primitive :: Account -> CInt -> IO StatusType

-- | Returns the account status type with the specified primitive. 
accountGetStatusTypeWithPrimitive :: Account -> StatusPrimitive -> IO StatusType
accountGetStatusTypeWithPrimitive a st =
    c_purple_account_get_status_type_with_primitive a (fromIntegral $ unStatusPrimitive st)

-- PurplePresence *purple_account_get_presence(const PurpleAccount *account);

foreign import ccall "purple_account_get_presence"
    c_purple_account_get_presence :: Account -> IO Presence

-- | Returns the account's presence. 
accountGetPresence :: Account -> IO Presence
accountGetPresence = c_purple_account_get_presence

-- gboolean purple_account_is_status_active(const PurpleAccount *account,
--                                        const char *status_id);

foreign import ccall "purple_account_is_status_active"
    c_purple_account_is_status_active :: Account -> CString -> IO CInt

-- | Returns whether or not an account status is active. 
accountIsStatusActive :: Account
                      -> String     -- ^ Status Id
                      -> IO Bool
accountIsStatusActive a s = do
    cs <- newCString s
    (1 ==) `fmap` c_purple_account_is_status_active a cs

-- GList *purple_account_get_status_types(const PurpleAccount *account);

foreign import ccall "purple_account_get_status_types"
    c_purple_account_get_status_types :: Account -> IO (Ptr C'GList)

-- | Returns the account's status types. 
accountGetStatusTypes :: Account -> IO [StatusType]
accountGetStatusTypes a =
    c_purple_account_get_status_types a >>= peek >>= gListToList

-- PurpleLog *purple_account_get_log(PurpleAccount *account, gboolean create);

foreign import ccall "purple_account_get_log"
    c_purple_account_get_log :: Account -> CInt -> IO Log

-- | Returns the system log for an account. 
accountGetLog :: Account
              -> Bool       -- ^ Create?
              -> IO Log
accountGetLog a b = c_purple_account_get_log a (if b then 1 else 0)


-- void purple_account_destroy_log(PurpleAccount *account);

foreign import ccall "purple_account_destroy_log"
    c_purple_account_destroy_log :: Account -> IO ()

-- | Frees the system log of an account. 
accountDestroyLog :: Account -> IO ()
accountDestroyLog = c_purple_account_destroy_log

-- void purple_account_add_buddy(PurpleAccount *account, PurpleBuddy *buddy);

foreign import ccall "purple_account_add_buddy"
    c_purple_account_add_buddy :: Account -> Buddy -> IO ()

-- | Adds a buddy to the server-side buddy list for the specified account. 
accountAddBuddy :: Account -> Buddy -> IO ()
accountAddBuddy = c_purple_account_add_buddy

-- void purple_account_add_buddies(PurpleAccount *account, GList *buddies);

foreign import ccall "purple_account_add_buddies"
    c_purple_account_add_buddies :: Account -> Ptr C'GList -> IO ()

-- | Adds a list of buddies to the server-side buddy list. 
accountAddBuddies :: Account -> [Buddy] -> IO ()
accountAddBuddies a lis = alloca $ \ptr -> do
    ptr' <- listToGList ptr lis
    c_purple_account_add_buddies a ptr'

-- void purple_account_remove_buddy(PurpleAccount *account, PurpleBuddy *buddy,
--                                 PurpleGroup *group);

foreign import ccall "purple_account_remove_buddy"
    c_purple_account_remove_buddy :: Account -> Buddy -> Group -> IO ()

-- | Removes a buddy from the server-side buddy list. 
accountRemoveBuddy :: Account -> Buddy -> Group -> IO ()
accountRemoveBuddy = c_purple_account_remove_buddy

-- void purple_account_remove_buddies(PurpleAccount *account, GList *buddies,
--                                     GList *groups);

foreign import ccall "purple_account_remove_buddies"
    c_purple_account_remove_buddies :: Account -> Ptr C'GList -> Ptr C'GList -> IO ()

-- | Removes a list of buddies from the server-side buddy list. 
accountRemoveBuddies :: Account -> [(Buddy, Group)] -> IO ()
accountRemoveBuddies a lis = alloca $ \ptr1 -> alloca $ \ptr2 -> do
    ptr1' <- listToGList ptr1 $ map fst lis -- buddies
    ptr2' <- listToGList ptr2 $ map snd lis -- groups
    c_purple_account_remove_buddies a ptr1' ptr2'

-- void purple_account_remove_group(PurpleAccount *account, PurpleGroup *group);

foreign import ccall "purple_account_remove_group"
    c_purple_account_remove_group :: Account -> Group -> IO ()

-- | Removes a group from the server-side buddy list. 
accountRemoveGroup :: Account -> Group -> IO ()
accountRemoveGroup = c_purple_account_remove_group

-- void purple_account_change_password(PurpleAccount *account, const char *orig_pw,
--                                     const char *new_pw);

foreign import ccall "purple_account_change_password"
    c_purple_account_change_password :: Account -> CString -> CString -> IO ()

-- | Changes the password on the specified account. 
accountChangePassword :: Account
                      -> String     -- ^ Old password
                      -> String     -- ^ New password
                      -> IO ()
accountChangePassword a s1 s2 = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c_purple_account_change_password a cs1 cs2
    

-- gboolean purple_account_supports_offline_message(PurpleAccount *account, PurpleBuddy *buddy);

foreign import ccall "purple_account_supports_offline_message"
    c_purple_account_supports_offline_message :: Account -> Buddy -> IO CInt

-- | Whether the account supports sending offline messages to buddy. 
accountSupportsOfflineMessage :: Account -> Buddy -> IO Bool
accountSupportsOfflineMessage a b =
    (1 ==) `fmap` c_purple_account_supports_offline_message a b


-- const PurpleConnectionErrorInfo *purple_account_get_current_error(PurpleAccount *account);

foreign import ccall "purple_account_get_current_error"
    c_purple_account_get_current_error :: Account -> IO PurpleError

-- | Get the error that caused the account to be disconnected, or NULL if the
-- account is happily connected or disconnected without an error. 
accountGetCurrentError :: Account -> IO (Maybe PurpleError)
accountGetCurrentError a' = makeMaybe `fmap` c_purple_account_get_current_error a'
  where makeMaybe a | a == nullPtr = Nothing
                    | otherwise    = Just a


-- void purple_account_clear_current_error(PurpleAccount *account);

foreign import ccall "purple_account_clear_current_error"
    c_purple_account_clear_current_error :: Account -> IO ()

-- | Clear an account's current error state, resetting it to NULL. 
accountClearCurrentError :: Account -> IO ()
accountClearCurrentError = c_purple_account_clear_current_error





--------------------------------------------------------------------------------
-- Accounts functions
--------------------------------------------------------------------------------

{-
/**
 * Adds an account to the list of accounts.
 *
 * @param account The account.
 */
void purple_accounts_add(PurpleAccount *account);
-}

foreign import ccall "purple_accounts_add "
    c_purple_accounts_add  :: Account -> IO ()


-- | Adds an account to the list of accounts.
accountsAdd :: Account -> IO ()
accountsAdd = c_purple_accounts_add

-- void purple_accounts_remove(PurpleAccount *account);

foreign import ccall "purple_accounts_remove"
    c_purple_accounts_remove :: Account -> IO ()

-- | Removes an account from the list of accounts.
accountsRemove :: Account -> IO ()
accountsRemove = c_purple_accounts_remove

-- void purple_accounts_delete(PurpleAccount *account);

foreign import ccall "purple_accounts_delete "
    c_purple_accounts_delete  :: Account -> IO ()

-- | Deletes an account.
-- This will remove any buddies from the buddy list that belong to this
-- account, buddy pounces that belong to this account, and will also
-- destroy @a account.
accountsDelete :: Account -> IO ()
accountsDelete = c_purple_accounts_delete

-- void purple_accounts_reorder(PurpleAccount *account, gint new_index);

foreign import ccall "purple_accounts_reorder"
    c_purple_accounts_reorder :: Account -> CInt -> IO ()

-- | Reorders an account.
accountsReorder :: Account
                -> Int      -- ^ New index
                -> IO ()
accountsReorder a i = c_purple_accounts_reorder a (fromIntegral i)

-- GList *purple_accounts_get_all(void);

foreign import ccall "purple_accounts_get_all"
    c_purple_accounts_get_all :: IO (Ptr C'GList)

-- | Returns a list of all accounts.
accountsGetAll :: IO [Account]
accountsGetAll =
    c_purple_accounts_get_all >>= peek >>= gListToList

-- GList *purple_accounts_get_all_active(void);

foreign import ccall "purple_accounts_get_all_active"
    c_purple_accounts_get_all_active :: IO (Ptr C'GList)

-- | Returns a list of all enabled accounts.
-- The list is owned by the caller, and must be g_list_free()d to avoid leaking
-- the nodes.
accountsGetAllActive :: IO [Account]
accountsGetAllActive =
    c_purple_accounts_get_all_active >>= peek >>= gListToList

-- PurpleAccount *purple_accounts_find(const char *name, const char *protocol);

foreign import ccall "purple_accounts_find"
    c_purple_accounts_find :: CString -> CString -> IO Account

-- | Finds an account with the specified name and protocol id.
accountsFind :: String      -- The account username.
             -> String      -- The account protocol ID.
             -> IO Account
accountsFind s1 s2 = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    c_purple_accounts_find cs1 cs2

-- void purple_accounts_restore_current_statuses(void);

foreign import ccall "purple_accounts_restore_current_statuses"
    c_purple_accounts_restore_current_statuses :: IO ()

-- | This is called by the core after all subsystems and what
-- not have been initialized.  It sets all enabled accounts
-- to their startup status by signing them on, setting them
-- away, etc.
-- 
-- You probably shouldn't call this unless you really know
-- what you're doing.
accountsRestoreCurrentStatuses :: IO ()
accountsRestoreCurrentStatuses = c_purple_accounts_restore_current_statuses
