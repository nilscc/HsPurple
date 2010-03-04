-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.ConnectionUiOps
    (
    -- * ConnectionUiOps
      ConnectionUiOps (..)
    , ConnectProgress
    , Connected
    , Disconnected
    , Notice
    , NetworkConnected
    , NetworkDisconnected
    , ReportDisconnectReason

    -- * Enums
    , ConnectionError
    , errorNetworkError
    , errorInvalidUsername
    , errorAuthenticationFailed
    , errorAuthenticationImpossible
    , errorNoSslSupport
    , errorEncryptionError
    , errorNameInUse
    , errorInvalidSettings
    , errorCertNotProvided
    , errorCertUntrusted
    , errorCertExpired
    , errorCertNotActivated
    , errorCertHostnameMismatch
    , errorCertFingerprintMismatch
    , errorCertSelfSigned
    , errorCertOtherError
    , errorOtherError

    , ConnectionState
    , stateDisconnected
    , stateConnected
    , stateConnecting

    , ConnectionFlags
    , connectionHtml
    , connectionNoBgcolor
    , connectionAutoResp
    , connectionFormattingWbfo
    , connectionNoNewlines
    , connectionNoFontsize
    , connectionNoUrldesc
    , connectionNoImages
    , connectionAllowCustomSmiley
    ) where

import Control.Applicative
import Foreign
import Foreign.C



--------------------------------------------------------------------------------
-- Haskell data representation
--------------------------------------------------------------------------------

type Connection = Ptr ()

data ConnectionUiOps = ConnectionUiOps
    { connectProgress               :: ConnectProgress
    , connected                     :: Connected
    , disconnected                  :: Disconnected
    , notice                        :: Notice
    -- , reportDisconnect              :: ReportDisconnect -- depreciated
    , networkConnected              :: NetworkConnected
    , networkDisconnected           :: NetworkDisconnected
    , reportDisconnectReason        :: ReportDisconnectReason
    }

-- | When an account is connecting, this operation is called to notify
-- the UI of what is happening, as well as which \"step\" out of
-- \"step count\" has been reached (which might be displayed as a progress
-- bar).
type ConnectProgress = Connection
                    -> String           -- ^ Text
                    -> Int              -- ^ Step
                    -> Int              -- ^ Step count
                    -> IO ()

-- | Called when a connection is established (just before the
-- @ref signed-on signal).
type Connected = Connection -> IO ()

-- | Called when a connection is ended (between the @ref signing-off
-- and @ref signed-off signals).
type Disconnected = Connection -> IO ()

-- | Used to display connection-specific notices.  (Pidgin's Gtk user
-- interface implements this as a no-op; purple_connection_notice(),
-- which uses this operation, is not used by any of the protocols
-- shipped with libpurple.)
type Notice = Connection
           -> String        -- ^ Text
           -> IO ()

{-
-- | Called when an error causes a connection to be disconnected.
-- Called before disconnected.
type ReportDisconnect = Connection
                     -> String      -- ^ Localized error message
                     -> IO ()
-}

-- | Called when an error causes a connection to be disconnected.
--  Called before disconnected.  This op is intended to replace
--  report_disconnect.  If both are implemented, this will be called
--  first; however, there's no real reason to implement both.
type ReportDisconnectReason = Connection
                           -> ConnectionError   -- ^ Error code
                           -> String            -- ^ Localized message
                                                -- describing the disconnection
                                                -- in more detail to the user.
                           -> IO ()

-- | Called when libpurple discovers that the computer's network
-- connection has gone away.
type NetworkConnected = IO ()

-- | Called when libpurple discovers that the computer's network
-- connection is active.  On Linux, this uses Network Manager if
-- available; on Windows, it uses Win32's network change notification
-- infrastructure.
type NetworkDisconnected = IO ()

instance Storable ConnectionUiOps where
    sizeOf _    = #size PurpleConnectionUiOps
    alignment _ = #alignof PurpleConnectionUiOps
    peek ptr    = get_ConnectionUiOps <$> peek (castPtr ptr)
    poke ptr ui = mk_ConnectionUiOps ui >>= poke (castPtr ptr)


-- | Convert a ConnectionUiOps struct into its C representation
mk_ConnectionUiOps :: ConnectionUiOps -> IO C'PurpleConnectionUiOps
mk_ConnectionUiOps (ConnectionUiOps con_p
                                    con
                                    dis
                                    note
                                    -- r_dis
                                    n_con
                                    n_dis
                                    r_dis_r) =

    C'PurpleConnectionUiOps <$> mk_ConnectProgress          con_p
                            <*> mk_Connected                con
                            <*> mk_Disconnected             dis
                            <*> mk_Notice                   note
                            -- <*> mk_ReportDisconnect         r_dis
                            <*> return nullFunPtr
                            <*> mk_NetworkConnected         n_con
                            <*> mk_NetworkDisconnected      n_dis
                            <*> mk_ReportDisconnectReason   r_dis_r
                            <*> return nullPtr
                            <*> return nullPtr
                            <*> return nullPtr


-- | Convert a C'PurpleConnectionUiOps struct into its Haskell representation
get_ConnectionUiOps :: C'PurpleConnectionUiOps -> ConnectionUiOps
get_ConnectionUiOps (C'PurpleConnectionUiOps con_p
                                             con
                                             dis
                                             note
                                             _ -- r_dis
                                             n_con
                                             n_dis
                                             r_dis_r
                                             _ _ _ -- nullpointer
                                             ) =

    ConnectionUiOps ( get_ConnectProgress          con_p   )
                    ( get_Connected                con     )
                    ( get_Disconnected             dis     )
                    ( get_Notice                   note    )
                    -- ( get_ReportDisconnect         r_dis   )
                    ( get_NetworkConnected         n_con   )
                    ( get_NetworkDisconnected      n_dis   )
                    ( get_ReportDisconnectReason   r_dis_r )  



--------------------------------------------------------------------------------
-- Create C versions of our callback functions
--------------------------------------------------------------------------------

mk_ConnectProgress          :: ConnectProgress          -> IO C'ConnectProgress
mk_ConnectProgress f =
    mk'ConnectProgress $ \con cs1 ci1 ci2 -> do
        s1 <- peekCString cs1
        f con s1 (fi ci1) (fi ci2)

mk_Connected                :: Connected                -> IO C'Connected
mk_Connected =
    mk'Connected

mk_Disconnected             :: Disconnected             -> IO C'Disconnected
mk_Disconnected =
    mk'Disconnected

mk_Notice                   :: Notice                   -> IO C'Notice
mk_Notice f =
    mk'Notice $ \con cs1 -> do
        s1 <- peekCString cs1
        f con s1

{-
mk_ReportDisconnect         :: ReportDisconnect         -> IO C'ReportDisconnect
mk_ReportDisconnect f =
    mk'ReportDisconnect $ \con cs1 -> do
        s1 <- peekCString cs1
        f con s1
-}

mk_ReportDisconnectReason   :: ReportDisconnectReason   -> IO C'ReportDisconnectReason
mk_ReportDisconnectReason f =
    mk'ReportDisconnectReason $ \con ce cs1 -> do
        s1 <- peekCString cs1
        f con ce s1

mk_NetworkConnected         :: NetworkConnected         -> IO C'NetworkConnected
mk_NetworkConnected =
    mk'NetworkConnected

mk_NetworkDisconnected      :: NetworkDisconnected      -> IO C'NetworkDisconnected
mk_NetworkDisconnected =
    mk'NetworkDisconnected


--------------------------------------------------------------------------------
-- Create Haskell versions of our callback functions
--------------------------------------------------------------------------------

get_ConnectProgress          :: C'ConnectProgress          -> ConnectProgress
get_ConnectProgress f = \con s1 i1 i2 -> do
    cs1 <- newCString s1
    (mK'ConnectProgress f) con cs1 (fi i1) (fi i2)

get_Connected                :: C'Connected                -> Connected
get_Connected = mK'Connected

get_Disconnected             :: C'Disconnected             -> Disconnected
get_Disconnected = mK'Disconnected

get_Notice                   :: C'Notice                   -> Notice
get_Notice f = \con s1 -> do
    cs1 <- newCString s1
    (mK'Notice f) con cs1

{-
get_ReportDisconnect         :: C'ReportDisconnect         -> ReportDisconnect
get_ReportDisconnect f =
    mK'ReportDisconnect $ \con s1 -> do
        cs1 <- newCString s1
        f con cs1
-}

get_ReportDisconnectReason   :: C'ReportDisconnectReason   -> ReportDisconnectReason
get_ReportDisconnectReason f = \con ce s1 -> do
        cs1 <- newCString s1
        (mK'ReportDisconnectReason f) con ce cs1

get_NetworkConnected         :: C'NetworkConnected         -> NetworkConnected
get_NetworkConnected = mK'NetworkConnected

get_NetworkDisconnected      :: C'NetworkDisconnected      -> NetworkDisconnected
get_NetworkDisconnected = mK'NetworkDisconnected



--------------------------------------------------------------------------------
-- ForeignFunctionInterface macros
--------------------------------------------------------------------------------

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <bindings.dsl.h>
#include <purple.h>
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

type ConnectionError = C'PurpleConnectionError
errorNetworkError :: ConnectionError
errorNetworkError = c'PURPLE_CONNECTION_ERROR_NETWORK_ERROR
errorInvalidUsername :: ConnectionError
errorInvalidUsername = c'PURPLE_CONNECTION_ERROR_INVALID_USERNAME
errorAuthenticationFailed :: ConnectionError
errorAuthenticationFailed = c'PURPLE_CONNECTION_ERROR_AUTHENTICATION_FAILED
errorAuthenticationImpossible :: ConnectionError
errorAuthenticationImpossible = c'PURPLE_CONNECTION_ERROR_AUTHENTICATION_IMPOSSIBLE
errorNoSslSupport :: ConnectionError
errorNoSslSupport = c'PURPLE_CONNECTION_ERROR_NO_SSL_SUPPORT
errorEncryptionError :: ConnectionError
errorEncryptionError = c'PURPLE_CONNECTION_ERROR_ENCRYPTION_ERROR
errorNameInUse :: ConnectionError
errorNameInUse = c'PURPLE_CONNECTION_ERROR_NAME_IN_USE
errorInvalidSettings :: ConnectionError
errorInvalidSettings = c'PURPLE_CONNECTION_ERROR_INVALID_SETTINGS
errorCertNotProvided :: ConnectionError
errorCertNotProvided = c'PURPLE_CONNECTION_ERROR_CERT_NOT_PROVIDED
errorCertUntrusted :: ConnectionError
errorCertUntrusted = c'PURPLE_CONNECTION_ERROR_CERT_UNTRUSTED
errorCertExpired :: ConnectionError
errorCertExpired = c'PURPLE_CONNECTION_ERROR_CERT_EXPIRED
errorCertNotActivated :: ConnectionError
errorCertNotActivated = c'PURPLE_CONNECTION_ERROR_CERT_NOT_ACTIVATED
errorCertHostnameMismatch :: ConnectionError
errorCertHostnameMismatch = c'PURPLE_CONNECTION_ERROR_CERT_HOSTNAME_MISMATCH
errorCertFingerprintMismatch :: ConnectionError
errorCertFingerprintMismatch = c'PURPLE_CONNECTION_ERROR_CERT_FINGERPRINT_MISMATCH
errorCertSelfSigned :: ConnectionError
errorCertSelfSigned = c'PURPLE_CONNECTION_ERROR_CERT_SELF_SIGNED
errorCertOtherError :: ConnectionError
errorCertOtherError = c'PURPLE_CONNECTION_ERROR_CERT_OTHER_ERROR
errorOtherError :: ConnectionError
errorOtherError = c'PURPLE_CONNECTION_ERROR_OTHER_ERROR

type ConnectionState = C'PurpleConnectionState
stateDisconnected :: ConnectionState
stateDisconnected = c'PURPLE_DISCONNECTED
stateConnected :: ConnectionState
stateConnected = c'PURPLE_CONNECTED
stateConnecting :: ConnectionState
stateConnecting = c'PURPLE_CONNECTING

type ConnectionFlags = C'PurpleConnectionFlags
connectionHtml :: ConnectionFlags
connectionHtml = c'PURPLE_CONNECTION_HTML
connectionNoBgcolor :: ConnectionFlags
connectionNoBgcolor = c'PURPLE_CONNECTION_NO_BGCOLOR
connectionAutoResp :: ConnectionFlags
connectionAutoResp = c'PURPLE_CONNECTION_AUTO_RESP
connectionFormattingWbfo :: ConnectionFlags
connectionFormattingWbfo = c'PURPLE_CONNECTION_FORMATTING_WBFO
connectionNoNewlines :: ConnectionFlags
connectionNoNewlines = c'PURPLE_CONNECTION_NO_NEWLINES
connectionNoFontsize :: ConnectionFlags
connectionNoFontsize = c'PURPLE_CONNECTION_NO_FONTSIZE
connectionNoUrldesc :: ConnectionFlags
connectionNoUrldesc = c'PURPLE_CONNECTION_NO_URLDESC
connectionNoImages :: ConnectionFlags
connectionNoImages = c'PURPLE_CONNECTION_NO_IMAGES
connectionAllowCustomSmiley :: ConnectionFlags
connectionAllowCustomSmiley = c'PURPLE_CONNECTION_ALLOW_CUSTOM_SMILEY

#integral_t PurpleConnectionError
#num PURPLE_CONNECTION_ERROR_NETWORK_ERROR
#num PURPLE_CONNECTION_ERROR_INVALID_USERNAME
#num PURPLE_CONNECTION_ERROR_AUTHENTICATION_FAILED
#num PURPLE_CONNECTION_ERROR_AUTHENTICATION_IMPOSSIBLE
#num PURPLE_CONNECTION_ERROR_NO_SSL_SUPPORT
#num PURPLE_CONNECTION_ERROR_ENCRYPTION_ERROR
#num PURPLE_CONNECTION_ERROR_NAME_IN_USE
#num PURPLE_CONNECTION_ERROR_INVALID_SETTINGS
#num PURPLE_CONNECTION_ERROR_CERT_NOT_PROVIDED
#num PURPLE_CONNECTION_ERROR_CERT_UNTRUSTED
#num PURPLE_CONNECTION_ERROR_CERT_EXPIRED
#num PURPLE_CONNECTION_ERROR_CERT_NOT_ACTIVATED
#num PURPLE_CONNECTION_ERROR_CERT_HOSTNAME_MISMATCH
#num PURPLE_CONNECTION_ERROR_CERT_FINGERPRINT_MISMATCH
#num PURPLE_CONNECTION_ERROR_CERT_SELF_SIGNED
#num PURPLE_CONNECTION_ERROR_CERT_OTHER_ERROR
#num PURPLE_CONNECTION_ERROR_OTHER_ERROR

#integral_t PurpleConnectionState
#num PURPLE_DISCONNECTED
#num PURPLE_CONNECTED
#num PURPLE_CONNECTING

#integral_t PurpleConnectionFlags
#num PURPLE_CONNECTION_HTML
#num PURPLE_CONNECTION_NO_BGCOLOR
#num PURPLE_CONNECTION_AUTO_RESP
#num PURPLE_CONNECTION_FORMATTING_WBFO
#num PURPLE_CONNECTION_NO_NEWLINES
#num PURPLE_CONNECTION_NO_FONTSIZE
#num PURPLE_CONNECTION_NO_URLDESC
#num PURPLE_CONNECTION_NO_IMAGES
#num PURPLE_CONNECTION_ALLOW_CUSTOM_SMILEY

#callback ConnectProgress           , Connection -> CString -> CInt -> CInt -> IO ()
#callback Connected                 , Connection -> IO ()
#callback Disconnected              , Connection -> IO ()
#callback Notice                    , Connection -> CString -> IO ()
#callback ReportDisconnect          , Connection -> CString -> IO ()
#callback ReportDisconnectReason    , Connection -> <PurpleConnectionError> -> CString -> IO ()
#callback NetworkConnected          , IO ()
#callback NetworkDisconnected       , IO ()

#starttype PurpleConnectionUiOps
#field connect_progress             , <ConnectProgress>
#field connected                    , <Connected>
#field disconnected                 , <Disconnected>
#field notice                       , <Notice>
#field report_disconnect            , <ReportDisconnect>
#field network_connected            , <NetworkConnected>
#field network_disconnected         , <NetworkDisconnected>
#field report_disconnect_reason     , <ReportDisconnectReason>
#field _purple_reserved1            , Ptr ()
#field _purple_reserved2            , Ptr ()
#field _purple_reserved3            , Ptr ()
#stoptype
