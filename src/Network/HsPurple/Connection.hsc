-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Connection
    (
    -- * ConnectionUiOps
      ConnectionUiOps (..)
    , ConnectionConnectProgress
    , ConnectionConnected
    , ConnectionDisconnected
    , ConnectionNotice
    , ConnectionNetworkConnected
    , ConnectionNetworkDisconnected
    , ConnectionReportDisconnectReason

    -- * Connection API
    , connectionIsConnected
    , connectionDestroy
    , connectionSetState
    , connectionSetAccount
    , connectionSetDisplayName
    , connectionSetProtocolData
    , connectionGetState
    , connectionGetAccount
    , connectionGetPlugin
    , connectionGetPassword
    , connectionGetDisplayName
    , connectionGetProtocolData
    -- , connectionUpdateProgress
    , connectionNotice
    , connectionError
    , connectionErrorReason
    -- , connectionSslError
    , connectionErrorIsFatal

    -- * Connections API
    , connectionIsValid
    , connectionsDisconnectAll
    , connectionsGetAll
    , connectionsGetConnecting

    -- * Enums
    , ConnectionError
    , connectionErrorNetworkError
    , connectionErrorInvalidUsername
    , connectionErrorAuthenticationFailed
    , connectionErrorAuthenticationImpossible
    , connectionErrorNoSslSupport
    , connectionErrorEncryptionError
    , connectionErrorNameInUse
    , connectionErrorInvalidSettings
    , connectionErrorCertNotProvided
    , connectionErrorCertUntrusted
    , connectionErrorCertExpired
    , connectionErrorCertNotActivated
    , connectionErrorCertHostnameMismatch
    , connectionErrorCertFingerprintMismatch
    , connectionErrorCertSelfSigned
    , connectionErrorCertOtherError
    , connectionErrorOtherError

    , ConnectionState
    , connectionStateDisconnected
    , connectionStateConnected
    , connectionStateConnecting

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

import Foreign
import Foreign.C
import Network.HsPurple.GLib.GList
import Network.HsPurple.UiOps.ConnectionUiOps hiding (ConnectionError, Connection, fi)

#include <purple.h>
#include <bindings.dsl.h>


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


--------------------------------------------------------------------------------
-- Connection API
--------------------------------------------------------------------------------

type Connection     = Ptr ()
type Account        = Ptr ()
type ProtoData      = Ptr ()
type Plugin         = Ptr ()
type ProtocolData   = Ptr ()

-- | Returns TRUE if the account is connected, otherwise returns FALSE. 
connectionIsConnected :: Connection -> IO Bool
connectionIsConnected c =  (connectionStateConnected ==) `fmap` connectionGetState c

#ccall purple_connection_destroy , Connection -> IO ()
-- | Disconnects and destroys a PurpleConnection. 
connectionDestroy :: Connection -> IO ()
connectionDestroy = c'purple_connection_destroy

#ccall purple_connection_set_state , Connection -> CInt -> IO ()
-- | Sets the connection state. 
connectionSetState :: Connection -> ConnectionState -> IO ()
connectionSetState c s = c'purple_connection_set_state c (fi s)

#ccall purple_connection_set_account , Connection -> Account -> IO ()
-- | Sets the connection's account. 
connectionSetAccount :: Connection -> Account -> IO ()
connectionSetAccount = c'purple_connection_set_account

#ccall purple_connection_set_display_name , Connection -> CString -> IO ()
-- | Sets the connection's displayed name. 
connectionSetDisplayName :: Connection -> String -> IO ()
connectionSetDisplayName c s = newCString s >>= c'purple_connection_set_display_name c

#ccall purple_connection_set_protocol_data , Connection -> Ptr () -> IO ()
-- | Sets the protocol data for a connection. 
connectionSetProtocolData :: Connection -> ProtoData -> IO ()
connectionSetProtocolData = c'purple_connection_set_protocol_data

#ccall purple_connection_get_state , Connection -> IO CInt
-- | Returns the connection state. 
connectionGetState :: Connection -> IO ConnectionState
connectionGetState = fmap fi . c'purple_connection_get_state

#ccall purple_connection_get_account , Connection -> IO Account
-- | Returns the connection's account. 
connectionGetAccount :: Connection -> IO Account
connectionGetAccount = c'purple_connection_get_account

#ccall purple_connection_get_prpl , Connection -> IO Plugin
-- | Returns the protocol plugin managing a connection. 
connectionGetPlugin :: Connection -> IO Plugin
connectionGetPlugin = c'purple_connection_get_prpl

#ccall purple_connection_get_password , Connection -> IO CString
-- | Returns the connection's password. 
connectionGetPassword :: Connection -> IO String
connectionGetPassword c = peekCString =<< c'purple_connection_get_password c

#ccall purple_connection_get_display_name , Connection -> IO CString
-- | Returns the connection's displayed name. 
connectionGetDisplayName :: Connection -> IO String
connectionGetDisplayName c = peekCString =<< c'purple_connection_get_display_name c

#ccall purple_connection_get_protocol_data , Connection -> IO ProtoData
-- | Gets the protocol data from a connection. 
connectionGetProtocolData :: Connection -> IO ProtocolData
connectionGetProtocolData = c'purple_connection_get_protocol_data

{-
#call purple_connection_update_progress , Ptr () -> CString -> CInt -> CInt -> IO ()
-- | Updates the connection progress. 
connectionUpdateProgress :: Connection
                         -> String      -- ^ Text
                         -> Int         -- ^ Step
                         -> Int         -- ^ Step count
                         -> IO ()
connectionUpdateProgress c s i1 i2 = do
    cs <- newCString s
    c'purple_connection_update_progress c cs (fi i1) (fi i2)
    -}

#ccall purple_connection_notice , Connection -> CString -> IO ()
-- | Displays a connection-specific notice. 
connectionNotice :: Connection -> String -> IO ()
connectionNotice c s = newCString s >>= c'purple_connection_notice c

#ccall purple_connection_error , Connection -> CString -> IO ()
-- | Closes a connection with an error. 
connectionError :: Connection
                -> String       -- ^ Reason
                -> IO ()
connectionError c s = newCString s >>= c'purple_connection_error c

#ccall purple_connection_error_reason , Connection -> CInt -> CString -> IO ()
-- | Closes a connection with an error and a human-readable description of the error. 
connectionErrorReason :: Connection
                      -> ConnectionError
                      -> String             -- ^ Description
                      -> IO ()
connectionErrorReason c ce s = do
    cs <- newCString s
    c'purple_connection_error_reason c (fi ce) cs

{-
#ccall purple_connection_ssl_error , Connection -> CInt -> IO ()
-- | Closes a connection due to an SSL error; this is basically a shortcut to
-- turning the PurpleSslErrorType into a PurpleConnectionError and a
-- human-readable string and then calling purple_connection_error_reason(). 
connectionSslError :: Connection -> SslErrorType -> IO ()
connectionSslError c s = c'purple_connection_ssl_error c (fi s)
-}

#ccall purple_connection_error_is_fatal , CInt -> IO CInt
-- | Reports whether a disconnection reason is fatal (in which case the account
-- should probably not be automatically reconnected) or transient (so
-- auto-reconnection is a good idea).
connectionErrorIsFatal :: ConnectionError -> IO Bool
connectionErrorIsFatal = fmap (1 ==) . c'purple_connection_error_is_fatal . fi



--------------------------------------------------------------------------------
-- Connections API
--------------------------------------------------------------------------------

-- | Checks if gc is still a valid pointer to a gc. 
connectionIsValid :: Connection -> IO Bool
connectionIsValid c = (c `elem`) `fmap` connectionsGetAll

#ccall purple_connections_disconnect_all , IO ()
-- | Disconnects from all connections. 
connectionsDisconnectAll :: IO ()
connectionsDisconnectAll = c'purple_connections_disconnect_all

#ccall purple_connections_get_all , IO (Ptr <GList>)
-- | Returns a list of all active connections. 
connectionsGetAll :: IO [Connection]
connectionsGetAll = c'purple_connections_get_all >>= gListToList

#ccall purple_connections_get_connecting , IO (Ptr <GList>)
-- | Returns a list of all connections in the process of connecting.
connectionsGetConnecting :: IO [Connection]
connectionsGetConnecting = c'purple_connections_get_connecting >>= gListToList



--------------------------------------------------------------------------------
-- Enumcs
--------------------------------------------------------------------------------

type ConnectionError = Int
connectionErrorNetworkError :: ConnectionError
connectionErrorNetworkError = c'PURPLE_CONNECTION_ERROR_NETWORK_ERROR
connectionErrorInvalidUsername :: ConnectionError
connectionErrorInvalidUsername = c'PURPLE_CONNECTION_ERROR_INVALID_USERNAME
connectionErrorAuthenticationFailed :: ConnectionError
connectionErrorAuthenticationFailed = c'PURPLE_CONNECTION_ERROR_AUTHENTICATION_FAILED
connectionErrorAuthenticationImpossible :: ConnectionError
connectionErrorAuthenticationImpossible = c'PURPLE_CONNECTION_ERROR_AUTHENTICATION_IMPOSSIBLE
connectionErrorNoSslSupport :: ConnectionError
connectionErrorNoSslSupport = c'PURPLE_CONNECTION_ERROR_NO_SSL_SUPPORT
connectionErrorEncryptionError :: ConnectionError
connectionErrorEncryptionError = c'PURPLE_CONNECTION_ERROR_ENCRYPTION_ERROR
connectionErrorNameInUse :: ConnectionError
connectionErrorNameInUse = c'PURPLE_CONNECTION_ERROR_NAME_IN_USE
connectionErrorInvalidSettings :: ConnectionError
connectionErrorInvalidSettings = c'PURPLE_CONNECTION_ERROR_INVALID_SETTINGS
connectionErrorCertNotProvided :: ConnectionError
connectionErrorCertNotProvided = c'PURPLE_CONNECTION_ERROR_CERT_NOT_PROVIDED
connectionErrorCertUntrusted :: ConnectionError
connectionErrorCertUntrusted = c'PURPLE_CONNECTION_ERROR_CERT_UNTRUSTED
connectionErrorCertExpired :: ConnectionError
connectionErrorCertExpired = c'PURPLE_CONNECTION_ERROR_CERT_EXPIRED
connectionErrorCertNotActivated :: ConnectionError
connectionErrorCertNotActivated = c'PURPLE_CONNECTION_ERROR_CERT_NOT_ACTIVATED
connectionErrorCertHostnameMismatch :: ConnectionError
connectionErrorCertHostnameMismatch = c'PURPLE_CONNECTION_ERROR_CERT_HOSTNAME_MISMATCH
connectionErrorCertFingerprintMismatch :: ConnectionError
connectionErrorCertFingerprintMismatch = c'PURPLE_CONNECTION_ERROR_CERT_FINGERPRINT_MISMATCH
connectionErrorCertSelfSigned :: ConnectionError
connectionErrorCertSelfSigned = c'PURPLE_CONNECTION_ERROR_CERT_SELF_SIGNED
connectionErrorCertOtherError :: ConnectionError
connectionErrorCertOtherError = c'PURPLE_CONNECTION_ERROR_CERT_OTHER_ERROR
connectionErrorOtherError :: ConnectionError
connectionErrorOtherError = c'PURPLE_CONNECTION_ERROR_OTHER_ERROR

type ConnectionState = Int
connectionStateDisconnected :: ConnectionState
connectionStateDisconnected = c'PURPLE_DISCONNECTED
connectionStateConnected :: ConnectionState
connectionStateConnected = c'PURPLE_CONNECTED
connectionStateConnecting :: ConnectionState
connectionStateConnecting = c'PURPLE_CONNECTING

type ConnectionFlags = Int
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

#num PURPLE_DISCONNECTED
#num PURPLE_CONNECTED
#num PURPLE_CONNECTING

#num PURPLE_CONNECTION_HTML
#num PURPLE_CONNECTION_NO_BGCOLOR
#num PURPLE_CONNECTION_AUTO_RESP
#num PURPLE_CONNECTION_FORMATTING_WBFO
#num PURPLE_CONNECTION_NO_NEWLINES
#num PURPLE_CONNECTION_NO_FONTSIZE
#num PURPLE_CONNECTION_NO_URLDESC
#num PURPLE_CONNECTION_NO_IMAGES
#num PURPLE_CONNECTION_ALLOW_CUSTOM_SMILEY
