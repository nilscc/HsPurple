{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.SslConn
    (
    -- * Subsystem API
      initSsl
    , uninitSsl
    , sslSetOps
    , sslGetOps

    -- * SSL API
    , sslIsSupported
    , sslStrError
    , sslConnect
    , sslConnectWithSslCn
    , sslConnectFd
    , sslConnectWithHostFd
    , sslInputAdd
    , sslClose
    , sslRead
    , sslWrite
    , sslGetPeerCertificates

    -- * Function types
    , SslInputFunction
    , SslErrorFunction

    -- * Enums
    , SslErrorType
    , sslHandshakeFailed
    , sslConnectFailed
    , sslCertificateInvalid
    ) where

import Foreign
import Foreign.C
import Network.HsPurple.GLib.GList
import System.Posix

#include <purple.h>
#include <bindings.dsl.h>

type SslOps = Ptr () -- TODO!
type SslConnection = Ptr ()
type UserData = Ptr ()
type Account = Ptr ()
type Cert = Ptr ()
type Buffer = Ptr ()
type InputCondition = Int



--------------------------------------------------------------------------------
-- Subsystem API
--------------------------------------------------------------------------------

#ccall purple_ssl_init , IO ()
-- | Initializes the SSL subsystem. 
initSsl :: IO ()
initSsl = c'purple_ssl_init

#ccall purple_ssl_uninit , IO ()
-- | Uninitializes the SSL subsystem. 
uninitSsl :: IO ()
uninitSsl = c'purple_ssl_uninit

#ccall purple_ssl_set_ops , SslOps -> IO ()
-- | Sets the current SSL operations structure. 
sslSetOps :: SslOps -> IO ()
sslSetOps = c'purple_ssl_set_ops

#ccall purple_ssl_get_ops , IO SslOps
-- | Returns the current SSL operations structure. 
sslGetOps :: IO SslOps
sslGetOps = c'purple_ssl_get_ops


--------------------------------------------------------------------------------
-- SSL API
--------------------------------------------------------------------------------

#ccall purple_ssl_is_supported , IO CInt
-- | Returns whether or not SSL is currently supported. 
sslIsSupported :: IO Bool
sslIsSupported = fmap (1 ==) c'purple_ssl_is_supported

#ccall purple_ssl_strerror , CInt -> IO CString
-- | Returns a human-readable string for an SSL error. 
sslStrError :: SslErrorType -> IO String
sslStrError et = c'purple_ssl_strerror (fromIntegral et) >>= peekCString

{-
00045 typedef void (*PurpleSslInputFunction)(gpointer, PurpleSslConnection *,
00046                                      PurpleInputCondition);
00047 typedef void (*PurpleSslErrorFunction)(PurpleSslConnection *, PurpleSslErrorType,
00048                                      gpointer);
-}
#callback SslInputFunction , Ptr () -> SslConnection -> InputCondition -> IO ()
#callback SslErrorFunction , SslConnection -> SslErrorType -> Ptr () -> IO ()

type SslInputFunction   = Ptr () -> SslConnection -> InputCondition -> IO ()
type SslErrorFunction   = SslConnection -> SslErrorType -> Ptr () -> IO ()

#ccall purple_ssl_connect , Account -> CString -> CInt -> <SslInputFunction> -> <SslErrorFunction> -> Ptr () -> IO SslConnection
-- | Makes a SSL connection to the specified host and port. 
sslConnect :: Account
           -> String        -- ^ Host
           -> Int           -- ^ Port
           -> SslInputFunction
           -> SslErrorFunction
           -> UserData
           -> IO SslConnection
sslConnect a s i inf errf ud = do
    cs <- newCString s
    infP <- mk'SslInputFunction inf
    errfP <- mk'SslErrorFunction errf
    c'purple_ssl_connect a cs (fromIntegral i) infP errfP ud

#ccall purple_ssl_connect_with_ssl_cn , Account -> CString -> CInt -> <SslInputFunction> -> <SslErrorFunction> -> CString -> Ptr () -> IO SslConnection
-- | Makes a SSL connection to the specified host and port, using the separate
-- name to verify with the certificate. 
sslConnectWithSslCn :: Account
                    -> String        -- ^ Host
                    -> Int           -- ^ Port
                    -> SslInputFunction
                    -> SslErrorFunction
                    -> String        -- ^ SSL host
                    -> UserData
                    -> IO SslConnection
sslConnectWithSslCn a s1 i inf errf s2 ud = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    infP <- mk'SslInputFunction inf
    errfP <- mk'SslErrorFunction errf
    c'purple_ssl_connect_with_ssl_cn a cs1 (fromIntegral i) infP errfP cs2 ud

#ccall purple_ssl_connect_fd , Account -> CInt -> <SslInputFunction> -> <SslErrorFunction> -> UserData -> IO SslConnection
-- | Makes a SSL connection using an already open file descriptor. 
sslConnectFd :: Account -> Fd -> SslInputFunction -> SslErrorFunction -> UserData -> IO SslConnection
sslConnectFd a (Fd i) inf errf ud = do
    infP <- mk'SslInputFunction inf
    errfP <- mk'SslErrorFunction errf
    c'purple_ssl_connect_fd a (fromIntegral i) infP errfP ud

#ccall purple_ssl_connect_with_host_fd , Account -> CInt -> <SslInputFunction> -> <SslErrorFunction> -> CString -> UserData -> IO ()
-- | Makes a SSL connection using an already open file descriptor. 
sslConnectWithHostFd :: Account
                     -> Fd
                     -> SslInputFunction
                     -> SslErrorFunction
                     -> String              -- ^ Host
                     -> UserData
                     -> IO ()
sslConnectWithHostFd a (Fd i) inf errf s ud = do
    cs <- newCString s
    infP <- mk'SslInputFunction inf
    errfP <- mk'SslErrorFunction errf
    c'purple_ssl_connect_with_host_fd a (fromIntegral i) infP errfP cs ud

#ccall purple_ssl_input_add , SslConnection -> <SslInputFunction> -> UserData -> IO ()
-- | Adds an input watcher for the specified SSL connection. 
sslInputAdd :: SslConnection -> SslInputFunction -> UserData -> IO ()
sslInputAdd sc inf ud = do
    infP <- mk'SslInputFunction inf
    c'purple_ssl_input_add sc infP ud

#ccall purple_ssl_close , SslConnection -> IO ()
-- | Closes a SSL connection. 
sslClose :: SslConnection -> IO ()
sslClose = c'purple_ssl_close

#ccall purple_ssl_read , SslConnection -> Buffer -> CInt -> IO CInt
-- | Reads data from an SSL connection. 
sslRead :: SslConnection
        -> Buffer
        -> Int              -- ^ Length
        -> IO Int
sslRead sc b i = fromIntegral `fmap` c'purple_ssl_read sc b (fromIntegral i)

#ccall purple_ssl_write , SslConnection -> Buffer -> CInt -> IO CInt
-- | Writes data to an SSL connection. 
sslWrite :: SslConnection
         -> Buffer
         -> Int             -- ^ Length
         -> IO Int
sslWrite sc b i = fromIntegral `fmap` c'purple_ssl_write sc b (fromIntegral i)

#ccall purple_ssl_get_peer_certificates , SslConnection -> IO (Ptr <GList>)
-- | Obtains the peer's presented certificates. 
sslGetPeerCertificates :: SslConnection -> IO [Cert]
sslGetPeerCertificates sc = c'purple_ssl_get_peer_certificates sc >>= gListToList



--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

type SslErrorType = Int

#num PURPLE_SSL_HANDSHAKE_FAILED
#num PURPLE_SSL_CONNECT_FAILED
#num PURPLE_SSL_CERTIFICATE_INVALID

sslHandshakeFailed    :: SslErrorType
sslConnectFailed      :: SslErrorType
sslCertificateInvalid :: SslErrorType

sslHandshakeFailed    = c'PURPLE_SSL_HANDSHAKE_FAILED
sslConnectFailed      = c'PURPLE_SSL_CONNECT_FAILED
sslCertificateInvalid = c'PURPLE_SSL_CERTIFICATE_INVALID
