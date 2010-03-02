-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module HsPurple.Structs.ProxyInfo
    ( ProxyInfo (..)
    ) where

import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

data ProxyInfo = ProxyInfo
    { proxyType     :: ProxyType
    , proxyHost     :: Hostname
    , proxyPort     :: Port
    , proxyUsername :: String
    , proxyPassword :: String
    }

type Port = Int
type Hostname = String

data ProxyType
    = ProxyUseGlobal
    | ProxyNone
    | ProxyHTTP
    | ProxySocks4
    | ProxySocks5
    | ProxyUseEnvvar


--------------------------------------------------------------------------------
-- Storable instances
--------------------------------------------------------------------------------

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

instance Storable ProxyInfo where
    sizeOf _    = #size    PurpleProxyInfo
    alignment _ = #alignof PurpleProxyInfo
    peek ptr = do

        (cType :: CInt) <- (#peek PurpleProxyInfo, type) ptr
        port    <- (#peek PurpleProxyInfo, port) ptr
        host    <- (#peek PurpleProxyInfo, host) ptr >>= peekCString
        user    <- (#peek PurpleProxyInfo, username) ptr >>= peekCString
        pwd     <- (#peek PurpleProxyInfo, password) ptr >>= peekCString

        let pType = case cType of
                         (-1) -> ProxyUseGlobal
                         0    -> ProxyNone
                         1    -> ProxyHTTP
                         2    -> ProxySocks4
                         3    -> ProxySocks5
                         4    -> ProxyUseEnvvar

        return $ ProxyInfo pType host ((fromIntegral :: CInt -> Int) port) user pwd

    poke ptr (ProxyInfo pType host port user pwd) = do

        cHost <- newCString host
        cUser <- newCString user
        cPwd  <- newCString pwd

        let cType = case pType of
                         ProxyUseGlobal -> (-1)
                         ProxyNone      -> 0
                         ProxyHTTP      -> 1
                         ProxySocks4    -> 2
                         ProxySocks5    -> 3
                         ProxyUseEnvvar -> 4

        (#poke PurpleProxyInfo, type)        ptr (cType :: CInt)
        (#poke PurpleProxyInfo, port)        ptr ((fromIntegral :: Int -> CInt) port)
        (#poke PurpleProxyInfo, host)        ptr cHost
        (#poke PurpleProxyInfo, username)    ptr cUser
        (#poke PurpleProxyInfo, password)    ptr cPwd
