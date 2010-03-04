-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Core
    (
    -- * Core functions
      coreInit
    , coreQuit
    , coreQuitCb
    , coreGetVersion
    , coreGetUi
    , coreGetUiInfo
    , getCore
    , coreEnsureSingleInstance
    
    -- * Setting/getting CoreUiOps
    , CoreUiOps (..)
    , setCoreUiOps
    , getCoreUiOps

    -- * Function types
    , CoreUiPrefsInit
    , CoreDebugUiInit
    , CoreUiInit
    , CoreQuit
    , CoreGetUiInfo
    ) where

import Foreign
import Foreign.C

import Network.HsPurple.UiOps.CoreUiOps
import Network.HsPurple.GLib.GHashTable

import qualified Data.Map as M

#include <purple.h>
#include <bindings.dsl.h>

type Core     = Ptr ()
type UserData = Ptr ()

#ccall purple_core_init                     , CString -> IO CInt
#ccall purple_core_quit                     , IO ()
#ccall purple_core_quit_cb                  , Ptr () -> IO CInt
#ccall purple_core_get_version              , IO CString
#ccall purple_core_get_ui                   , IO CString
#ccall purple_get_core                      , IO Core
#ccall purple_core_set_ui_ops               , Ptr <PurpleCoreUiOps> -> IO ()
#ccall purple_core_get_ui_ops               , IO (Ptr <PurpleCoreUiOps>)
#ccall purple_core_ensure_single_instance   , IO CInt
#ccall purple_core_get_ui_info              , IO (Ptr <GHashTable>)

coreInit :: String -> IO Bool
coreInit s = do
    cs <- newCString s
    (1 ==) `fmap` c'purple_core_init cs

coreQuit :: IO ()
coreQuit = c'purple_core_quit

coreQuitCb :: UserData -> IO Bool
coreQuitCb ptr = (1 ==) `fmap` c'purple_core_quit_cb ptr

coreGetVersion :: IO String
coreGetVersion = c'purple_core_get_version >>= peekCString

coreGetUi :: IO String
coreGetUi = c'purple_core_get_ui >>= peekCString

getCore :: IO Core
getCore = c'purple_get_core

setCoreUiOps :: CoreUiOps -> IO ()
setCoreUiOps cui = do
    ptr <- malloc
    poke ptr cui
    c'purple_core_set_ui_ops (castPtr ptr)

getCoreUiOps :: IO CoreUiOps
getCoreUiOps = do
    ptr <- c'purple_core_get_ui_ops
    peek (castPtr ptr)

coreEnsureSingleInstance :: IO Bool
coreEnsureSingleInstance = (1 ==) `fmap` c'purple_core_ensure_single_instance

coreGetUiInfo :: IO (M.Map String String)
coreGetUiInfo = do
    ptr <- c'purple_core_get_ui_info
    gHasHTableToStringMap ptr
