{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Plugin
    (
      pluginsGetProtocols
    , pluginGetName
    , pluginGetId
    ) where

import Foreign
import Foreign.C

import Network.HsPurple.GLib.GList

#include <purple.h>
#include <bindings.dsl.h>

type Plugin = Ptr ()

#ccall purple_plugins_get_protocols , IO (Ptr <GList>)
-- | Returns a list of all valid protocol plugins. 
pluginsGetProtocols :: IO [Plugin]
pluginsGetProtocols = c'purple_plugins_get_protocols >>= gListToList

#ccall purple_plugin_get_name , Plugin -> IO CString
-- | Returns a plugin's name.
pluginGetName :: Plugin -> IO String
pluginGetName p = c'purple_plugin_get_name p >>= peekCString

#ccall purple_plugin_get_id , Plugin -> IO CString
-- | Returns a plugin's id.
pluginGetId :: Plugin -> IO String
pluginGetId p = c'purple_plugin_get_id p >>= peekCString
