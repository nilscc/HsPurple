-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}
 
module Network.HsPurple.GLib.GList where

import Foreign

#include <bindings.dsl.h>
#include <glib.h>

#starttype GList
#field data , Ptr ()
#field next , Ptr <GList>
#field prev , Ptr <GList>
#stoptype

-- Very basic bindings

#ccall g_list_append , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)
#ccall g_list_prepend , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)
