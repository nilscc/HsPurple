{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Blist
    (
    -- * Buddy List Subsystem
      initBlist
    , uninitBlist
    , blistGetHandle

    -- Buddy List API
    , blistNew
    , setBlist
    ) where

import Foreign
import Foreign.C

#include <purple.h>
#include <bindings.dsl.h>


type BuddyList = Ptr ()
type UIHandle = Ptr ()

--------------------------------------------------------------------------------
-- Buddy List Subsystem
--------------------------------------------------------------------------------

#ccall purple_blist_init , IO ()
-- | Initializes the buddy list subsystem. 
initBlist :: IO ()
initBlist = c'purple_blist_init

#ccall purple_blist_uninit , IO ()
-- | Uninitializes the buddy list subsystem.
uninitBlist :: IO ()
uninitBlist = c'purple_blist_uninit


#ccall purple_blist_get_handle , IO UIHandle
-- | Returns the handle for the buddy list subsystem. 
blistGetHandle :: IO UIHandle
blistGetHandle = c'purple_blist_get_handle

--------------------------------------------------------------------------------
-- Buddy List API
--------------------------------------------------------------------------------

#ccall purple_blist_new , IO BuddyList
-- | Creates a new buddy list. 
blistNew :: IO BuddyList
blistNew = c'purple_blist_new

#ccall purple_set_blist , BuddyList -> IO ()
-- | Sets the main buddy list.
setBlist :: BuddyList -> IO ()
setBlist = c'purple_set_blist

