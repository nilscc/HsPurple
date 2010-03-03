{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Core
    (
    -- * Lowlevel core functions, incomplete
      init
    , quit
    , quitCb

    , ensureSingleInstance

    , getUi
    , getVersion
    ) where

import Prelude hiding (init)
import Foreign
import Foreign.C

--------------------------------------------------------------------------------
-- Foreign imports
--------------------------------------------------------------------------------

foreign import ccall "purple_core_init"
    c_init :: CString -> IO CInt

foreign import ccall "purple_core_quit"
    c_quit :: IO ()

foreign import ccall "purple_core_ensure_single_instance"
    c_ensure_single_instance :: IO CInt

foreign import ccall "purple_core_get_version"
    c_get_version :: IO CString

foreign import ccall "purple_core_get_ui"
    c_get_ui :: IO CString


type UI = String


--------------------------------------------------------------------------------
-- Haskell functions
--------------------------------------------------------------------------------

-- | Initializes the core of purple.
-- This will setup preferences for all the core subsystems.
init :: UI -> IO Bool
init ui = do
    cs <- newCString ui
    cb <- c_init cs
    return (cb == 1)

-- | Quit
quit :: IO ()
quit = c_quit

quitCb :: a -> IO Bool
quitCb _ = const False `fmap` quit

-- | Ensures that only one instance is running.
-- If libpurple is built with D-Bus support, this checks if another process owns
-- the libpurple bus name and if so whether that process is using the same
-- configuration directory as this process.
-- Returns True if this is the first instance of libpurple running; False if
-- there is another instance running.
ensureSingleInstance :: IO Bool
ensureSingleInstance = (1 ==) `fmap` c_ensure_single_instance

-- | Returns the version of the core libpurple library.
getVersion :: IO String
getVersion = c_get_version >>= peekCString

-- | Returns the ID of the UI that is using the core, as passed to
-- purple_core_init().
getUi :: IO UI
getUi = c_get_ui >>= peekCString
