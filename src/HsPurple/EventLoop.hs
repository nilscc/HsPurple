{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.EventLoop
    (
    -- * Event loop api
      inputAdd
    , inputRemove

    , timeoutAdd
    , timeoutAddSeconds
    , timeoutRemove

    , getError
    
    -- * UI Registration Functions
    , setUiOps
    , getUiOps

    -- * Types
    , EventLoopUiOps (..)

    ) where

import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Posix


import HsPurple.Structs.EventLoopUiOps


type UserData = Ptr ()
type Handle = Int


--------------------------------------------------------------------------------
-- Purple functions: Foreign imports
--------------------------------------------------------------------------------

foreign import ccall "purple_input_add"
    c_input_add             :: CInputAdd

foreign import ccall "purple_input_remove"
    c_input_remove          :: CInputRemove

foreign import ccall "purple_timeout_add"
    c_timeout_add           :: CTimeoutAdd

foreign import ccall "purple_timeout_add_seconds"
    c_timeout_add_seconds   :: CTimeoutAddSeconds

foreign import ccall "purple_timeout_remove"
    c_timeout_remove        :: CTimeoutRemove

foreign import ccall "purple_input_get_error"
    c_get_error             :: CInputGetError

foreign import ccall "purple_eventloop_set_ui_ops"
    c_set_ui_ops            :: Ptr EventLoopUiOps -> IO ()

foreign import ccall "purple_eventloop_get_ui_ops"
    c_get_ui_ops            :: IO (Ptr EventLoopUiOps)



--------------------------------------------------------------------------------
-- Purple functions: Haskell representations
--------------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Add an input handler
inputAdd :: Fd -> InputCondition -> InputFunc -> UserData -> IO Int
inputAdd (Fd fd) cond func ud = do

    cf <- c_mk_input_func (hInputFunc func)
    fi `fmap` c_input_add fd cond' cf ud

  where cond' = case cond of
                     InputRead  -> 1 -- 1 << 0
                     InputWrite -> 2 -- 1 << 1

-- | Remove a input handler
inputRemove :: Handle -> IO Bool
inputRemove h =

    (== 1) `fmap` c_input_remove (fi h)

-- | Creates a callback timer.
timeoutAdd :: Int -> GSourceFunc -> UserData -> IO Int
timeoutAdd interval func ud = do

    cf <- c_mk_gsourcefunc (hGSourceFunc func)
    fi `fmap` c_timeout_add (fi interval) cf ud

-- | Creates a callback timer.
timeoutAddSeconds :: Int -> GSourceFunc -> UserData -> IO Int
timeoutAddSeconds interval func ud = do

    cf <- c_mk_gsourcefunc (hGSourceFunc func)
    fi `fmap` c_timeout_add_seconds (fi interval) cf ud


-- | Removes a timeout handler.
timeoutRemove :: Handle -> IO Bool
timeoutRemove h =

    (== 1) `fmap` c_timeout_remove (fi h)

-- | Get the current error status for an input.
getError :: Fd -> Ptr CInt -> IO Int
getError (Fd fd) errPtr =

    fi `fmap` c_get_error (fi fd) errPtr

-- | Sets the UI operations structure to be used for accounts.
setUiOps :: EventLoopUiOps -> IO ()
setUiOps eventLoopUiOps = do
    ptr     <- malloc
    poke ptr eventLoopUiOps
    c_set_ui_ops ptr

-- | Returns the UI operations structure used for accounts.
getUiOps :: IO (Ptr EventLoopUiOps)
getUiOps = c_get_ui_ops
