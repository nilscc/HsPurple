{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.EventLoop
    (
    -- * Types
      InputCondition (..)
    , EventLoopUiOps (..)

    -- * Function aliases
    , TimeoutAdd
    , TimeoutAddSeconds
    , TimeoutRemove
    , InputAdd
    , InputRemove
    , InputGetError

    -- * Event loop api
    , inputAdd
    , inputRemove

    , timeoutAdd
    , timeoutAddSeconds
    , timeoutRemove

    , getError
    
    -- * UI Registration Functions
    , setUiOps
    , getUiOps

    ) where

import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Posix



--------------------------------------------------------------------------------
-- Data/type definitions
--------------------------------------------------------------------------------

{- C header:

guint purple_input_add(int fd, PurpleInputCondition cond,
                       PurpleInputFunction func, gpointer user_data);

gboolean purple_input_remove(guint handle);

-}

data InputCondition = InputRead
                    | InputWrite

type Handle   = Int
type Error    = Int
type UserData = Ptr ()

-- From Bindings.GLib: @type C'GSourceFunc = FunPtr (C'gpointer -> IO C'gboolean)@
type CGSourceFunc = UserData -> IO CInt
type GSourceFunc = UserData -> IO Int

-- | The type of callbacks to handle events on file descriptors, as passed to
-- purple_input_add().  The callback will receive the user_data passed to
-- purple_input_add(), the file descriptor on which the event occurred, and the
-- condition that was satisfied to cause the callback to be invoked.
type CInputFunc = UserData -> CInt -> CInt -> IO ()
type InputFunc = UserData -> Int -> Int -> IO ()

type CBool = CInt

-- | C function types
type CTimeoutAdd         = CInt -> FunPtr CGSourceFunc -> UserData -> IO CInt
type CTimeoutAddSeconds  = CInt -> FunPtr CGSourceFunc -> UserData -> IO CInt
type CTimeoutRemove      = CInt -> IO CBool
type CInputAdd           = CInt -> CInt -> FunPtr CInputFunc -> UserData -> IO CInt
type CInputRemove        = CInt -> IO CBool
type CInputGetError      = CInt -> Ptr CInt -> IO CInt

-- | Haskell function types
type TimeoutAdd         = Int -> GSourceFunc -> UserData -> IO Int
type TimeoutAddSeconds  = Int -> GSourceFunc -> UserData -> IO Int
type TimeoutRemove      = Int -> IO Bool
type InputAdd           = Int -> Int -> InputFunc -> UserData -> IO Int
type InputRemove        = Int -> IO Bool
type InputGetError      = Int -> Ptr CInt -> IO Int



--------------------------------------------------------------------------------
-- The struct
--------------------------------------------------------------------------------

-- | PurpleEventLoopUiOps struct representation
data EventLoopUiOps = EventLoopUiOps
    { timeout_add           :: TimeoutAdd
    , timeout_add_seconds   :: TimeoutAddSeconds
    , timeout_remove        :: TimeoutRemove
    , input_add             :: InputAdd
    , input_remove          :: InputRemove
    , input_get_error       :: InputGetError
    }



--------------------------------------------------------------------------------
-- C type functions to Haskell type functions
--------------------------------------------------------------------------------

cGSourceFunc :: CGSourceFunc -> GSourceFunc
cGSourceFunc f = fmap fi . f

cInputFunc :: CInputFunc -> InputFunc
cInputFunc f = \u ci1 ci2 -> f u (fi ci1) (fi ci2)



--------------------------------------------------------------------------------
-- Haskell type functions to C type functions
--------------------------------------------------------------------------------

hGSourceFunc :: GSourceFunc -> CGSourceFunc
hGSourceFunc f = fmap fi . f

hInputFunc :: InputFunc -> CInputFunc
hInputFunc f = \u ci1 ci2 -> f u (fi ci1) (fi ci2)

hTimeoutAdd :: TimeoutAdd -> CTimeoutAdd
hTimeoutAdd f = \ci cgs ud ->
    fi `fmap` f (fi ci) (cGSourceFunc $ c_get_gsourcefunc cgs) ud

hTimeoutAddSeconds :: TimeoutAddSeconds -> CTimeoutAddSeconds
hTimeoutAddSeconds f = \ci cgs ud ->
    fi `fmap` f (fi ci) (cGSourceFunc $ c_get_gsourcefunc cgs) ud

hTimeoutRemove :: TimeoutRemove -> CTimeoutRemove
hTimeoutRemove f = \ci ->
    (\b -> if b then 1 else 0) `fmap` f (fi ci)

hInputAdd :: InputAdd -> CInputAdd
hInputAdd f = \ci1 ci2 cif ud ->
    fi `fmap` f (fi ci1) (fi ci2) (cInputFunc $ c_get_input_func cif) ud

hInputRemove :: InputRemove -> CInputRemove
hInputRemove f = \ci ->
    (\b -> if b then 1 else 0) `fmap` f (fi ci)

hInputGetError :: InputGetError -> CInputGetError
hInputGetError f = \ci ptr ->
    fi `fmap` f (fi ci) ptr




--------------------------------------------------------------------------------
-- FunPtr to Fun
--------------------------------------------------------------------------------

-- create a InputFunc function pointer
foreign import ccall "dynamic"
    c_get_input_func                 :: FunPtr CInputFunc -> CInputFunc

-- create a GSourceFunc function pointer
foreign import ccall "dynamic"
    c_get_gsourcefunc                :: FunPtr CGSourceFunc -> CGSourceFunc



--------------------------------------------------------------------------------
-- FunPtr generators
--------------------------------------------------------------------------------

-- create a InputFunc function pointer
foreign import ccall "wrapper"
    c_mk_input_func                 :: CInputFunc -> IO (FunPtr CInputFunc)

-- create a GSourceFunc function pointer
foreign import ccall "wrapper"
    c_mk_gsourcefunc                :: CGSourceFunc -> IO (FunPtr CGSourceFunc)

foreign import ccall "wrapper"
    c_mk_timeout_add_funptr         :: CTimeoutAdd -> IO (FunPtr CTimeoutAdd)

foreign import ccall "wrapper"
    c_mk_timeout_add_seconds_funptr :: CTimeoutAddSeconds -> IO (FunPtr CTimeoutAddSeconds)

foreign import ccall "wrapper"
    c_mk_timeout_remove_funptr      :: CTimeoutRemove -> IO (FunPtr CTimeoutRemove)

foreign import ccall "wrapper"
    c_mk_input_add_funptr           :: CInputAdd -> IO (FunPtr CInputAdd)

foreign import ccall "wrapper"
    c_mk_input_remove_funptr        :: CInputRemove -> IO (FunPtr CInputRemove)

foreign import ccall "wrapper"
    c_mk_input_get_error_funptr     :: CInputGetError -> IO (FunPtr CInputGetError)



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
    c_set_ui_ops :: Ptr EventLoopUiOps -> IO ()

foreign import ccall "purple_eventloop_get_ui_ops"
    c_get_ui_ops :: IO (Ptr EventLoopUiOps)



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
    error "Not implemented"
    -- ptr     <- malloc
    -- poke ptr eventLoopUiOps
    -- c_set_ui_ops ptr

-- | Returns the UI operations structure used for accounts.
getUiOps :: IO (Ptr EventLoopUiOps)
getUiOps = c_get_ui_ops



--------------------------------------------------------------------------------
-- EventLoopUiOps structure instance
--------------------------------------------------------------------------------

{- Structure of the _PurpleEventLoopUiOps:

-- Should create a callback timer with an interval measured in milliseconds. 
guint(* 	timeout_add )(guint interval, GSourceFunc function, gpointer data)

-- Should remove a callback timer. 
gboolean(* 	timeout_remove )(guint handle)

-- Should add an input handler. 
guint(* 	input_add )(int fd, PurpleInputCondition cond, PurpleInputFunction func, gpointer user_data)

-- Should remove an input handler. 
gboolean(* 	input_remove )(guint handle)

-- If implemented, should get the current error status for an input. 
int(* 	input_get_error )(int fd, int *error)

-- If implemented, should create a callback timer with an interval measured in seconds. 
guint(* 	timeout_add_seconds )(guint interval, GSourceFunc function, gpointer data)

void(* 	_purple_reserved2 )(void)
void(* 	_purple_reserved3 )(void)
void(* 	_purple_reserved4 )(void)

-}

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

instance Storable EventLoopUiOps where
    sizeOf _ = #size struct _PurpleEventLoopUiOps
    alignment _ = #alignof struct _PurpleEventLoopUiOps
    peek ptr = error "peek EventLoopUiOps: Not implemented"
    poke ptr x = do

        t_add   <- c_mk_timeout_add_funptr          . hTimeoutAdd        $ timeout_add x
        t_add_s <- c_mk_timeout_add_seconds_funptr  . hTimeoutAddSeconds $ timeout_add_seconds x
        t_rem   <- c_mk_timeout_remove_funptr       . hTimeoutRemove     $ timeout_remove x

        i_add   <- c_mk_input_add_funptr            . hInputAdd          $ input_add x
        i_rem   <- c_mk_input_remove_funptr         . hInputRemove       $ input_remove x
        i_err   <- c_mk_input_get_error_funptr      . hInputGetError     $ input_get_error x

        (#poke struct _PurpleEventLoopUiOps, timeout_add)            ptr t_add
        (#poke struct _PurpleEventLoopUiOps, timeout_add_seconds)    ptr t_add_s
        (#poke struct _PurpleEventLoopUiOps, timeout_remove)         ptr t_rem

        (#poke struct _PurpleEventLoopUiOps, input_add)              ptr i_add
        (#poke struct _PurpleEventLoopUiOps, input_remove)           ptr i_rem
        (#poke struct _PurpleEventLoopUiOps, input_get_error)        ptr i_err

        (#poke struct _PurpleEventLoopUiOps, _purple_reserved2)      ptr ((#const NULL) :: CInt)
        (#poke struct _PurpleEventLoopUiOps, _purple_reserved3)      ptr ((#const NULL) :: CInt)
        (#poke struct _PurpleEventLoopUiOps, _purple_reserved4)      ptr ((#const NULL) :: CInt)

-- vim: ft=haskell
