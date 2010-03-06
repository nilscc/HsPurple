-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.EventLoopUiOps where

import Data.Monoid
import Foreign
import Foreign.C
import System.Posix
import System.Event hiding (intToEvent)


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


--------------------------------------------------------------------------------
-- The struct
--------------------------------------------------------------------------------

-- | PurpleEventLoopUiOps struct representation
data EventLoopUiOps = EventLoopUiOps
    { timeout_add           :: EventTimeoutAdd
    , timeout_add_seconds   :: EventTimeoutAddSeconds
    , timeout_remove        :: EventTimeoutRemove
    , input_add             :: EventInputAdd
    , input_remove          :: EventInputRemove
    -- , input_get_error       :: EventInputGetError
    }



--------------------------------------------------------------------------------
-- Data/type definitions
--------------------------------------------------------------------------------

type EventId  = Int
type CBool    = CInt
type UserData = Ptr ()
type ErrPtr   = Ptr CInt

-- From Bindings.GLib: @type C'GSourceFunc = FunPtr (C'gpointer -> IO C'gboolean)@
type CGSourceFunc = UserData -> IO CInt
type EventGSourceFunc  = UserData -> IO Bool

-- | The type of callbacks to handle events on file descriptors, as passed to
-- purple_input_add().  The callback will receive the user_data passed to
-- purple_input_add(), the file descriptor on which the event occurred, and the
-- condition that was satisfied to cause the callback to be invoked.
type CInputFunc = UserData -> CInt -> CInt -> IO ()
type EventInputFunc  = UserData -> EventId -> Event -> IO ()



-- | C function types
type CTimeoutAdd        = CUInt -> FunPtr CGSourceFunc -> UserData -> IO CUInt
type CTimeoutAddSeconds = CUInt -> FunPtr CGSourceFunc -> UserData -> IO CUInt
type CInputAdd          = CInt -> CInt -> FunPtr CInputFunc -> UserData -> IO CUInt
type CTimeoutRemove     = CInt -> IO CBool
type CInputRemove       = CInt -> IO CBool
type CInputGetError     = CInt -> ErrPtr -> IO CInt

-- | Haskell function types
type EventTimeoutAdd         = Int -> EventGSourceFunc -> UserData -> IO EventId
type EventTimeoutAddSeconds  = Int -> EventGSourceFunc -> UserData -> IO EventId
type EventInputAdd           = Fd  -> Event -> EventInputFunc -> UserData -> IO EventId
type EventTimeoutRemove      = EventId -> IO Bool
type EventInputRemove        = EventId -> IO Bool
type EventInputGetError      = Fd  -> ErrPtr -> IO Errno



--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

intToEvent :: Int -> Event
intToEvent 1 = evtRead
intToEvent 2 = evtWrite
intToEvent 3 = mconcat [evtRead, evtWrite]
intToEvent _ = mconcat []

eventToInt :: Event -> Int
eventToInt ev
    | ev == evtRead  = 1
    | ev == evtWrite = 2
    | ev == mconcat [evtRead, evtWrite] = 3
    | otherwise = 0


--------------------------------------------------------------------------------
-- C type functions to Haskell type functions
--------------------------------------------------------------------------------

cGSourceFunc :: CGSourceFunc -> EventGSourceFunc
cGSourceFunc f = fmap (1==) . f

cInputFunc :: CInputFunc -> EventInputFunc
cInputFunc f = \u i ev -> f u (fi i) (fi $ eventToInt ev)

cTimeoutAdd :: CTimeoutAdd -> EventTimeoutAdd
cTimeoutAdd f = \cui fp ud -> do
    gs <- c_mk_gsource_func $ hGSourceFunc fp
    fi `fmap` f (fi cui) gs ud

cTimeoutAddSeconds :: CTimeoutAddSeconds -> EventTimeoutAddSeconds
cTimeoutAddSeconds f = \cui fp ud -> do
    gs <- c_mk_gsource_func $ hGSourceFunc fp
    fi `fmap` f (fi cui) gs ud

cTimeoutRemove :: CTimeoutRemove -> EventTimeoutRemove
cTimeoutRemove f = \i -> (1 ==) `fmap` f (fi i)

cInputAdd :: CInputAdd -> EventInputAdd
cInputAdd f = \(Fd i) c inf ud -> do
    fp <- c_mk_input_func $ hInputFunc inf
    fi `fmap` f (fi i) (fi $ eventToInt c) fp ud

cInputRemove :: CInputRemove -> EventInputRemove
cInputRemove f = fmap ((1 :: CInt) ==) . f . fi

cInputGetError :: CInputGetError -> EventInputGetError
cInputGetError f = \(Fd i) ptr -> Errno `fmap` f i ptr

--------------------------------------------------------------------------------
-- Haskell type functions to C type functions
--------------------------------------------------------------------------------

hGSourceFunc :: EventGSourceFunc -> CGSourceFunc
hGSourceFunc f = fmap (\b -> if b then 1 else 0) . f

hInputFunc :: EventInputFunc -> CInputFunc
hInputFunc f = \u ci1 ci2 -> f u (fi ci1) (intToEvent $ fi ci2)

hTimeoutAdd :: EventTimeoutAdd -> CTimeoutAdd
hTimeoutAdd f = \ci cgs ud ->
    let gs = c_get_gsource_func cgs
    in  fi `fmap` f (fi ci) (cGSourceFunc gs) ud

hTimeoutAddSeconds :: EventTimeoutAddSeconds -> CTimeoutAddSeconds
hTimeoutAddSeconds f = \ci cgs ud ->
    let gs = c_get_gsource_func cgs
    in  fi `fmap` f (fi ci) (cGSourceFunc gs) ud

hTimeoutRemove :: EventTimeoutRemove -> CTimeoutRemove
hTimeoutRemove f = \ci ->
    (\b -> if b then 1 else 0) `fmap` f (fi ci)

hInputAdd :: EventInputAdd -> CInputAdd
hInputAdd f = \ci1 ci2 cif ud ->
    let inf = c_get_input_func cif
    in  fi `fmap` f (Fd $ fi ci1) (intToEvent $ fi ci2) (cInputFunc inf) ud

hInputRemove :: EventInputRemove -> CInputRemove
hInputRemove f = \ci ->
    (\b -> if b then 1 else 0) `fmap` f (fi ci)

hInputGetError :: EventInputGetError -> CInputGetError
hInputGetError f = \ci ptr ->
    unErrno `fmap` f (Fd $ fi ci) ptr
  where unErrno (Errno e) = e



--------------------------------------------------------------------------------
-- FunPtr generators
--------------------------------------------------------------------------------

-- create a InputFunc function pointer
foreign import ccall "wrapper"
    c_mk_input_func                 :: CInputFunc -> IO (FunPtr CInputFunc)

-- create a GSourceFunc function pointer
foreign import ccall "wrapper"
    c_mk_gsource_func               :: CGSourceFunc -> IO (FunPtr CGSourceFunc)

foreign import ccall "wrapper"
    c_mk_timeout_add                :: CTimeoutAdd -> IO (FunPtr CTimeoutAdd)

foreign import ccall "wrapper"
    c_mk_timeout_add_seconds        :: CTimeoutAddSeconds -> IO (FunPtr CTimeoutAddSeconds)

foreign import ccall "wrapper"
    c_mk_timeout_remove             :: CTimeoutRemove -> IO (FunPtr CTimeoutRemove)

foreign import ccall "wrapper"
    c_mk_input_add                  :: CInputAdd -> IO (FunPtr CInputAdd)

foreign import ccall "wrapper"
    c_mk_input_remove               :: CInputRemove -> IO (FunPtr CInputRemove)

foreign import ccall "wrapper"
    c_mk_input_get_error            :: CInputGetError -> IO (FunPtr CInputGetError)



--------------------------------------------------------------------------------
-- FunPtr to Fun
--------------------------------------------------------------------------------

-- create a InputFunc function pointer
foreign import ccall "dynamic"
    c_get_input_func                 :: FunPtr CInputFunc -> CInputFunc

-- create a GSourceFunc function pointer
foreign import ccall "dynamic"
    c_get_gsource_func               :: FunPtr CGSourceFunc -> CGSourceFunc

foreign import ccall "dynamic"
    c_get_timeout_add                :: FunPtr CTimeoutAdd -> CTimeoutAdd

foreign import ccall "dynamic"
    c_get_timeout_add_seconds        :: FunPtr CTimeoutAddSeconds -> CTimeoutAddSeconds

foreign import ccall "dynamic"
    c_get_timeout_remove             :: FunPtr CTimeoutRemove -> CTimeoutRemove

foreign import ccall "dynamic"
    c_get_input_add                  :: FunPtr CInputAdd -> CInputAdd

foreign import ccall "dynamic"
    c_get_input_remove               :: FunPtr CInputRemove -> CInputRemove

foreign import ccall "dynamic"
    c_get_input_get_error            :: FunPtr CInputGetError -> CInputGetError



--------------------------------------------------------------------------------
-- EventLoopUiOps structure instance
--------------------------------------------------------------------------------

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

instance Storable EventLoopUiOps where
    sizeOf _    = #size    PurpleEventLoopUiOps
    alignment _ = #alignof PurpleEventLoopUiOps
    peek ptr = do

        t_add   <- c_get_timeout_add            `fmap` (#peek PurpleEventLoopUiOps, timeout_add) ptr
        t_add_s <- c_get_timeout_add_seconds    `fmap` (#peek PurpleEventLoopUiOps, timeout_add_seconds) ptr
        t_rem   <- c_get_timeout_remove         `fmap` (#peek PurpleEventLoopUiOps, timeout_remove) ptr
        i_add   <- c_get_input_add              `fmap` (#peek PurpleEventLoopUiOps, input_add) ptr
        i_rem   <- c_get_input_remove           `fmap` (#peek PurpleEventLoopUiOps, input_remove) ptr
        -- i_err   <- c_get_input_get_error        `fmap` (#peek PurpleEventLoopUiOps, input_get_error) ptr

        return $ EventLoopUiOps (cTimeoutAdd t_add)
                                (cTimeoutAddSeconds t_add_s)
                                (cTimeoutRemove t_rem)
                                (cInputAdd i_add)
                                (cInputRemove i_rem)
                                -- (cInputGetError i_err)

    poke ptr x = do

        t_add   <- c_mk_timeout_add                 . hTimeoutAdd        $ timeout_add x
        t_add_s <- c_mk_timeout_add_seconds         . hTimeoutAddSeconds $ timeout_add_seconds x
        t_rem   <- c_mk_timeout_remove              . hTimeoutRemove     $ timeout_remove x

        i_add   <- c_mk_input_add                   . hInputAdd          $ input_add x
        i_rem   <- c_mk_input_remove                . hInputRemove       $ input_remove x
        -- i_err   <- c_mk_input_get_error             . hInputGetError     $ input_get_error x

        (#poke PurpleEventLoopUiOps, timeout_add)            ptr t_add
        (#poke PurpleEventLoopUiOps, timeout_add_seconds)    ptr t_add_s
        (#poke PurpleEventLoopUiOps, timeout_remove)         ptr t_rem

        (#poke PurpleEventLoopUiOps, input_add)              ptr i_add
        (#poke PurpleEventLoopUiOps, input_remove)           ptr i_rem
        -- (#poke PurpleEventLoopUiOps, input_get_error)        ptr i_err
        (#poke PurpleEventLoopUiOps, input_get_error)        ptr nullFunPtr

        (#poke PurpleEventLoopUiOps, _purple_reserved2)      ptr nullPtr
        (#poke PurpleEventLoopUiOps, _purple_reserved3)      ptr nullPtr
        (#poke PurpleEventLoopUiOps, _purple_reserved4)      ptr nullPtr
