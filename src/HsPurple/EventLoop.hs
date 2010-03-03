{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.EventLoop
    (
    -- * Types
      EventLoopUiOps (..)
    , defaultEventLoopUiOps

    -- * Initialize UI Ops
    , setEventUiOps
    , getEventUiOps
    , initEvents

    -- * The Default Event Loop Functions
    , inputAdd
    , inputRemove
    , inputGetError
    , timeoutAdd
    , timeoutAddSeconds
    , timeoutRemove

    ) where

import Control.Applicative
import Control.Concurrent.MVar

import Foreign
import Foreign.C
import System.Posix
import System.Event

import HsPurple.UiOps.EventLoopUiOps

import qualified Data.Set as S

type UserData = Ptr ()
type EventId = Int

data IFd = IFd
    { evId  :: EventId
    , key   :: Either FdKey TimeoutKey
    }

instance Eq IFd where
    a == b = evId a == evId b

instance Ord IFd where
    compare a b = evId a `compare` evId b

data Events = Events
    { eventManager      :: EventManager
    , handles           :: MVar (S.Set IFd)
    }


--------------------------------------------------------------------------------
-- Purple functions: Foreign imports
--------------------------------------------------------------------------------

foreign import ccall "purple_eventloop_set_ui_ops"
    c_set_ui_ops            :: Ptr EventLoopUiOps -> IO ()

foreign import ccall "purple_eventloop_get_ui_ops"
    c_get_ui_ops            :: IO (Ptr EventLoopUiOps)



--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

incHandles :: MVar (S.Set IFd) -> Either FdKey TimeoutKey -> IO Int
incHandles m key' = modifyMVar m $ \s ->
    let newH n = (S.insert (IFd n key') s, n)
    in  return . newH $ if S.null s
                          then 0
                          else evId (S.findMax s) + 1

lookupFdk :: FdKey -> S.Set IFd -> Maybe Int
lookupFdk fdk s =
    case S.elems $ S.filter ((Left fdk ==) . key) s of
         [IFd i _] -> Just i
         _         -> Nothing

lookupEventId :: EventId -> S.Set IFd -> Maybe (Either FdKey TimeoutKey)
lookupEventId i s =
    case S.elems $ S.filter ((i ==) . evId) s of
         [IFd _ ifd] -> Just ifd
         _           -> Nothing



--------------------------------------------------------------------------------
-- Setting up the Event loop
--------------------------------------------------------------------------------


-- | Add an input handler
inputAdd :: Events -> Fd -> Event -> InputFunc -> UserData -> IO EventId
inputAdd evs fd event func ud = do

    -- putStrLn $ "HsPurple.EventLoop.inputAdd - Fd: " ++ show fd

    registerFd (eventManager evs) callback fd event >>= incHandles (handles evs) . Left

  where callback :: IOCallback
        callback fdk ev = do
            i <- lookupFdk fdk `fmap` readMVar (handles evs)
            case i of
                 Just i' -> func ud i' ev
                 _       -> return ()


-- | Remove a input handler
inputRemove :: Events -> EventId -> IO Bool
inputRemove ae h = do

    -- putStr $ "HsPurple.EventLoop.inputRemove - Event: " ++ show h
    fdk <- lookupEventId h `fmap` readMVar (handles ae)
    case fdk of
         Just (Left f) -> do unregisterFd (eventManager ae) f
                             -- putStrLn " OK"
                             return True
         _             -> do -- putStrLn " Fail"
                             return False

-- | Get the current error status for an input.
inputGetError :: Events -> Fd -> Ptr CInt -> IO Errno
inputGetError _ _ _ = return $ Errno 0

-- | Creates a callback timer.
timeoutAdd :: Events -> Int -> GSourceFunc -> UserData -> IO EventId
timeoutAdd ae interval func ud = do

    -- putStrLn $ "HsPurple.EventLoop.timeoutAdd - Interval: " ++ show interval

    registerTimeout (eventManager ae) interval callback >>= incHandles (handles ae) . Right

  where callback :: TimeoutCallback -- type TimeoutCallback = IO ()
        callback = () <$ func ud -- hmhmhm no this actually returns a Bool

-- | Creates a callback timer.
timeoutAddSeconds :: Events -> Int -> GSourceFunc -> UserData -> IO EventId
timeoutAddSeconds ae interval func ud =

    timeoutAdd ae (interval * 1000) func ud -- not sure... hmhm


-- | Removes a timeout handler.
timeoutRemove :: Events -> EventId -> IO Bool
timeoutRemove ae h = do

    -- putStr $ "HsPurple.EventLoop.timeoutRemove - Event: " ++ show h
    k <- lookupEventId h `fmap` readMVar (handles ae)
    case k of
         Just (Right tk) -> do unregisterTimeout (eventManager ae) tk
                               -- putStrLn " OK"
                               return True
         _               -> do -- putStrLn " Fail"
                               return False



--------------------------------------------------------------------------------
-- Initialize UI Ops
--------------------------------------------------------------------------------

-- | Sets the UI operations structure to be used for accounts.
setEventUiOps :: EventLoopUiOps -> IO ()
setEventUiOps eventLoopUiOps = do
    ptr     <- malloc
    poke ptr eventLoopUiOps
    c_set_ui_ops ptr

-- | Returns the UI operations structure used for accounts.
getEventUiOps :: IO (Ptr EventLoopUiOps)
getEventUiOps = c_get_ui_ops

-- | The default event loop
defaultEventLoopUiOps :: Events -> EventLoopUiOps
defaultEventLoopUiOps ae = EventLoopUiOps
    { timeout_add           = timeoutAdd ae
    , timeout_add_seconds   = timeoutAddSeconds ae
    , timeout_remove        = timeoutRemove ae
    , input_add             = inputAdd ae
    , input_remove          = inputRemove ae
    , input_get_error       = inputGetError ae
    }

-- | Initiliaze the Eventloop.
-- Returns the Events datatype and the main loop function
initEvents :: IO (Events, IO ())
initEvents = do
    evMg <- System.Event.new
    hand <- newMVar S.empty
    return (Events evMg hand, loop evMg)
