{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Network.HsPurple.EventLoop
    (
    -- * Types
      EventLoopUiOps (..)
    , defaultEventLoopUiOps

    -- * Function types
    , EventTimeoutAdd
    , EventTimeoutAddSeconds
    , EventTimeoutRemove
    , EventInputAdd
    , EventInputRemove
    -- , EventInputGetError
    , EventGSourceFunc
    , EventInputFunc

    -- * Initialize UI Ops
    , setEventUiOps
    , getEventUiOps
    , initEvents

    -- * The Default Event Loop Functions
    , inputAdd
    , inputRemove
    -- , inputGetError
    , timeoutAdd
    , timeoutAddSeconds
    , timeoutRemove

    ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import qualified Control.Exception as E

import Foreign
import Foreign.C
import System.Posix
import System.Event

import Network.HsPurple.UiOps.EventLoopUiOps

import qualified Data.Set as S

data IFd = IFd
    { evId  :: EventId
    , key   :: Maybe (Either FdKey TimeoutKey)
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

-- | Generate a new EventId and return a function to add a Fd/TimeoutKey
prepare :: MVar (S.Set IFd) -> IO (Either FdKey TimeoutKey -> IO (), EventId)
prepare m = do

    eid <- modifyMVar m $ \s ->
        let i = if S.null s then 0 else evId (S.findMax s) + 1
        in return (S.insert (IFd i Nothing) s, i)

    let f k = modifyMVar_ m $ \s -> return $ S.insert (IFd eid (Just k)) s
    return (f, eid)

-- | Find the EventId of a FdKey
lookupFdk :: FdKey -> S.Set IFd -> Maybe EventId
lookupFdk fdk s =
    case S.elems $ S.filter ((Just (Left fdk) ==) . key) s of
         [IFd i _] -> Just i
         _         -> Nothing

-- | Find the Key of an Event
lookupEventId :: EventId -> S.Set IFd -> Maybe (Either FdKey TimeoutKey)
lookupEventId i s =
    case S.elems $ S.filter ((i ==) . evId) s of
         [IFd _ (Just ifd)] -> Just ifd
         _                  -> Nothing

-- | Handle IO exceptions
safe :: IO () -> IO ()
safe = safe' ()

-- | Handle IO exceptions with a default value
safe' :: a -> IO a -> IO a
safe' a = E.handle (\(e :: E.IOException) -> do putStrLn $ "Network.HsPurple.EventLoop.safe: Exception caught: `" ++ show e ++ "'"
                                                return a)



--------------------------------------------------------------------------------
-- Setting up the Event loop
--------------------------------------------------------------------------------


-- | Add an input handler
inputAdd :: Events -> Fd -> Event -> EventInputFunc -> UserData -> IO EventId
inputAdd evs fd event func ud = do

    -- putStr   $ "Network.HsPurple.EventLoop.inputAdd - Fd: " ++ show fd

    (f, i) <- prepare $ handles evs
    registerFd (eventManager evs) callback fd event >>= f . Left

    -- putStrLn " OK"
    return i

  where callback :: IOCallback
        callback fdk ev = do
            -- putStrLn $ "Network.HsPurple.EventLoop.inputAdd - Callback: " ++ show fdk ++ " - " ++ show ev
            i <- lookupFdk fdk `fmap` readMVar (handles evs)
            case i of
                 Just i' -> safe $ func ud i' ev
                 _       -> return () -- putStrLn "Event triggered but no callback found!"


-- | Remove a input handler
inputRemove :: Events -> EventId -> IO Bool
inputRemove ae h = do

    -- putStrLn $ "Network.HsPurple.EventLoop.inputRemove - Event: `" ++ show h ++ "'"
    fdk <- lookupEventId h `fmap` readMVar (handles ae)
    case fdk of
         Just (Left f) -> do unregisterFd (eventManager ae) f
                             return True
         _             -> do -- putStrLn $ "Network.HsPurple.EventLoop.inputRemove: No waiting event with id `" ++ show h ++ "'"
                             return False

{- TODO

-- | Get the current error status for an input.
inputGetError :: Events -> Fd -> Ptr CInt -> IO Errno
inputGetError _ _ _ = do
    return $ Errno (0) -- TODO

-}

-- | Creates a callback timer.
timeoutAdd :: Events -> Int -> EventGSourceFunc -> UserData -> IO EventId
timeoutAdd ae interval func ud = do

    -- putStrLn $ "Network.HsPurple.EventLoop.timeoutAdd - Interval: " ++ show interval

    (f,i) <- prepare $ handles ae
    safe $ registerTimeout (eventManager ae) interval (callback i) >>= f . Right
    return i

  where callback :: EventId -> TimeoutCallback -- type TimeoutCallback = IO ()
        callback i = do
            -- putStrLn $ "Network.HsPurple.EventLoop.timeoutAdd - Callback: `" ++ show i ++ "'"
            b <- safe' False (func ud)
            unless b $ () <$ timeoutRemove ae i

-- | Creates a callback timer.
timeoutAddSeconds :: Events -> Int -> EventGSourceFunc -> UserData -> IO EventId
timeoutAddSeconds ae interval func ud =

    timeoutAdd ae (interval * 1000) func ud -- not sure... hmhm


-- | Removes a timeout handler.
timeoutRemove :: Events -> EventId -> IO Bool
timeoutRemove ae h = do

    -- putStrLn $ "Network.HsPurple.EventLoop.timeoutRemove - Event: " ++ show h
    k <- lookupEventId h `fmap` readMVar (handles ae)
    case k of
         Just (Right tk) -> do unregisterTimeout (eventManager ae) tk
                               return True
         _               -> do return () -- putStrLn $ "Network.HsPurple.EventLoop.timeoutRemove - Fail: No function found with EventId `" ++ show h ++ "'"
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
    -- , input_get_error       = inputGetError ae
    }

-- | Initiliaze the Eventloop.
-- Returns the Events datatype and the main loop function
initEvents :: IO (Events, IO ())
initEvents = do
    evMg <- System.Event.new
    hand <- newMVar S.empty
    return (Events evMg hand, loop evMg)
