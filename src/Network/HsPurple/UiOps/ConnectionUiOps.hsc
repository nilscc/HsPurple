-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.ConnectionUiOps where

import Control.Applicative
import Foreign
import Foreign.C

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <bindings.dsl.h>
#include <purple.h>


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


--------------------------------------------------------------------------------
-- Haskell data representation
--------------------------------------------------------------------------------

type Connection = Ptr ()

data ConnectionUiOps = ConnectionUiOps
    { connectProgress               :: ConnectionConnectProgress
    , connected                     :: ConnectionConnected
    , disconnected                  :: ConnectionDisconnected
    , notice                        :: ConnectionNotice
    -- , reportDisconnect              :: ReportDisconnect -- depreciated
    , networkConnected              :: ConnectionNetworkConnected
    , networkDisconnected           :: ConnectionNetworkDisconnected
    , reportDisconnectReason        :: ConnectionReportDisconnectReason
    }

-- | When an account is connecting, this operation is called to notify
-- the UI of what is happening, as well as which \"step\" out of
-- \"step count\" has been reached (which might be displayed as a progress
-- bar).
type ConnectionConnectProgress = Connection
                              -> String           -- ^ Text
                              -> Int              -- ^ Step
                              -> Int              -- ^ Step count
                              -> IO ()

-- | Called when a connection is established (just before the
-- @ref signed-on signal).
type ConnectionConnected = Connection -> IO ()

-- | Called when a connection is ended (between the @ref signing-off
-- and @ref signed-off signals).
type ConnectionDisconnected = Connection -> IO ()

-- | Used to display connection-specific notices.  (Pidgin's Gtk user
-- interface implements this as a no-op; purple_connection_notice(),
-- which uses this operation, is not used by any of the protocols
-- shipped with libpurple.)
type ConnectionNotice = Connection
                     -> String        -- ^ Text
                     -> IO ()

{-
-- | Called when an error causes a connection to be disconnected.
-- Called before disconnected.
type ReportDisconnect = Connection
                     -> String      -- ^ Localized error message
                     -> IO ()
-}

type ConnectionError = Int

-- | Called when an error causes a connection to be disconnected.
--  Called before disconnected.  This op is intended to replace
--  report_disconnect.  If both are implemented, this will be called
--  first; however, there's no real reason to implement both.
type ConnectionReportDisconnectReason = Connection
                                     -> ConnectionError   -- ^ Error code
                                     -> String            -- ^ Localized message
                                                          -- describing the disconnection
                                                          -- in more detail to the user.
                                     -> IO ()

-- | Called when libpurple discovers that the computer's network
-- connection has gone away.
type ConnectionNetworkConnected = IO ()

-- | Called when libpurple discovers that the computer's network
-- connection is active.  On Linux, this uses Network Manager if
-- available; on Windows, it uses Win32's network change notification
-- infrastructure.
type ConnectionNetworkDisconnected = IO ()

instance Storable ConnectionUiOps where
    sizeOf _    = #size PurpleConnectionUiOps
    alignment _ = #alignof PurpleConnectionUiOps
    peek ptr    = get_ConnectionUiOps <$> peek (castPtr ptr)
    poke ptr ui = mk_ConnectionUiOps ui >>= poke (castPtr ptr)


-- | Convert a ConnectionUiOps struct into its C representation
mk_ConnectionUiOps :: ConnectionUiOps -> IO C'PurpleConnectionUiOps
mk_ConnectionUiOps (ConnectionUiOps con_p
                                    con
                                    dis
                                    note
                                    -- r_dis
                                    n_con
                                    n_dis
                                    r_dis_r) =

    C'PurpleConnectionUiOps <$> mk_ConnectProgress          con_p
                            <*> mk_Connected                con
                            <*> mk_Disconnected             dis
                            <*> mk_Notice                   note
                            -- <*> mk_ReportDisconnect         r_dis
                            <*> return nullFunPtr
                            <*> mk_NetworkConnected         n_con
                            <*> mk_NetworkDisconnected      n_dis
                            <*> mk_ReportDisconnectReason   r_dis_r
                            <*> return nullPtr
                            <*> return nullPtr
                            <*> return nullPtr


-- | Convert a C'PurpleConnectionUiOps struct into its Haskell representation
get_ConnectionUiOps :: C'PurpleConnectionUiOps -> ConnectionUiOps
get_ConnectionUiOps (C'PurpleConnectionUiOps con_p
                                             con
                                             dis
                                             note
                                             _ -- r_dis
                                             n_con
                                             n_dis
                                             r_dis_r
                                             _ _ _ -- nullpointer
                                             ) =

    ConnectionUiOps ( get_ConnectProgress          con_p   )
                    ( get_Connected                con     )
                    ( get_Disconnected             dis     )
                    ( get_Notice                   note    )
                    -- ( get_ReportDisconnect         r_dis   )
                    ( get_NetworkConnected         n_con   )
                    ( get_NetworkDisconnected      n_dis   )
                    ( get_ReportDisconnectReason   r_dis_r )  


#callback ConnectProgress           , Connection -> CString -> CInt -> CInt -> IO ()
#callback Connected                 , Connection -> IO ()
#callback Disconnected              , Connection -> IO ()
#callback Notice                    , Connection -> CString -> IO ()
#callback ReportDisconnect          , Connection -> CString -> IO ()
#callback ReportDisconnectReason    , Connection -> CInt -> CString -> IO ()
#callback NetworkConnected          , IO ()
#callback NetworkDisconnected       , IO ()

#starttype PurpleConnectionUiOps
#field connect_progress             , <ConnectProgress>
#field connected                    , <Connected>
#field disconnected                 , <Disconnected>
#field notice                       , <Notice>
#field report_disconnect            , <ReportDisconnect>
#field network_connected            , <NetworkConnected>
#field network_disconnected         , <NetworkDisconnected>
#field report_disconnect_reason     , <ReportDisconnectReason>
#field _purple_reserved1            , Ptr ()
#field _purple_reserved2            , Ptr ()
#field _purple_reserved3            , Ptr ()
#stoptype


--------------------------------------------------------------------------------
-- Create C versions of our callback functions
--------------------------------------------------------------------------------

mk_ConnectProgress          :: ConnectionConnectProgress          -> IO C'ConnectProgress
mk_ConnectProgress f =
    mk'ConnectProgress $ \con cs1 ci1 ci2 -> do
        s1 <- peekCString cs1
        f con s1 (fi ci1) (fi ci2)

mk_Connected                :: ConnectionConnected                -> IO C'Connected
mk_Connected =
    mk'Connected

mk_Disconnected             :: ConnectionDisconnected             -> IO C'Disconnected
mk_Disconnected =
    mk'Disconnected

mk_Notice                   :: ConnectionNotice                   -> IO C'Notice
mk_Notice f =
    mk'Notice $ \con cs1 -> do
        s1 <- peekCString cs1
        f con s1

{-
mk_ReportDisconnect         :: ConnectionReportDisconnect         -> IO C'ReportDisconnect
mk_ReportDisconnect f =
    mk'ReportDisconnect $ \con cs1 -> do
        s1 <- peekCString cs1
        f con s1
-}

mk_ReportDisconnectReason   :: ConnectionReportDisconnectReason   -> IO C'ReportDisconnectReason
mk_ReportDisconnectReason f =
    mk'ReportDisconnectReason $ \con ce cs1 -> do
        s1 <- peekCString cs1
        f con (fi ce) s1

mk_NetworkConnected         :: ConnectionNetworkConnected         -> IO C'NetworkConnected
mk_NetworkConnected =
    mk'NetworkConnected

mk_NetworkDisconnected      :: ConnectionNetworkDisconnected      -> IO C'NetworkDisconnected
mk_NetworkDisconnected =
    mk'NetworkDisconnected


--------------------------------------------------------------------------------
-- Create Haskell versions of our callback functions
--------------------------------------------------------------------------------

get_ConnectProgress          :: C'ConnectProgress          -> ConnectionConnectProgress
get_ConnectProgress f = \con s1 i1 i2 -> do
    cs1 <- newCString s1
    (mK'ConnectProgress f) con cs1 (fi i1) (fi i2)

get_Connected                :: C'Connected                -> ConnectionConnected
get_Connected = mK'Connected

get_Disconnected             :: C'Disconnected             -> ConnectionDisconnected
get_Disconnected = mK'Disconnected

get_Notice                   :: C'Notice                   -> ConnectionNotice
get_Notice f = \con s1 -> do
    cs1 <- newCString s1
    (mK'Notice f) con cs1

{-
get_ReportDisconnect         :: C'ReportDisconnect         -> ConnectionReportDisconnect
get_ReportDisconnect f =
    mK'ReportDisconnect $ \con s1 -> do
        cs1 <- newCString s1
        f con cs1
-}

get_ReportDisconnectReason   :: C'ReportDisconnectReason   -> ConnectionReportDisconnectReason
get_ReportDisconnectReason f = \con ce s1 -> do
        cs1 <- newCString s1
        (mK'ReportDisconnectReason f) con (fi ce) cs1

get_NetworkConnected         :: C'NetworkConnected         -> ConnectionNetworkConnected
get_NetworkConnected = mK'NetworkConnected

get_NetworkDisconnected      :: C'NetworkDisconnected      -> ConnectionNetworkDisconnected
get_NetworkDisconnected = mK'NetworkDisconnected
