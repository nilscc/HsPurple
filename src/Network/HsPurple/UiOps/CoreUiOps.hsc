-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.CoreUiOps where

import Control.Applicative
import Foreign
import Network.HsPurple.GLib.GHashTable

import qualified Data.Map as M

#include <purple.h>
#include <bindings.dsl.h>


--------------------------------------------------------------------------------
-- CoreUiOps struct
--------------------------------------------------------------------------------

data CoreUiOps = CoreUiOps
    { ui_prefs_init     :: CoreUiPrefsInit
    , debug_ui_init     :: CoreDebugUiInit
    , ui_init           :: CoreUiInit
    , quit              :: CoreQuit
    , get_ui_info       :: CoreGetUiInfo
    }

instance Storable CoreUiOps where
    sizeOf _    = #size    PurpleCoreUiOps
    alignment _ = #alignof PurpleCoreUiOps
    peek ptr    = mK_CoreUiOps <$> peek (castPtr ptr)
    poke ptr a  = mk_CoreUiOps a >>= poke (castPtr ptr)


mk_CoreUiOps :: CoreUiOps -> IO C'PurpleCoreUiOps
mk_CoreUiOps (CoreUiOps ui_p d ui_i q g) =

    C'PurpleCoreUiOps <$> mk_UiPrefsInit  ui_p
                      <*> mk_DebugUiInit  d
                      <*> mk_UiInit       ui_i
                      <*> mk_Quit         q
                      <*> mk_GetUiInfo    g
                      <*> return nullFunPtr
                      <*> return nullFunPtr
                      <*> return nullFunPtr

mK_CoreUiOps :: C'PurpleCoreUiOps -> CoreUiOps
mK_CoreUiOps (C'PurpleCoreUiOps ui_p d ui_i q g _ _ _) =

    CoreUiOps ( mK_UiPrefsInit  ui_p )
              ( mK_DebugUiInit  d    )
              ( mK_UiInit       ui_i )
              ( mK_Quit         q    )
              ( mK_GetUiInfo    g    )

--------------------------------------------------------------------------------
-- C imports
--------------------------------------------------------------------------------

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#starttype PurpleCoreUiOps
#field ui_prefs_init        , <UiPrefsInit>
#field debug_ui_init        , <DebugUiInit>
#field ui_init              , <UiInit>
#field quit                 , <Quit>
#field get_ui_info          , <GetUiInfo>
#field _purple_reserved1    , FunPtr ()
#field _purple_reserved2    , FunPtr ()
#field _purple_reserved3    , FunPtr ()
#stoptype

#callback UiPrefsInit , IO ()
#callback DebugUiInit , IO ()
#callback UiInit      , IO ()
#callback Quit        , IO ()
#callback GetUiInfo   , IO (Ptr <GHashTable>)

type CoreUiPrefsInit = IO ()
type CoreDebugUiInit = IO ()
type CoreUiInit      = IO ()
type CoreQuit        = IO ()
type CoreGetUiInfo   = IO (M.Map String String)

mk_UiPrefsInit :: CoreUiPrefsInit -> IO C'UiPrefsInit 
mk_UiPrefsInit f = mk'UiPrefsInit f
mk_DebugUiInit :: CoreDebugUiInit -> IO C'DebugUiInit
mk_DebugUiInit f = mk'DebugUiInit f
mk_UiInit :: CoreUiInit -> IO C'UiInit
mk_UiInit f = mk'UiInit f
mk_Quit :: CoreQuit -> IO C'Quit
mk_Quit f = mk'Quit f
mk_GetUiInfo :: CoreGetUiInfo -> IO C'GetUiInfo
mk_GetUiInfo f = mk'GetUiInfo (f >>= stringMapToGHashTable)

mK_UiPrefsInit :: C'UiPrefsInit -> CoreUiPrefsInit
mK_UiPrefsInit f = mK'UiPrefsInit f
mK_DebugUiInit :: C'DebugUiInit -> CoreDebugUiInit
mK_DebugUiInit f = mK'DebugUiInit f
mK_UiInit :: C'UiInit -> CoreUiInit
mK_UiInit f = mK'UiInit f
mK_Quit :: C'Quit -> CoreQuit
mK_Quit f = mK'Quit f
mK_GetUiInfo :: C'GetUiInfo -> CoreGetUiInfo
mK_GetUiInfo f = mK'GetUiInfo f >>= gHasHTableToStringMap
