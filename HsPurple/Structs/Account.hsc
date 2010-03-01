-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Structs.Account
    ( Account (..)
    ) where

import Bindings.GLib
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import qualified Data.Map as M
import qualified Data.Set as S

import HsPurple.Structs.ProxyInfo



type Table                          = M.Map String String
type List                           = S.Set String
type ProtocolId                     = String
type Connection                     = Ptr ()    -- TODO
type Presence                       = Ptr ()    -- TODO
type Log                            = Ptr ()    -- TODO
type UIData                         = Ptr ()    -- TODO
type AccountRegistrationCb          = Account -> Bool -> UserData -> IO ()
type AccountRegistrationCbUserData  = Ptr ()
type PrivateData                    = Ptr ()



--------------------------------------------------------------------------------
-- PurpleAccount Struct
--------------------------------------------------------------------------------

data Account = Account
    { username                  :: String                       -- ^ The username
    , alias                     :: String                       -- ^ How you appear to yourself
    , password                  :: String                       -- ^ The account password
    , userInfo                  :: String                       -- ^ User information
    , buddyIconPath             :: FilePath                     -- ^ The buddy icons non-cached path
    , rememberPass              :: Bool                         -- ^ Remember the password
    , protocolId                :: ProtocolId                   -- ^ The ID of the protocol
    , gc                        :: Connection                   -- ^ The connection handle
    , disconnecting             :: Bool                         -- ^ The account is currently disconnecting
    , settings                  :: Table                        -- ^ Protocol-specific settings
    , uiSettings                :: Table                        -- ^ UI-specific settings
    , proxyInfo                 :: ProxyInfo                    -- ^ Proxy information
    , permit                    :: List                         -- ^ Permit list
    , deny                      :: List                         -- ^ Deny list
    , permDeny                  :: Int                          -- ^ The permit/deny setting
    , statusTypes               :: List                         -- ^ Status types
    , presence                  :: Presence                     -- ^ Presence
    , systemLog                 :: Log                          -- ^ The system log
    , uiData                    :: UIData                       -- ^ The UI can put data here
    , registrationCb            :: AccountRegistrationCb
    , registrationCbUserData    :: AccountRegistrationCbUserData
    , priv                      :: PrivateData                  -- ^ Pointer to opaque private data
    }



--------------------------------------------------------------------------------
-- Storable instances
--------------------------------------------------------------------------------

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

-- TODO

instance Storable Account where
    sizeOf _    = #size    PurpleAccount
    alignment _ = #alignof PurpleAccount
    peek ptr = do

        user    <- (#peek PurpleAccount, username) ptr          >>= peekCString
        alias   <- (#peek PurpleAccount, alias) ptr             >>= peekCString
        pwd     <- (#peek PurpleAccount, password) ptr          >>= peekCString
        uInfo   <- (#peek PurpleAccount, user_info) ptr         >>= peekCString
        icon    <- (#peek PurpleAccount, buddy_icon_path) ptr   >>= peekCString
        rem_pwd <- ((1 :: CInt) ==) `fmap` (#peek PurpleAccount, remember_pass) ptr
        proto   <- (#peek PurpleAccount, protocol_id) ptr       >>= peekCString

        return $ Account    user
                            alias
                            pwd
                            uInfo
                            icon
                            rem_pwd
                            proto
                            undefined -- TODO
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined
                            undefined

    poke ptr (Account user
                      alias
                      pwd
                      uInfo
                      icon
                      rem_pwd
                      proto
                      _ -- TODO
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      _
                      ) = do

        newCString user     >>= (#poke PurpleAccount, username) ptr
        newCString alias    >>= (#poke PurpleAccount, alias) ptr
        newCString pwd      >>= (#poke PurpleAccount, password) ptr
        newCString uInfo    >>= (#poke PurpleAccount, user_info) ptr
        newCString icon     >>= (#poke PurpleAccount, buddy_icon_path) ptr
        newCString proto    >>= (#poke PurpleAccount, protocol_id) ptr
