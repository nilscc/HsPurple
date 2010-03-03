-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.UiOps.BlistUiOps
    (
    ) where

import Foreign
import Foreign.C
import Foreign.Storable


#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

-- | Sets UI-specific data on a buddy list
type NewList = BuddyList -> IO ()
type BuddyList = Ptr ()

foreign import ccall "wrapper"
    c_mk_new_list :: NewList -> IO (FunPtr NewList)

foreign import ccall "dynamic"
    c_get_new_list :: FunPtr NewList -> NewList

-- | Sets UI-specific data on a node
type NewNode = BlistNode -> IO ()
type BlistNode = Ptr ()

foreign import ccall "wrapper"
    c_mk_new_node :: NewNode -> IO (FunPtr NewNode)

foreign import ccall "dynamic"
    c_get_new_node :: FunPtr NewNode -> NewNode

-- | The core will call this when it's finished doing its core stuff
type ShowBlist = BuddyList -> IO ()

foreign import ccall "wrapper"
    c_mk_show_blist :: ShowBlist -> IO (FunPtr ShowBlist)

foreign import ccall "dynamic"
    c_get_show_blist :: FunPtr ShowBlist -> ShowBlist

-- | This will update a node in the buddy list
type Update = BuddyList -> BlistNode -> IO ()

foreign import ccall "wrapper"
    c_mk_update :: Update -> IO (FunPtr Update)

foreign import ccall "dynamic"
    c_get_update :: FunPtr Update -> Update

-- | This removes a node from the list
type Remove = BuddyList -> BlistNode -> IO ()

foreign import ccall "wrapper"
    c_mk_remove :: Remove -> IO (FunPtr Remove)

foreign import ccall "dynamic"
    c_get_remove :: FunPtr Remove -> Remove

-- | When the list is destroyed, this is called to destroy the UI
type Destroy = BuddyList -> IO ()

foreign import ccall "wrapper"
    c_mk_destroy :: Destroy -> IO (FunPtr Destroy)

foreign import ccall "dynamic"
    c_get_destroy :: FunPtr Destroy -> Destroy

-- | Hides or unhides the buddy list
type SetVisible  = BuddyList -> Bool -> IO ()
type CSetVisible = BuddyList -> CInt -> IO ()

hSetVisible :: SetVisible -> CSetVisible
hSetVisible f = \bl ci ->
    f bl (ci == 1)

cSetVisible :: CSetVisible -> SetVisible
cSetVisible f = \bl b ->
    f bl (if b then 1 else 0)

foreign import ccall "wrapper"
    c_mk_set_visible :: CSetVisible -> IO (FunPtr CSetVisible)

foreign import ccall "dynamic"
    c_get_set_visible :: FunPtr CSetVisible -> CSetVisible

type RequestAddBuddy  = Account -> Username -> Group -> Alias -> IO ()
type CRequestAddBuddy = Account -> CString -> CString -> CString -> IO ()

foreign import ccall "wrapper"
    c_mk_request_add_buddy :: CRequestAddBuddy -> IO (FunPtr CRequestAddBuddy)

foreign import ccall "dynamic"
    c_get_request_add_buddy :: FunPtr CRequestAddBuddy -> CRequestAddBuddy

hRequestAddBuddy :: RequestAddBuddy -> CRequestAddBuddy
hRequestAddBuddy f = \acc cs1 cs2 cs3 -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    s3 <- peekCString cs3
    f acc s1 s2 s3

cRequestAddBuddy :: CRequestAddBuddy -> RequestAddBuddy
cRequestAddBuddy f = \acc s1 s2 s3 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    f acc cs1 cs2 cs3

type Account = Ptr ()
type Username = String
type Group = String
type Alias = String

type RequestAddChat  = Account -> GroupP -> Alias -> Username -> IO ()
type CRequestAddChat = Account -> GroupP -> CString -> CString -> IO ()

foreign import ccall "wrapper"
    c_mk_request_add_chat :: CRequestAddChat -> IO (FunPtr CRequestAddChat)

foreign import ccall "dynamic"
    c_get_request_add_chat :: FunPtr CRequestAddChat -> CRequestAddChat

hRequestAddChat :: RequestAddChat -> CRequestAddChat
hRequestAddChat f = \acc gpp cs1 cs2 -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    f acc gpp s1 s2

cRequestAddChat :: CRequestAddChat -> RequestAddChat
cRequestAddChat f = \acc gpp s1 s2 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    f acc gpp cs1 cs2

type GroupP = Ptr ()

type RequestAddGroup = IO ()

foreign import ccall "wrapper"
    c_mk_request_add_group :: RequestAddGroup -> IO (FunPtr RequestAddGroup)

foreign import ccall "dynamic"
    c_get_request_add_group :: FunPtr RequestAddGroup -> RequestAddGroup

-- | This is called when a node has been modified and should be saved.
-- 
-- Implementation of this UI op is OPTIONAL. If not implemented, it will
-- be set to a fallback function that saves data to blist.xml like in
-- previous libpurple versions.
-- 
-- Since 2.6.0.
type SaveNode = BlistNode -> IO ()

foreign import ccall "wrapper"
    c_mk_save_node :: SaveNode -> IO (FunPtr SaveNode)

foreign import ccall "dynamic"
    c_get_save_node :: FunPtr SaveNode -> SaveNode

-- | Called when a node is about to be removed from the buddy list.
-- The UI op should update the relevant data structures to remove this
-- node (for example, removing a buddy from the group this node is in).
--
-- Implementation of this UI op is OPTIONAL. If not implemented, it will
-- be set to a fallback function that saves data to blist.xml like in
-- previous libpurple versions.
--
-- Since 2.6.0.
type RemoveNode = BlistNode -> IO ()

foreign import ccall "wrapper"
    c_mk_remove_node :: RemoveNode -> IO (FunPtr RemoveNode)

foreign import ccall "dynamic"
    c_get_remove_node :: FunPtr RemoveNode -> RemoveNode

-- | Called to save all the data for an account. If the UI sets this,
-- the callback must save the privacy and buddy list data for an account.
-- If the account is NULL, save the data for all accounts.
--
-- Implementation of this UI op is OPTIONAL. If not implemented, it will
-- be set to a fallback function that saves data to blist.xml like in
-- previous libpurple versions.
--
-- Since 2.6.0.
type SaveAccount = Account -> IO ()

foreign import ccall "wrapper"
    c_mk_save_account :: SaveAccount -> IO (FunPtr SaveAccount)

foreign import ccall "dynamic"
    c_get_save_account :: FunPtr SaveAccount -> SaveAccount


data BlistUiOps = BlistUiOps
    { newList           :: NewList
    , newNode           :: NewNode
    , showBlist         :: ShowBlist
    , update            :: Update
    , remove            :: Remove
    , destroy           :: Destroy
    , setVisible        :: SetVisible
    , requestAddBuddy   :: RequestAddBuddy
    , requestAddChat    :: RequestAddChat
    , requestAddGroup   :: RequestAddGroup
	-- void (*_purple_reserved1)(void);
    }

(<$>) = fmap

instance Storable BlistUiOps where
    sizeOf _    = #size    PurpleBlistUiOps
    alignment _ = #alignof PurpleBlistUiOps
    peek ptr    = do

        new_l   <- c_get_new_list <$> (#peek PurpleBlistUiOps, new_list) ptr
        new_n   <- c_get_new_node <$> (#peek PurpleBlistUiOps, new_node) ptr
        shw     <- c_get_show_blist <$> (#peek PurpleBlistUiOps, show) ptr
        upd     <- c_get_update <$> (#peek PurpleBlistUiOps, update) ptr
        rm      <- c_get_remove <$> (#peek PurpleBlistUiOps, remove) ptr
        des     <- c_get_destroy <$> (#peek PurpleBlistUiOps, destroy) ptr
        set_v   <- c_get_set_visible <$> (#peek PurpleBlistUiOps, set_visible) ptr
        req_b   <- c_get_request_add_buddy <$> (#peek PurpleBlistUiOps, request_add_buddy) ptr
        req_c   <- c_get_request_add_chat <$> (#peek PurpleBlistUiOps, request_add_chat) ptr
        req_g   <- c_get_request_add_group <$> (#peek PurpleBlistUiOps, request_add_group) ptr

        return $ BlistUiOps new_l
                            new_n
                            shw
                            upd
                            rm
                            des
                            (cSetVisible set_v)
                            (cRequestAddBuddy req_b)
                            (cRequestAddChat req_c)
                            req_g

    poke ptr (BlistUiOps new_l
                         new_n
                         shw
                         upd
                         rm
                         des
                         set_v
                         req_b
                         req_c
                         req_g
                         ) = do

        c_mk_new_list new_l >>= (#poke PurpleBlistUiOps, new_list) ptr
        c_mk_new_node new_n >>= (#poke PurpleBlistUiOps, new_node) ptr
        c_mk_show_blist shw >>= (#poke PurpleBlistUiOps, show) ptr
        c_mk_update upd >>= (#poke PurpleBlistUiOps, update) ptr
        c_mk_remove rm >>= (#poke PurpleBlistUiOps, remove) ptr
        c_mk_destroy des >>= (#poke PurpleBlistUiOps, destroy) ptr
        c_mk_set_visible (hSetVisible set_v) >>= (#poke PurpleBlistUiOps, set_visible) ptr
        c_mk_request_add_buddy (hRequestAddBuddy req_b) >>= (#poke PurpleBlistUiOps, request_add_buddy) ptr
        c_mk_request_add_chat (hRequestAddChat req_c) >>= (#poke PurpleBlistUiOps, request_add_chat) ptr
        c_mk_request_add_group req_g >>= (#poke PurpleBlistUiOps, request_add_group) ptr

        (#poke PurpleBlistUiOps, _purple_reserved1) ptr nullPtr
