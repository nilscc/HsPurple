-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.UiOps.ConversationUiOps
    (
      ConversationUiOps (..)
    , MessageFlag (..)
    , showMessageFlag
    , readMessageFlag

    -- * Function types
    , CreateConversation
    , DestroyConversation
    , WriteChat
    , WriteIM
    , WriteConv
    , ChatAddUsers
    , ChatRenameUser
    , ChatRemoveUsers
    , ChatUpdateUser
    , Present
    , HasFocus
    , CustomSmileyAdd
    , CustomSmileyWrite
    , CustomSmileyClose
    , SendConfirm
    ) where

import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Clock.POSIX

import Foreign
import Foreign.C

import Bindings.GLib

import HsPurple.UiOps.Util

#let alignof t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <purple.h>

-- | Conversation operations and events.
--
-- Any UI representing a conversation must assign a filled-out
-- PurpleConversationUiOps structure to the PurpleConversation.
data ConversationUiOps = ConversationUiOps
    { createConversation :: CreateConversation
    , destroyConversation :: DestroyConversation
    , writeChat :: WriteChat
    , writeIM :: WriteIM
    , writeConv :: WriteConv
    , chatAddUsers :: ChatAddUsers
    , chatRenameUser :: ChatRenameUser
    , chatRemoveUsers :: ChatRemoveUsers
    , chatUpdateUser :: ChatUpdateUser
    , present :: Present
    , hasFocus :: HasFocus
    , customSmileyAdd :: CustomSmileyAdd
    , customSmileyWrite :: CustomSmileyWrite
    , customSmileyClose :: CustomSmileyClose
    , sendConfirm :: SendConfirm
	-- void (*_purple_reserved1)(void);
	-- void (*_purple_reserved2)(void);
	-- void (*_purple_reserved3)(void);
	-- void (*_purple_reserved4)(void);
    }

instance Storable ConversationUiOps where
    sizeOf _    = #size    PurpleConversationUiOps
    alignment _ = #alignof PurpleConversationUiOps
    peek ptr    = do

        create_conversation <- c_get_create_conversation `fmap` (#peek PurpleConversationUiOps, create_conversation) ptr
        destroy_conversation <- c_get_destroy_conversation `fmap` (#peek PurpleConversationUiOps, destroy_conversation) ptr
        write_chat <- c_get_write_chat `fmap` (#peek PurpleConversationUiOps, write_chat) ptr
        write_im <- c_get_write_im `fmap` (#peek PurpleConversationUiOps, write_im) ptr
        write_conv <- c_get_write_conv `fmap` (#peek PurpleConversationUiOps, write_conv) ptr
        chat_add_users <- c_get_chat_add_users `fmap` (#peek PurpleConversationUiOps, chat_add_users) ptr
        chat_rename_user <- c_get_chat_rename_user `fmap` (#peek PurpleConversationUiOps, chat_rename_user) ptr
        chat_remove_users <- c_get_chat_remove_users `fmap` (#peek PurpleConversationUiOps, chat_remove_users) ptr
        chat_update_user <- c_get_chat_update_user `fmap` (#peek PurpleConversationUiOps, chat_update_user) ptr
        present' <- c_get_present `fmap` (#peek PurpleConversationUiOps, present) ptr
        has_focus <- c_get_has_focus `fmap` (#peek PurpleConversationUiOps, has_focus) ptr
        custom_smiley_add <- c_get_custom_smiley_add `fmap` (#peek PurpleConversationUiOps, custom_smiley_add) ptr
        custom_smiley_write <- c_get_custom_smiley_write `fmap` (#peek PurpleConversationUiOps, custom_smiley_write) ptr
        custom_smiley_close <- c_get_custom_smiley_close `fmap` (#peek PurpleConversationUiOps, custom_smiley_close) ptr
        send_confirm <- c_get_send_confirm `fmap` (#peek PurpleConversationUiOps, send_confirm) ptr

        return $ ConversationUiOps create_conversation
                                   destroy_conversation
                                   (cWriteChat write_chat)
                                   (cWriteIM write_im)
                                   (cWriteConv write_conv)
                                   (cChatAddUsers chat_add_users)
                                   (cChatRenameUser chat_rename_user)
                                   (cChatRemoveUsers chat_remove_users)
                                   (cChatUpdateUser chat_update_user)
                                   present'
                                   (cHasFocus has_focus)
                                   (cCustomSmileyAdd custom_smiley_add)
                                   (cCustomSmileyWrite custom_smiley_write)
                                   (cCustomSmileyClose custom_smiley_close)
                                   (cSendConfirm send_confirm)

    poke ptr (ConversationUiOps create_conversation
                                destroy_conversation
                                write_chat
                                write_im
                                write_conv
                                chat_add_users
                                chat_rename_user
                                chat_remove_users
                                chat_update_user
                                present'
                                has_focus
                                custom_smiley_add
                                custom_smiley_write
                                custom_smiley_close
                                send_confirm
                                ) = do

        c_mk_create_conversation create_conversation >>= (#poke PurpleConversationUiOps, create_conversation) ptr
        c_mk_destroy_conversation destroy_conversation >>= (#poke PurpleConversationUiOps, destroy_conversation) ptr
        c_mk_write_chat (hWriteChat write_chat) >>= (#poke PurpleConversationUiOps, write_chat) ptr
        c_mk_write_im (hWriteIM write_im) >>= (#poke PurpleConversationUiOps, write_im) ptr
        c_mk_write_conv (hWriteConv write_conv) >>= (#poke PurpleConversationUiOps, write_conv) ptr
        c_mk_chat_add_users (hChatAddUsers chat_add_users) >>= (#poke PurpleConversationUiOps, chat_add_users) ptr
        c_mk_chat_rename_user (hChatRenameUser chat_rename_user) >>= (#poke PurpleConversationUiOps, chat_rename_user) ptr
        c_mk_chat_remove_users (hChatRemoveUsers chat_remove_users) >>= (#poke PurpleConversationUiOps, chat_remove_users) ptr
        c_mk_chat_update_user (hChatUpdateUser chat_update_user) >>= (#poke PurpleConversationUiOps, chat_update_user) ptr
        c_mk_present present' >>= (#poke PurpleConversationUiOps, present) ptr
        c_mk_has_focus (hHasFocus has_focus) >>= (#poke PurpleConversationUiOps, has_focus) ptr
        c_mk_custom_smiley_add (hCustomSmileyAdd custom_smiley_add) >>= (#poke PurpleConversationUiOps, custom_smiley_add) ptr
        c_mk_custom_smiley_write (hCustomSmileyWrite custom_smiley_write) >>= (#poke PurpleConversationUiOps, custom_smiley_write) ptr
        c_mk_custom_smiley_close (hCustomSmileyClose custom_smiley_close) >>= (#poke PurpleConversationUiOps, custom_smiley_close) ptr
        c_mk_send_confirm (hSendConfirm send_confirm) >>= (#poke PurpleConversationUiOps, send_confirm) ptr

        (#poke PurpleConversationUiOps, _purple_reserved1) ptr nullPtr
        (#poke PurpleConversationUiOps, _purple_reserved2) ptr nullPtr
        (#poke PurpleConversationUiOps, _purple_reserved3) ptr nullPtr
        (#poke PurpleConversationUiOps, _purple_reserved4) ptr nullPtr


--------------------------------------------------------------------------------
-- Function types
--------------------------------------------------------------------------------

-- | Called when @a conv is created (but before the @ref
-- conversation-created signal is emitted).
type CreateConversation = Conversation -> IO ()
type Conversation = Ptr ()

foreign import ccall "wrapper"
    c_mk_create_conversation :: CreateConversation -> IO (FunPtr CreateConversation)

foreign import ccall "dynamic"
    c_get_create_conversation :: FunPtr CreateConversation -> CreateConversation

-- | Called just before a conv is freed.
type DestroyConversation = Conversation -> IO ()

foreign import ccall "wrapper"
    c_mk_destroy_conversation :: DestroyConversation -> IO (FunPtr DestroyConversation)

foreign import ccall "dynamic"
    c_get_destroy_conversation :: FunPtr DestroyConversation -> DestroyConversation

-- Write a message to a chat.  If this field is @c NULL, libpurple will
-- fall back to using #write_conv.
type WriteChat = Conversation -> Who -> Message -> [MessageFlag] -> UTCTime -> IO ()
type CWriteChat = Conversation -> CString -> CString -> CInt -> CTime -> IO ()
type Who = String
type Message = String

foreign import ccall "wrapper"
    c_mk_write_chat :: CWriteChat -> IO (FunPtr CWriteChat)

foreign import ccall "dynamic"
    c_get_write_chat :: FunPtr CWriteChat -> CWriteChat

hWriteChat :: WriteChat -> CWriteChat
hWriteChat f = \con cs1 cs2 ci ctime -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    let time = posixSecondsToUTCTime (realToFrac ctime :: POSIXTime)
        mfs = cMessageFlags ci
    f con s1 s2 mfs time

cWriteChat :: CWriteChat -> WriteChat
cWriteChat f = \con s1 s2 mfs time -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    let ctime = fromIntegral . (round :: (RealFrac a) => a -> Int) $ utcTimeToPOSIXSeconds time
        ci    = hMessageFlags mfs
    f con cs1 cs2 ci ctime

-- | Write a message to an IM conversation.  If this field is @c NULL,
-- libpurple will fall back to using #write_conv.
-- @see purple_conv_im_write()
type WriteIM = Conversation -> Who -> Message -> [MessageFlag] -> UTCTime -> IO ()
type CWriteIM = Conversation -> CString -> CString -> CInt -> CTime -> IO ()

foreign import ccall "wrapper"
    c_mk_write_im :: CWriteIM -> IO (FunPtr CWriteIM)

foreign import ccall "dynamic"
    c_get_write_im :: FunPtr CWriteIM -> CWriteIM

hWriteIM :: WriteIM -> CWriteIM
hWriteIM f = \con cs1 cs2 ci ctime -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    let time = posixSecondsToUTCTime $ realToFrac ctime
        mfs  = cMessageFlags ci
    f con s1 s2 mfs time

cWriteIM :: CWriteIM -> WriteIM
cWriteIM f = \con s1 s2 mfs time -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    let ctime = fromIntegral . (round :: (RealFrac a) => a -> Int) $ utcTimeToPOSIXSeconds time
        ci    = hMessageFlags mfs
    f con cs1 cs2 ci ctime


-- | Write a message to a conversation.  This is used rather than the
-- chat- or im-specific ops for errors, system messages (such as "x is
-- now know as y"), and as the fallback if #write_im and #write_chat
-- are not implemented.  It should be implemented, or the UI will miss
-- conversation error messages and your users will hate you.
type WriteConv = Conversation -> Name -> Alias -> Message -> [MessageFlag] -> UTCTime -> IO ()
type CWriteConv = Conversation -> CString -> CString -> CString -> CInt -> CTime -> IO ()
type Name = String
type Alias = String

foreign import ccall "wrapper"
    c_mk_write_conv :: CWriteConv -> IO (FunPtr CWriteConv)

foreign import ccall "dynamic"
    c_get_write_conv :: FunPtr CWriteConv -> CWriteConv

hWriteConv :: WriteConv -> CWriteConv
hWriteConv f = \con cs1 cs2 cs3 ci ctime -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    s3 <- peekCString cs3
    let time = posixSecondsToUTCTime $ realToFrac ctime
        mfs  = cMessageFlags ci
    f con s1 s2 s3 mfs time

cWriteConv :: CWriteConv -> WriteConv
cWriteConv f = \con s1 s2 s3 mfs time -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    let ctime = fromIntegral . (round :: (RealFrac a) => a -> Int) $ utcTimeToPOSIXSeconds time
        ci    = hMessageFlags mfs
    f con cs1 cs2 cs3 ci ctime

-- | Add buddies to a chat.
type ChatAddUsers = Conversation
                 -> [Buddy]         -- ^ A List of ChatBuddy structs
                 -> NewArrivals     -- ^ Whether join notices should be shown.
                                    -- Join notices are actually written to the
                                    -- conversation by ChatAddUsers
                 -> IO ()
type CChatAddUsers = Conversation -> Ptr C'GList -> CInt -> IO ()
type NewArrivals = Bool
type Buddy = Ptr ()

foreign import ccall "wrapper"
    c_mk_chat_add_users :: CChatAddUsers -> IO (FunPtr CChatAddUsers)

foreign import ccall "dynamic"
    c_get_chat_add_users :: FunPtr CChatAddUsers -> CChatAddUsers

hChatAddUsers :: ChatAddUsers -> CChatAddUsers
hChatAddUsers f = \con ptr ci -> do
    lis <- peek ptr >>= gListToList
    f con lis (ci == 1)

cChatAddUsers :: CChatAddUsers -> ChatAddUsers
cChatAddUsers f = \con lis b -> alloca $ \ptr -> do
    ptr' <- listToGList ptr lis
    f con ptr' (if b then 1 else 0)


-- | Rename the user in this chat named old_name to new_name. (The
-- rename message is written to the conversation by libpurple.)
type ChatRenameUser = Conversation -> OldName -> NewName -> NewAlias -> IO ()
type CChatRenameUser = Conversation -> CString -> CString -> CString -> IO ()
type OldName = String
type NewName = String
type NewAlias = String

foreign import ccall "wrapper"
    c_mk_chat_rename_user :: CChatRenameUser -> IO (FunPtr CChatRenameUser)

foreign import ccall "dynamic"
    c_get_chat_rename_user :: FunPtr CChatRenameUser -> CChatRenameUser

hChatRenameUser :: ChatRenameUser -> CChatRenameUser
hChatRenameUser f = \con cs1 cs2 cs3 -> do
    s1 <- peekCString cs1
    s2 <- peekCString cs2
    s3 <- peekCString cs3
    f con s1 s2 s3

cChatRenameUser :: CChatRenameUser -> ChatRenameUser
cChatRenameUser f = \con s1 s2 s3 -> do
    cs1 <- newCString s1
    cs2 <- newCString s2
    cs3 <- newCString s3
    f con cs1 cs2 cs3

-- | Remove users from a chat.
type ChatRemoveUsers = Conversation -> [Buddy] -> IO ()
type CChatRemoveUsers = Conversation -> Ptr C'GList -> IO ()

foreign import ccall "wrapper"
    c_mk_chat_remove_users :: CChatRemoveUsers -> IO (FunPtr CChatRemoveUsers)

foreign import ccall "dynamic"
    c_get_chat_remove_users :: FunPtr CChatRemoveUsers -> CChatRemoveUsers

hChatRemoveUsers :: ChatRemoveUsers -> CChatRemoveUsers
hChatRemoveUsers f = \con ptr -> do
    lis <- peek ptr >>= gListToList
    f con lis

cChatRemoveUsers :: CChatRemoveUsers -> ChatRemoveUsers
cChatRemoveUsers f = \con lis -> alloca $ \ptr -> do
    ptr' <- listToGList ptr lis
    f con ptr'

-- | Called when a users flags are changed
type ChatUpdateUser = Conversation -> User -> IO ()
type CChatUpdateUser = Conversation -> CString -> IO ()
type User = String

foreign import ccall "wrapper"
    c_mk_chat_update_user :: CChatUpdateUser -> IO (FunPtr CChatUpdateUser)

foreign import ccall "dynamic"
    c_get_chat_update_user :: FunPtr CChatUpdateUser -> CChatUpdateUser

hChatUpdateUser :: ChatUpdateUser -> CChatUpdateUser
hChatUpdateUser f = \con cs1 -> do
    s1 <- peekCString cs1
    f con s1

cChatUpdateUser :: CChatUpdateUser -> ChatUpdateUser
cChatUpdateUser f = \con s1 -> do
    cs1 <- newCString s1
    f con cs1

-- | Present this conversation to the user; for example, by displaying
-- the IM dialog.
type Present = Conversation -> IO ()

foreign import ccall "wrapper"
    c_mk_present :: Present -> IO (FunPtr Present)

foreign import ccall "dynamic"
    c_get_present :: FunPtr Present -> Present

-- If this UI has a concept of focus (as in a windowing system) and
-- this conversation has the focus, return TRUE; otherwise, return FALSE.
type HasFocus  = Conversation -> IO Bool
type CHasFocus = Conversation -> IO CInt

foreign import ccall "wrapper"
    c_mk_has_focus :: CHasFocus -> IO (FunPtr CHasFocus)

foreign import ccall "dynamic"
    c_get_has_focus :: FunPtr CHasFocus -> CHasFocus

hHasFocus :: HasFocus -> CHasFocus
hHasFocus f = fmap (\b -> if b then 1 else 0) . f

cHasFocus :: CHasFocus -> HasFocus
cHasFocus f = fmap (== 1) . f

-- | Custom Smilies
type CustomSmileyAdd = Conversation -> Smile -> Remote -> IO Bool
type CCustomSmileyAdd = Conversation -> CString -> CInt -> IO CInt
type Smile = String
type Remote = Bool

foreign import ccall "wrapper"
    c_mk_custom_smiley_add :: CCustomSmileyAdd -> IO (FunPtr CCustomSmileyAdd)

foreign import ccall "dynamic"
    c_get_custom_smiley_add :: FunPtr CCustomSmileyAdd -> CCustomSmileyAdd

hCustomSmileyAdd :: CustomSmileyAdd -> CCustomSmileyAdd
hCustomSmileyAdd f = \con cs ci -> do
    s <- peekCString cs
    (\b -> if b then 1 else 0) `fmap` f con s (1 == ci)

cCustomSmileyAdd :: CCustomSmileyAdd -> CustomSmileyAdd
cCustomSmileyAdd f = \con s b -> do
    cs <- newCString s
    (1 ==) `fmap` f con cs (if b then 1 else 0)

type CustomSmileyWrite  = Conversation -> Smile -> Data -> Size -> IO ()
type CCustomSmileyWrite = Conversation -> CString -> Data -> CUInt -> IO ()
type Data = Ptr ()
type Size = Int

foreign import ccall "wrapper"
    c_mk_custom_smiley_write :: CCustomSmileyWrite -> IO (FunPtr CCustomSmileyWrite)

foreign import ccall "dynamic"
    c_get_custom_smiley_write :: FunPtr CCustomSmileyWrite -> CCustomSmileyWrite

hCustomSmileyWrite :: CustomSmileyWrite -> CCustomSmileyWrite
hCustomSmileyWrite f = \con cs1 da cui -> do
    s1 <- peekCString cs1
    f con s1 da (fromIntegral cui)

cCustomSmileyWrite :: CCustomSmileyWrite -> CustomSmileyWrite
cCustomSmileyWrite f = \con s1 da i -> do
    cs1 <- newCString s1
    f con cs1 da (fromIntegral i)

type CustomSmileyClose = Conversation -> Smile -> IO ()
type CCustomSmileyClose = Conversation -> CString -> IO ()

foreign import ccall "wrapper"
    c_mk_custom_smiley_close :: CCustomSmileyClose -> IO (FunPtr CCustomSmileyClose)

foreign import ccall "dynamic"
    c_get_custom_smiley_close :: FunPtr CCustomSmileyClose -> CCustomSmileyClose

hCustomSmileyClose :: CustomSmileyClose -> CCustomSmileyClose
hCustomSmileyClose f = \con cs1 -> do
    s1 <- peekCString cs1
    f con s1

cCustomSmileyClose :: CCustomSmileyClose -> CustomSmileyClose
cCustomSmileyClose f = \con s1 -> do
    cs1 <- newCString s1
    f con cs1

-- | Prompt the user for confirmation to send message. This function
-- should arrange for the message to be sent if the user accepts.  If
-- this field is NULL, libpurple will fall back to using
-- \"purple_request_action()\".
type SendConfirm = Conversation -> Message -> IO ()
type CSendConfirm = Conversation -> CString -> IO ()

foreign import ccall "wrapper"
    c_mk_send_confirm :: CSendConfirm -> IO (FunPtr CSendConfirm)

foreign import ccall "dynamic"
    c_get_send_confirm :: FunPtr CSendConfirm -> CSendConfirm

hSendConfirm :: SendConfirm -> CSendConfirm
hSendConfirm f = \con cs -> do
    s <- peekCString cs
    f con s

cSendConfirm :: CSendConfirm -> SendConfirm
cSendConfirm f = \con s -> do
    cs <- newCString s
    f con cs

-- | Flags applicable to a message. Most will have send, recv or system.
data MessageFlag
    = MessageSend -- Outgoing message.
    | MessageRecv -- Incoming message.
    | MessageSystem -- System message.
    | MessageAutoResp -- Auto response.
    | MessageActiveOnly -- Hint to the UI that this message
                        -- should not be shown in conversations
                        -- which are only open for internal UI
                        -- purposes (e.g. for contact-aware
                        -- conversations).
    | MessageNick -- Contains your nick.
    | MessageNoLog -- Do not log.
    | MessageWhisper -- Whispered message.
    | MessageError -- Error message.
    | MessageDelayed -- Delayed message.
    | MessageRaw -- "Raw" message - don't apply formatting
    | MessageImages -- Message contains images
    | MessageNotify -- Message is a notification
    | MessageNoLinkify -- Message should not be auto-linkified @since 2.1.0
    | MessageInvisible -- Message should not be displayed

showMessageFlag :: MessageFlag -> CInt
showMessageFlag MessageSend = 0x0001
showMessageFlag MessageRecv = 0x0002
showMessageFlag MessageSystem = 0x0004
showMessageFlag MessageAutoResp   = 0x0008
showMessageFlag MessageActiveOnly = 0x0010
showMessageFlag MessageNick = 0x0020
showMessageFlag MessageNoLog = 0x0040
showMessageFlag MessageWhisper = 0x0080
showMessageFlag MessageError = 0x0200
showMessageFlag MessageDelayed = 0x0400
showMessageFlag MessageRaw = 0x0800
showMessageFlag MessageImages = 0x1000
showMessageFlag MessageNotify = 0x2000
showMessageFlag MessageNoLinkify = 0x4000
showMessageFlag MessageInvisible = 0x8000

hMessageFlags :: [MessageFlag] -> CInt
hMessageFlags = fromMaybe 0 . foldr bitwiseOr Nothing . map Just

  where bitwiseOr :: Maybe MessageFlag -> Maybe CInt -> Maybe CInt
        bitwiseOr (Just mf) Nothing  = Just $ showMessageFlag mf
        bitwiseOr (Just mf) (Just i) = Just $ showMessageFlag mf .|. i
        bitwiseOr _ _ = error "hMessageFlags: No value"

readMessageFlag :: CInt -> MessageFlag
readMessageFlag 0x0001 = MessageSend
readMessageFlag 0x0002 = MessageRecv
readMessageFlag 0x0004 = MessageSystem
readMessageFlag 0x0008 = MessageAutoResp
readMessageFlag 0x0010 = MessageActiveOnly
readMessageFlag 0x0020 = MessageNick
readMessageFlag 0x0040 = MessageNoLog
readMessageFlag 0x0080 = MessageWhisper
readMessageFlag 0x0200 = MessageError
readMessageFlag 0x0400 = MessageDelayed
readMessageFlag 0x0800 = MessageRaw
readMessageFlag 0x1000 = MessageImages
readMessageFlag 0x2000 = MessageNotify
readMessageFlag 0x4000 = MessageNoLinkify
readMessageFlag 0x8000 = MessageInvisible
readMessageFlag _      = error "readMessageFlag: Invalid MessageFlag"

cMessageFlags :: CInt -> [MessageFlag]
cMessageFlags ci = filter bitwiseAnd allMessageFlags 

  where bitwiseAnd      :: MessageFlag -> Bool
        bitwiseAnd mf   = 1 == showMessageFlag mf .&. ci
        allMessageFlags = [ MessageSend
                          , MessageRecv
                          , MessageSystem
                          , MessageAutoResp
                          , MessageActiveOnly
                          , MessageNick
                          , MessageNoLog
                          , MessageWhisper
                          , MessageError
                          , MessageDelayed
                          , MessageRaw
                          , MessageImages
                          , MessageNotify
                          , MessageNoLinkify
                          , MessageInvisible
                          ]
