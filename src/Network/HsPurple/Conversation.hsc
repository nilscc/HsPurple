-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.Conversation
    (
      ConversationUiOps (..)

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

    -- * UI Registration Functions
    , setConversationUiOps
    , setConversationsUiOps
    , getConversationUiOps

    -- * Conversations Subsystem
    , initConversations
    , uninitConversations
    , conversationsGetHandle

    -- * Conversation API
    , conversationNew
    , conversationDestroy
    , conversationPresent
    , conversationGetType
    , conversationSetAccount
    , conversationGetAccount
    , conversationGetConnection
    , conversationSetTitle
    , conversationGetTitle
    , conversationAutosetTitle
    , conversationSetName
    , conversationGetName
    , conversationSetLogging
    , conversationIsLogging
    , conversationCloseLogs
    , conversationGetImData
    , conversationGetChatData
    , conversationSetData
    , conversationGetData

    , getConversations
    , getIms
    , getChats

    , conversationWrite
    , conversationSetFeatures
    , conversationGetFeatures
    , conversationHasFocus
    , conversationUpdate
    , conversationForeach

    , conversationGetMessageHistory
    , conversationClearMessageHistory

    , conversationMessageGetSender
    , conversationMessageGetMessage
    , conversationMessageGetTimestamp

    -- * Enums

    , ConversationType

    , conversationTypeUnknown
    , conversationTypeIm
    , conversationTypeChat
    , conversationTypeMisc
    , conversationTypeAny

    , TypingState

    , typeStateNotTyping
    , typeStateTyping
    , typeStateTyped

    , MessageFlags

    , messageSend
    , messageRecv
    , messageSystem
    , messageAutoResp
    , messageActiveOnly
    , messageNick
    , messageNoLog
    , messageWhisper
    , messageError
    , messageDelayed
    , messageRaw
    , messageImages
    , messageNotify
    , messageNoLinkify
    , messageInvisible

    , ConvChatBuddyFlags

    , cbflagsNone
    , cbflagsVoice
    , cbflagsHalfop
    , cbflagsOp
    , cbflagsFounder
    , cbflagsTyping

    , ConvUpdateType

    , convUpdateAdd
    , convUpdateRemove
    , convUpdateAccount
    , convUpdateTyping
    , convUpdateUnseen
    , convUpdateLogging
    , convUpdateTopic
    , convAccountOnline
    , convAccountOffline
    , convUpdateAway
    , convUpdateIcon
    , convUpdateTitle
    , convUpdateChatleft
    , convUpdateFeatures

    ) where

import Foreign
import Foreign.C
import Network.HsPurple.UiOps.ConversationUiOps
import Network.HsPurple.GLib.GList

import Data.Time
import Data.Time.Clock.POSIX

#include <purple.h>
#include <bindings.dsl.h>

type Account = Ptr ()
type Connection = Ptr ()
type ConvIm = Ptr ()
type ConvChat = Ptr ()
type ConvMessage = Ptr ()
type UIHandle = Ptr ()
type ConnectionFlags = Int


--------------------------------------------------------------------------------
-- Conversations Subsystem
--------------------------------------------------------------------------------

#ccall purple_conversations_init , IO ()
-- | Initializes the conversation subsystem. 
initConversations :: IO ()
initConversations = c'purple_conversations_init

#ccall purple_conversations_uninit , IO ()
-- | Uninitializes the conversation subsystem.
uninitConversations :: IO ()
uninitConversations = c'purple_conversations_uninit

#ccall purple_conversations_get_handle , IO (Ptr ())
-- | Returns the conversation subsystem handle. 
conversationsGetHandle :: IO UIHandle
conversationsGetHandle = c'purple_conversations_get_handle



--------------------------------------------------------------------------------
-- UI Registration Functions
--------------------------------------------------------------------------------

#ccall purple_conversation_set_ui_ops , Conversation -> Ptr () -> IO ()
-- | Sets the specified conversation's UI operations structure. 
setConversationUiOps :: Conversation -> ConversationUiOps -> IO ()
setConversationUiOps c ui = do
    ptr <- malloc
    poke ptr ui
    c'purple_conversation_set_ui_ops c (castPtr ptr)

#ccall purple_conversations_set_ui_ops , Ptr () -> IO ()
-- | Sets the default conversation UI operations structure. 
setConversationsUiOps :: ConversationUiOps -> IO ()
setConversationsUiOps ui = do
    ptr <- malloc
    poke ptr ui
    c'purple_conversations_set_ui_ops (castPtr ptr)

#ccall purple_conversation_get_ui_ops , Conversation -> IO (Ptr ())
-- | Returns the specified conversation's UI operations structure. 
getConversationUiOps :: Conversation -> IO ConversationUiOps
getConversationUiOps c = do
    ptr <- c'purple_conversation_get_ui_ops c
    peek (castPtr ptr)



--------------------------------------------------------------------------------
-- Conversation API
--------------------------------------------------------------------------------

-- #define 	PURPLE_CONV_IM(c)   (purple_conversation_get_im_data(c))
-- #define 	PURPLE_CONV_CHAT(c)   (purple_conversation_get_chat_data(c))

#ccall purple_conversation_new , CInt -> Account -> CString -> IO Conversation
-- | Creates a new conversation of the specified type. 
conversationNew :: ConversationType
                -> Account
                -> String           -- ^ Name
                -> IO Conversation
conversationNew t a s = newCString s >>= c'purple_conversation_new (fi t) a

#ccall purple_conversation_destroy , Conversation -> IO ()
-- | Destroys the specified conversation and removes it from the parent window. 
conversationDestroy :: Conversation -> IO ()
conversationDestroy = c'purple_conversation_destroy

#ccall purple_conversation_present , Conversation -> IO ()
-- | Present a conversation to the user. 
conversationPresent :: Conversation -> IO ()
conversationPresent = c'purple_conversation_present

#ccall purple_conversation_get_type , Conversation -> IO CInt
-- | Returns the specified conversation's type. 
conversationGetType :: Conversation -> IO ConversationType
conversationGetType = fmap fi . c'purple_conversation_get_type

#ccall purple_conversation_set_account , Conversation -> Account -> IO ()
-- | Sets the specified conversation's purple_account. 
conversationSetAccount :: Conversation -> Account -> IO ()
conversationSetAccount = c'purple_conversation_set_account

#ccall purple_conversation_get_account , Conversation -> IO Account
-- | Returns the specified conversation's purple_account. 
conversationGetAccount :: Conversation -> IO Account
conversationGetAccount = c'purple_conversation_get_account

#ccall purple_conversation_get_gc , Conversation -> IO Connection
-- | Returns the specified conversation's purple_connection. 
conversationGetConnection :: Conversation -> IO Connection
conversationGetConnection = c'purple_conversation_get_gc

#ccall purple_conversation_set_title , Conversation -> CString -> IO ()
-- | Sets the specified conversation's title. 
conversationSetTitle :: Conversation
                     -> String          -- ^ Title
                     -> IO ()
conversationSetTitle c s = newCString s >>= c'purple_conversation_set_title c

#ccall purple_conversation_get_title , Conversation -> IO CString
-- | Returns the specified conversation's title. 
conversationGetTitle :: Conversation -> IO String
conversationGetTitle c = c'purple_conversation_get_title c >>= peekCString

#ccall purple_conversation_autoset_title , Conversation -> IO ()
-- | Automatically sets the specified conversation's title. 
conversationAutosetTitle :: Conversation -> IO ()
conversationAutosetTitle = c'purple_conversation_autoset_title

#ccall purple_conversation_set_name , Conversation -> CString -> IO ()
-- | Sets the specified conversation's name. 
conversationSetName :: Conversation
                    -> String           -- ^ Name
                    -> IO ()
conversationSetName c s = newCString s >>= c'purple_conversation_set_name c

#ccall purple_conversation_get_name , Conversation -> IO CString
-- | Returns the specified conversation's name. 
conversationGetName :: Conversation -> IO String
conversationGetName c = c'purple_conversation_get_name c >>= peekCString

#ccall purple_conversation_set_logging , Conversation -> CInt -> IO ()
-- | Enables or disables logging for this conversation. 
conversationSetLogging :: Conversation -> Bool -> IO ()
conversationSetLogging c b = c'purple_conversation_set_logging c (if b then 1 else 0)

#ccall purple_conversation_is_logging , Conversation -> IO CInt
-- | Returns whether or not logging is enabled for this conversation. 
conversationIsLogging :: Conversation -> IO Bool
conversationIsLogging = fmap (1 ==) . c'purple_conversation_is_logging

#ccall purple_conversation_close_logs , Conversation -> IO ()
-- | Closes any open logs for this conversation. 
conversationCloseLogs :: Conversation -> IO ()
conversationCloseLogs = c'purple_conversation_close_logs

#ccall purple_conversation_get_im_data , Conversation -> IO ConvIm
-- | Returns the specified conversation's IM-specific data. 
conversationGetImData :: Conversation -> IO ConvIm
conversationGetImData = c'purple_conversation_get_im_data

#ccall purple_conversation_get_chat_data , Conversation -> IO ConvChat
-- | Returns the specified conversation's chat-specific data. 
conversationGetChatData :: Conversation -> IO ConvChat
conversationGetChatData = c'purple_conversation_get_chat_data

#ccall purple_conversation_set_data , Conversation -> CString -> Data -> IO ()
-- | Sets extra data for a conversation. 
conversationSetData :: Conversation
                    -> String       -- ^ Key
                    -> Data
                    -> IO ()
conversationSetData c s d = do
    cs <- newCString s
    c'purple_conversation_set_data c cs d

#ccall purple_conversation_get_data , Conversation -> CString -> IO (Ptr ())
-- | Returns extra data in a conversation. 
conversationGetData :: Conversation
                    -> String -- ^ Key
                    -> IO Data
conversationGetData c s = newCString s >>= c'purple_conversation_get_data c

#ccall purple_get_conversations , IO (Ptr <GList>)
-- | Returns a list of all conversations. 
getConversations :: IO [Conversation]
getConversations = c'purple_get_conversations >>= gListToList

#ccall purple_get_ims , IO (Ptr <GList>)
-- | Returns a list of all IMs. 
getIms :: IO [ConvIm]
getIms = c'purple_get_ims >>= gListToList

#ccall purple_get_chats , IO (Ptr <GList>)
-- | Returns a list of all chats. 
getChats :: IO [ConvChat]
getChats = c'purple_get_chats >>= gListToList

#ccall purple_find_conversation_with_account , CInt -> CString -> Account -> IO Conversation
-- | Finds a conversation with the specified type, name, and Purple account. 
findConversationWithAccount :: ConversationType
                            -> String           -- Name
                            -> Account
                            -> IO Conversation
findConversationWithAccount t s a = do
    cs <- newCString s
    c'purple_find_conversation_with_account (fi t) cs a

#ccall purple_conversation_write , Conversation -> CString -> CString -> CInt -> CTime -> IO ()
-- | Writes to a conversation window. 
conversationWrite :: Conversation
                  -> String         -- ^ Who
                  -> String         -- ^ Message
                  -> MessageFlags
                  -> UTCTime
                  -> IO ()
conversationWrite c s1 s2 m t = do
    cs1 <- newCString s1
    cs2 <- newCString s2
    let time = fromIntegral . (round :: (RealFrac a) => a -> Int) $ utcTimeToPOSIXSeconds t
    c'purple_conversation_write c cs1 cs2 (fi m) time

#ccall purple_conversation_set_features , Conversation -> CInt -> IO ()
-- | Set the features as supported for the given conversation. 
conversationSetFeatures :: Conversation -> ConnectionFlags -> IO ()
conversationSetFeatures c f = c'purple_conversation_set_features c (fi f)

#ccall purple_conversation_get_features , Conversation -> IO CInt
-- | Get the features supported by the given conversation. 
conversationGetFeatures :: Conversation -> IO ConnectionFlags
conversationGetFeatures = fmap fi . c'purple_conversation_get_features

#ccall purple_conversation_has_focus , Conversation -> IO CInt
-- | Determines if a conversation has focus. 
conversationHasFocus :: Conversation -> IO Bool
conversationHasFocus = fmap (1 ==) . c'purple_conversation_has_focus

#ccall purple_conversation_update , Conversation -> CInt -> IO ()
-- | Updates the visual status and UI of a conversation. 
conversationUpdate :: Conversation -> ConvUpdateType -> IO ()
conversationUpdate c t = c'purple_conversation_update c (fi t)

#callback ConvForeach , Conversation -> IO ()
#ccall purple_conversation_foreach , <ConvForeach> -> IO ()
-- | Calls a function on each conversation. 
conversationForeach :: (Conversation -> IO ()) -> IO ()
conversationForeach f = mk'ConvForeach f >>= c'purple_conversation_foreach

#ccall purple_conversation_get_message_history , Conversation -> IO (Ptr <GList>)
-- | Retrieve the message history of a conversation. 
conversationGetMessageHistory :: Conversation -> IO [String]
conversationGetMessageHistory c = do
    gl <- c'purple_conversation_get_message_history c
    mapM getString =<< gListToList gl
  where getString p = peekCString $ castPtr p

#ccall purple_conversation_clear_message_history , Conversation -> IO ()
-- | Clear the message history of a conversation. 
conversationClearMessageHistory :: Conversation -> IO ()
conversationClearMessageHistory = c'purple_conversation_clear_message_history

#ccall purple_conversation_message_get_sender , ConvMessage -> IO CString
-- | Get the sender from a PurpleConvMessage. 
conversationMessageGetSender :: ConvMessage -> IO String
conversationMessageGetSender c = c'purple_conversation_message_get_sender c >>= peekCString

#ccall purple_conversation_message_get_message , ConvMessage -> IO CString
-- | Get the message from a PurpleConvMessage. 
conversationMessageGetMessage :: ConvMessage -> IO String
conversationMessageGetMessage c = c'purple_conversation_message_get_message c >>= peekCString

#ccall purple_conversation_message_get_flags , ConvMessage -> IO CInt
-- | Get the message-flags of a PurpleConvMessage. 
conversationMessageGetFlags :: ConvMessage -> IO MessageFlags
conversationMessageGetFlags = fmap fi . c'purple_conversation_message_get_flags

#ccall purple_conversation_message_get_timestamp , ConvMessage -> IO CTime
-- | Get the timestamp of a PurpleConvMessage. 
conversationMessageGetTimestamp :: ConvMessage -> IO UTCTime
conversationMessageGetTimestamp = fmap toUtc . c'purple_conversation_message_get_timestamp
  where toUtc = posixSecondsToUTCTime . realToFrac



--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

type ConversationType = Int

#num PURPLE_CONV_TYPE_UNKNOWN
#num PURPLE_CONV_TYPE_IM
#num PURPLE_CONV_TYPE_CHAT
#num PURPLE_CONV_TYPE_MISC
#num PURPLE_CONV_TYPE_ANY

conversationTypeUnknown :: ConversationType
conversationTypeIm :: ConversationType
conversationTypeChat :: ConversationType
conversationTypeMisc :: ConversationType
conversationTypeAny :: ConversationType

conversationTypeUnknown = c'PURPLE_CONV_TYPE_UNKNOWN
conversationTypeIm      = c'PURPLE_CONV_TYPE_IM
conversationTypeChat    = c'PURPLE_CONV_TYPE_CHAT
conversationTypeMisc    = c'PURPLE_CONV_TYPE_MISC
conversationTypeAny     = c'PURPLE_CONV_TYPE_ANY

type TypingState = Int

#num PURPLE_NOT_TYPING
#num PURPLE_TYPING
#num PURPLE_TYPED

typeStateNotTyping :: TypingState
typeStateTyping :: TypingState
typeStateTyped :: TypingState

typeStateNotTyping = c'PURPLE_NOT_TYPING
typeStateTyping    = c'PURPLE_TYPING
typeStateTyped     = c'PURPLE_TYPED

type MessageFlags = Int

#num PURPLE_MESSAGE_SEND
#num PURPLE_MESSAGE_RECV
#num PURPLE_MESSAGE_SYSTEM
#num PURPLE_MESSAGE_AUTO_RESP
#num PURPLE_MESSAGE_ACTIVE_ONLY
#num PURPLE_MESSAGE_NICK
#num PURPLE_MESSAGE_NO_LOG
#num PURPLE_MESSAGE_WHISPER
#num PURPLE_MESSAGE_ERROR
#num PURPLE_MESSAGE_DELAYED
#num PURPLE_MESSAGE_RAW
#num PURPLE_MESSAGE_IMAGES
#num PURPLE_MESSAGE_NOTIFY
#num PURPLE_MESSAGE_NO_LINKIFY
#num PURPLE_MESSAGE_INVISIBLE

messageSend :: MessageFlags
messageRecv :: MessageFlags
messageSystem :: MessageFlags
messageAutoResp :: MessageFlags
messageActiveOnly :: MessageFlags
messageNick :: MessageFlags
messageNoLog :: MessageFlags
messageWhisper :: MessageFlags
messageError :: MessageFlags
messageDelayed :: MessageFlags
messageRaw :: MessageFlags
messageImages :: MessageFlags
messageNotify :: MessageFlags
messageNoLinkify :: MessageFlags
messageInvisible :: MessageFlags

messageSend         = c'PURPLE_MESSAGE_SEND
messageRecv         = c'PURPLE_MESSAGE_RECV
messageSystem       = c'PURPLE_MESSAGE_SYSTEM
messageAutoResp     = c'PURPLE_MESSAGE_AUTO_RESP
messageActiveOnly   = c'PURPLE_MESSAGE_ACTIVE_ONLY
messageNick         = c'PURPLE_MESSAGE_NICK
messageNoLog        = c'PURPLE_MESSAGE_NO_LOG
messageWhisper      = c'PURPLE_MESSAGE_WHISPER
messageError        = c'PURPLE_MESSAGE_ERROR
messageDelayed      = c'PURPLE_MESSAGE_DELAYED
messageRaw          = c'PURPLE_MESSAGE_RAW
messageImages       = c'PURPLE_MESSAGE_IMAGES
messageNotify       = c'PURPLE_MESSAGE_NOTIFY
messageNoLinkify    = c'PURPLE_MESSAGE_NO_LINKIFY
messageInvisible    = c'PURPLE_MESSAGE_INVISIBLE

type ConvChatBuddyFlags = Int

#num PURPLE_CBFLAGS_NONE
#num PURPLE_CBFLAGS_VOICE
#num PURPLE_CBFLAGS_HALFOP
#num PURPLE_CBFLAGS_OP
#num PURPLE_CBFLAGS_FOUNDER
#num PURPLE_CBFLAGS_TYPING

cbflagsNone :: ConvChatBuddyFlags
cbflagsVoice :: ConvChatBuddyFlags
cbflagsHalfop :: ConvChatBuddyFlags
cbflagsOp :: ConvChatBuddyFlags
cbflagsFounder :: ConvChatBuddyFlags
cbflagsTyping :: ConvChatBuddyFlags

cbflagsNone     = c'PURPLE_CBFLAGS_NONE
cbflagsVoice    = c'PURPLE_CBFLAGS_VOICE
cbflagsHalfop   = c'PURPLE_CBFLAGS_HALFOP
cbflagsOp       = c'PURPLE_CBFLAGS_OP
cbflagsFounder  = c'PURPLE_CBFLAGS_FOUNDER
cbflagsTyping   = c'PURPLE_CBFLAGS_TYPING


type ConvUpdateType = Int

#num PURPLE_CONV_UPDATE_ADD
#num PURPLE_CONV_UPDATE_REMOVE
#num PURPLE_CONV_UPDATE_ACCOUNT
#num PURPLE_CONV_UPDATE_TYPING
#num PURPLE_CONV_UPDATE_UNSEEN
#num PURPLE_CONV_UPDATE_LOGGING
#num PURPLE_CONV_UPDATE_TOPIC
#num PURPLE_CONV_ACCOUNT_ONLINE
#num PURPLE_CONV_ACCOUNT_OFFLINE
#num PURPLE_CONV_UPDATE_AWAY
#num PURPLE_CONV_UPDATE_ICON
#num PURPLE_CONV_UPDATE_TITLE
#num PURPLE_CONV_UPDATE_CHATLEFT
#num PURPLE_CONV_UPDATE_FEATURES

convUpdateAdd :: ConvUpdateType
convUpdateRemove :: ConvUpdateType
convUpdateAccount :: ConvUpdateType
convUpdateTyping :: ConvUpdateType
convUpdateUnseen :: ConvUpdateType
convUpdateLogging :: ConvUpdateType
convUpdateTopic :: ConvUpdateType
convAccountOnline :: ConvUpdateType
convAccountOffline :: ConvUpdateType
convUpdateAway :: ConvUpdateType
convUpdateIcon :: ConvUpdateType
convUpdateTitle :: ConvUpdateType
convUpdateChatleft :: ConvUpdateType
convUpdateFeatures :: ConvUpdateType

convUpdateAdd       = c'PURPLE_CONV_UPDATE_ADD
convUpdateRemove    = c'PURPLE_CONV_UPDATE_REMOVE
convUpdateAccount   = c'PURPLE_CONV_UPDATE_ACCOUNT
convUpdateTyping    = c'PURPLE_CONV_UPDATE_TYPING
convUpdateUnseen    = c'PURPLE_CONV_UPDATE_UNSEEN
convUpdateLogging   = c'PURPLE_CONV_UPDATE_LOGGING
convUpdateTopic     = c'PURPLE_CONV_UPDATE_TOPIC
convAccountOnline   = c'PURPLE_CONV_ACCOUNT_ONLINE
convAccountOffline  = c'PURPLE_CONV_ACCOUNT_OFFLINE
convUpdateAway      = c'PURPLE_CONV_UPDATE_AWAY
convUpdateIcon      = c'PURPLE_CONV_UPDATE_ICON
convUpdateTitle     = c'PURPLE_CONV_UPDATE_TITLE
convUpdateChatleft  = c'PURPLE_CONV_UPDATE_CHATLEFT
convUpdateFeatures  = c'PURPLE_CONV_UPDATE_FEATURES
