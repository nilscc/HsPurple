-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Structs.ConversationUiOps
    (
      ConversationUiOps (..)
    , MessageFlag (..)
    , showMessageFlag
    , readMessageFlag

    -- * Function types
    , CreateConversation
    , DestroyConversation
    , WriteChar
    , WriteIM
    ) where

import Data.Bits
import Data.Maybe (fromMaybe)

import Foreign
import Foreign.C

-- | Conversation operations and events.
--
-- Any UI representing a conversation must assign a filled-out
-- PurpleConversationUiOps structure to the PurpleConversation.
data ConversationUiOps = ConversationUiOps
    {
    }

-- | Called when @a conv is created (but before the @ref
-- conversation-created signal is emitted).
type CreateConversation = Conversation -> IO ()
type Conversation = Ptr ()

{-
	/** Called just before @a conv is freed. */
	void (*destroy_conversation)(PurpleConversation *conv);
-}

-- | Called just before a conv is freed.
type DestroyConversation = Conversation -> IO ()

{-
	/** Write a message to a chat.  If this field is @c NULL, libpurple will
	 *  fall back to using #write_conv.
	 *  @see purple_conv_chat_write()
	 */
	void (*write_chat)(PurpleConversation *conv, const char *who,
	                   const char *message, PurpleMessageFlags flags,
	                   time_t mtime);
-}

-- Write a message to a chat.  If this field is @c NULL, libpurple will
-- fall back to using #write_conv.
type WriteChar = Conversation -> Who -> Message -> [MessageFlag] -> CTime -> IO ()
type Who = String
type Message = String

{-
	/** Write a message to an IM conversation.  If this field is @c NULL,
	 *  libpurple will fall back to using #write_conv.
	 *  @see purple_conv_im_write()
	 */
	void (*write_im)(PurpleConversation *conv, const char *who,
	                 const char *message, PurpleMessageFlags flags,
	                 time_t mtime);
-}

type WriteIM = Conversation -> Who -> Message -> [MessageFlag] -> CTime -> IO ()

{-
	/** Write a message to a conversation.  This is used rather than the
	 *  chat- or im-specific ops for errors, system messages (such as "x is
	 *  now know as y"), and as the fallback if #write_im and #write_chat
	 *  are not implemented.  It should be implemented, or the UI will miss
	 *  conversation error messages and your users will hate you.
	 *
	 *  @see purple_conversation_write()
	 */
	void (*write_conv)(PurpleConversation *conv,
	                   const char *name,
	                   const char *alias,
	                   const char *message,
	                   PurpleMessageFlags flags,
	                   time_t mtime);

	/** Add @a cbuddies to a chat.
	 *  @param cbuddies      A @c GList of #PurpleConvChatBuddy structs.
	 *  @param new_arrivals  Whether join notices should be shown.
	 *                       (Join notices are actually written to the
	 *                       conversation by #purple_conv_chat_add_users().)
	 */
	void (*chat_add_users)(PurpleConversation *conv,
	                       GList *cbuddies,
	                       gboolean new_arrivals);
	/** Rename the user in this chat named @a old_name to @a new_name.  (The
	 *  rename message is written to the conversation by libpurple.)
	 *  @param new_alias  @a new_name's new alias, if they have one.
	 *  @see purple_conv_chat_add_users()
	 */
	void (*chat_rename_user)(PurpleConversation *conv, const char *old_name,
	                         const char *new_name, const char *new_alias);
	/** Remove @a users from a chat.
	 *  @param users    A @c GList of <tt>const char *</tt>s.
	 *  @see purple_conv_chat_rename_user()
	 */
	void (*chat_remove_users)(PurpleConversation *conv, GList *users);
	/** Called when a user's flags are changed.
	 *  @see purple_conv_chat_user_set_flags()
	 */
	void (*chat_update_user)(PurpleConversation *conv, const char *user);

	/** Present this conversation to the user; for example, by displaying
	 *  the IM dialog.
	 */
	void (*present)(PurpleConversation *conv);

	/** If this UI has a concept of focus (as in a windowing system) and
	 *  this conversation has the focus, return @c TRUE; otherwise, return
	 *  @c FALSE.
	 */
	gboolean (*has_focus)(PurpleConversation *conv);

	/* Custom Smileys */
	gboolean (*custom_smiley_add)(PurpleConversation *conv, const char *smile, gboolean remote);
	void (*custom_smiley_write)(PurpleConversation *conv, const char *smile,
	                            const guchar *data, gsize size);
	void (*custom_smiley_close)(PurpleConversation *conv, const char *smile);

	/** Prompt the user for confirmation to send @a message.  This function
	 *  should arrange for the message to be sent if the user accepts.  If
	 *  this field is @c NULL, libpurple will fall back to using
	 *  #purple_request_action().
	 */
	void (*send_confirm)(PurpleConversation *conv, const char *message);

	void (*_purple_reserved1)(void);
	void (*_purple_reserved2)(void);
	void (*_purple_reserved3)(void);
	void (*_purple_reserved4)(void);
};

-}

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
