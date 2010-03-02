-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.UiOps.ConnectionUiOps
    (
    ) where

import Foreign
import Foreign.C

{-
/**
 * Connection UI operations.  Used to notify the user of changes to
 * connections, such as being disconnected, and to respond to the
 * underlying network connection appearing and disappearing.  UIs should
 * call #purple_connections_set_ui_ops() with an instance of this struct.
 *
 * @see @ref ui-ops
 */
typedef struct
{
	/**
	 * When an account is connecting, this operation is called to notify
	 * the UI of what is happening, as well as which @a step out of @a
	 * step_count has been reached (which might be displayed as a progress
	 * bar).
	 * @see #purple_connection_update_progress
	 */
	void (*connect_progress)(PurpleConnection *gc,
	                         const char *text,
	                         size_t step,
	                         size_t step_count);

	/**
	 * Called when a connection is established (just before the
	 * @ref signed-on signal).
	 */
	void (*connected)(PurpleConnection *gc);

	/**
	 * Called when a connection is ended (between the @ref signing-off
	 * and @ref signed-off signals).
	 */
	void (*disconnected)(PurpleConnection *gc);

	/**
	 * Used to display connection-specific notices.  (Pidgin's Gtk user
	 * interface implements this as a no-op; #purple_connection_notice(),
	 * which uses this operation, is not used by any of the protocols
	 * shipped with libpurple.)
	 */
	void (*notice)(PurpleConnection *gc, const char *text);

	/**
	 * Called when an error causes a connection to be disconnected.
	 * Called before #disconnected.
	 * @param text  a localized error message.
	 * @see #purple_connection_error
	 * @deprecated in favour of
	 *             #PurpleConnectionUiOps.report_disconnect_reason.
	 */
	void (*report_disconnect)(PurpleConnection *gc, const char *text);

	/**
	 * Called when libpurple discovers that the computer's network
	 * connection is active.  On Linux, this uses Network Manager if
	 * available; on Windows, it uses Win32's network change notification
	 * infrastructure.
	 */
	void (*network_connected)(void);

	/**
	 * Called when libpurple discovers that the computer's network
	 * connection has gone away.
	 */
	void (*network_disconnected)(void);

	/**
	 * Called when an error causes a connection to be disconnected.
	 *  Called before #disconnected.  This op is intended to replace
	 *  #report_disconnect.  If both are implemented, this will be called
	 *  first; however, there's no real reason to implement both.
	 *
	 *  @param reason  why the connection ended, if known, or
	 *                 #PURPLE_CONNECTION_ERROR_OTHER_ERROR, if not.
	 *  @param text  a localized message describing the disconnection
	 *               in more detail to the user.
	 *  @see #purple_connection_error_reason
	 *
	 *  @since 2.3.0
	 */
	void (*report_disconnect_reason)(PurpleConnection *gc,
	                                 PurpleConnectionError reason,
	                                 const char *text);

	void (*_purple_reserved1)(void);
	void (*_purple_reserved2)(void);
	void (*_purple_reserved3)(void);
} PurpleConnectionUiOps;
-}
