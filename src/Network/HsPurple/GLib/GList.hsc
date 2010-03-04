-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}
 
module Network.HsPurple.GLib.GList where

import Foreign

#include <bindings.dsl.h>
#include <glib.h>

#starttype GList
#field data , Ptr ()
#field next , Ptr <GList>
#field prev , Ptr <GList>
#stoptype

-- Very basic bindings

#ccall g_list_append , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)
#ccall g_list_prepend , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)

gListToList :: C'GList -> IO [Ptr ()]
gListToList glis = do

    let a = c'GList'data glis

    p <- peek (c'GList'prev glis) >>= previous
    n <- peek (c'GList'next glis) >>= next

    return $ p ++ [a] ++ n

  where previous, next :: C'GList -> IO [Ptr ()]
        previous glis' = case c'GList'data glis' of
                              a | a == nullPtr -> return []
                                | otherwise    -> do p <- peek (c'GList'prev glis) >>= previous
                                                     return $ p ++ [a]
        next glis'     = case c'GList'data glis' of
                              a | a == nullPtr -> return []
                                | otherwise    -> do n <- peek (c'GList'next glis) >>= next
                                                     return $ a : n

listToGList :: [Ptr ()] -> IO (Ptr C'GList)
listToGList lis = listToGList' (castPtr nullPtr) lis
listToGList' :: Ptr C'GList -> [Ptr ()] -> IO (Ptr C'GList)
listToGList' ptr []     = return ptr
listToGList' ptr (x:xs) = do
    ptr' <- c'g_list_append ptr x
    listToGList' ptr' xs
