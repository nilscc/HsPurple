-- vim: ft=haskell
{-# LANGUAGE ForeignFunctionInterface #-}
 
module Network.HsPurple.GLib.GList where

import Foreign

#include <bindings.dsl.h>
#include <glib.h>

-- Very basic bindings

#ccall g_list_append  , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)
#ccall g_list_prepend , Ptr <GList> -> Ptr () -> IO (Ptr <GList>)

#starttype GList
#field data , Ptr ()
#field next , Ptr <GList>
#field prev , Ptr <GList>
#stoptype

gListToList :: Ptr C'GList -> IO [Ptr ()]
gListToList ptr | ptr == nullPtr = return []
                | otherwise = do

    glis <- peek ptr
    let a = c'GList'data glis
    p <- toList c'GList'prev (c'GList'prev glis)
    n <- toList c'GList'next (c'GList'next glis)
    return $ reverse p ++ [a] ++ n

  where toList f ptr' | ptr' == nullPtr = return []
                      | otherwise = do glis <- peek ptr'
                                       let a = c'GList'data glis
                                       o <- toList f $ f glis
                                       return $ a : o


listToGList :: [Ptr ()] -> IO (Ptr C'GList)
listToGList lis = listToGList' (castPtr nullPtr) lis

listToGList' :: Ptr C'GList -> [Ptr ()] -> IO (Ptr C'GList)
listToGList' ptr []     = return ptr
listToGList' ptr (x:xs) = do
    ptr' <- c'g_list_append ptr x
    listToGList' ptr' xs
