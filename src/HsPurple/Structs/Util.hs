{-# LANGUAGE ForeignFunctionInterface #-}

module HsPurple.Structs.Util
    (
    -- * GLib utils
      gListToList
    , listToGList
    ) where

import Foreign

import Bindings.GLib

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

listToGList :: Ptr C'GList  -- ^ Pointer to store data in
            -> [Ptr ()]
            -> IO (Ptr C'GList)
listToGList ptr [] = return ptr -- do nothing
listToGList ptr (x:xs) = do
    ptr' <- c'g_list_append ptr x
    listToGList ptr' xs
