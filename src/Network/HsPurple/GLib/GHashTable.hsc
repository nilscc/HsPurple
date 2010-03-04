-- vim: ft=haskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HsPurple.GLib.GHashTable where

import Control.Applicative
import Foreign
import Foreign.C
import Network.HsPurple.GLib.GList

import qualified Data.Map as M

#include <glib.h>
#include <bindings.dsl.h>

type C'GHashTable = Ptr ()

#callback GHFunc                        , Ptr () -> Ptr () -> Ptr () -> IO ()
#callback GHRFunc                       , Ptr () -> Ptr () -> Ptr () -> IO CInt
#callback GHashFunc                     , Ptr () -> IO CInt
#callback GEqualFunc                    , Ptr () -> Ptr () -> IO CInt

#ccall g_hash_table_new                 , <GHashFunc> -> <GEqualFunc> -> IO (Ptr <GHashTable>)
#ccall g_hash_table_insert              , Ptr <GHashTable> -> Ptr () -> Ptr () -> IO ()
#ccall g_hash_table_replace             , Ptr <GHashTable> -> Ptr () -> Ptr () -> IO ()
#ccall g_hash_table_size                , Ptr <GHashTable> -> CInt
#ccall g_hash_table_lookup              , Ptr <GHashTable> -> Ptr () -> IO (Ptr ())
#ccall g_hash_table_lookup_extended     , Ptr <GHashTable> -> Ptr () -> IO (Ptr ())
#ccall g_hash_table_foreach             , Ptr <GHashTable> -> <GHFunc> -> Ptr () -> IO ()
#ccall g_hash_table_find                , Ptr <GHashTable> -> <GHRFunc> -> Ptr () -> IO (Ptr ())
#ccall g_hash_table_remove              , Ptr <GHashTable> -> Ptr () -> IO CInt
#ccall g_hash_table_steal               , Ptr <GHashTable> -> Ptr () -> IO CInt
#ccall g_hash_table_foreach_remove      , Ptr <GHashTable> -> <GHRFunc> -> Ptr () -> IO CInt
#ccall g_hash_table_foreach_steal       , Ptr <GHashTable> -> <GHRFunc> -> Ptr () -> IO CInt
#ccall g_hash_table_remove_all          , Ptr <GHashTable> -> IO ()
#ccall g_hash_table_steal_all           , Ptr <GHashTable> -> IO ()
#ccall g_hash_table_get_keys            , Ptr <GHashTable> -> IO (Ptr <GList>)
#ccall g_hash_table_get_values          , Ptr <GHashTable> -> IO (Ptr <GList>)
#ccall g_hash_table_destroy             , Ptr <GHashTable> -> IO ()
#ccall g_hash_table_ref                 , Ptr <GHashTable> -> IO (Ptr <GHashTable>)
#ccall g_hash_table_unref               , Ptr <GHashTable> -> IO ()

-- | Used for GHashTables with String keys
#ccall g_str_hash                       , CString -> IO CInt
#ccall g_str_equal                      , CString -> CString -> IO CInt

{-
type C'GHashTableIter = Ptr ()

#ccall g_hash_table_iter_init           , Ptr <GHashTableIter> -> Ptr <GHashTable> -> IO ()
-}

-- | Turn a GHashTable into a Map with String keys and values
gHasHTableToStringMap :: Ptr C'GHashTable -> IO (M.Map String String)
gHasHTableToStringMap ptr =
    (\a b -> M.fromList $ zip a b) <$> (c'g_hash_table_get_keys ptr   >>= makeStrings)
                                   <*> (c'g_hash_table_get_values ptr >>= makeStrings)

  where makeStrings glist = mapM (peekCString . castPtr) =<< gListToList =<< peek glist

-- | Turn a Map with String keys/values into a GHashTable
stringMapToGHashTable :: M.Map String String -> IO (Ptr C'GHashTable)
stringMapToGHashTable m = do

    let ghash p1        = c'g_str_hash  (castPtr p1)
        geq p1 p2       = c'g_str_equal (castPtr p1) (castPtr p2)

    gh <- mk'GHashFunc ghash
    ge <- mk'GEqualFunc geq
    ptr <- c'g_hash_table_new gh ge

    let insert (s1,s2)  = do
            cs1 <- newCString s1
            cs2 <- newCString s2
            c'g_hash_table_insert ptr (castPtr cs1) (castPtr cs2)

    mapM_ insert $ M.toList m
    return ptr
