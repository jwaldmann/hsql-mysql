module DB.HSQL.MySQL.Functions where

import Foreign((.&.),peekByteOff,nullPtr,peekElemOff)
import Foreign.C(CInt(..),CString,peekCString)
import Control.Concurrent.MVar(MVar,newMVar,modifyMVar,readMVar)
import Control.Exception (throw)
import Control.Monad(when)

import Database.HSQL.Types(ColDef,Statement(..),Connection(..),SqlError(..))
import DB.HSQL.MySQL.Type(MYSQL,MYSQL_RES,MYSQL_FIELD,MYSQL_ROW,MYSQL_LENGTHS
                         ,mkSqlType)

#include <HsMySQL.h>

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

-- |
foreign import #{CALLCONV} "HsMySQL.h mysql_init"
  mysql_init :: MYSQL -> IO MYSQL

foreign import #{CALLCONV} "HsMySQL.h mysql_real_connect"
  mysql_real_connect :: MYSQL -> CString -> CString -> CString -> CString -> CInt -> CString -> CInt -> IO MYSQL

foreign import #{CALLCONV} "HsMySQL.h mysql_close"
  mysql_close :: MYSQL -> IO ()

foreign import #{CALLCONV} "HsMySQL.h mysql_errno"
  mysql_errno :: MYSQL -> IO CInt

foreign import #{CALLCONV} "HsMySQL.h mysql_error"
  mysql_error :: MYSQL -> IO CString

foreign import #{CALLCONV} "HsMySQL.h mysql_query"
  mysql_query :: MYSQL -> CString -> IO CInt

foreign import #{CALLCONV} "HsMySQL.h mysql_use_result"
  mysql_use_result :: MYSQL -> IO MYSQL_RES

foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_field"
  mysql_fetch_field :: MYSQL_RES -> IO MYSQL_FIELD

foreign import #{CALLCONV} "HsMySQL.h mysql_free_result"
  mysql_free_result :: MYSQL_RES -> IO ()

foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_row"
  mysql_fetch_row :: MYSQL_RES -> IO MYSQL_ROW

foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_lengths"
  mysql_fetch_lengths :: MYSQL_RES -> IO MYSQL_LENGTHS

foreign import #{CALLCONV} "HsMySQL.h mysql_list_tables"
  mysql_list_tables :: MYSQL -> CString -> IO MYSQL_RES

foreign import #{CALLCONV} "HsMySQL.h mysql_list_fields"
  mysql_list_fields :: MYSQL -> CString -> CString -> IO MYSQL_RES

foreign import #{CALLCONV} "HsMySQL.h mysql_next_result"
  mysql_next_result :: MYSQL -> IO CInt

-- |
withStatement :: Connection -> MYSQL -> MYSQL_RES -> IO Statement
withStatement conn pMYSQL pRes = do
  currRow  <- newMVar (nullPtr, nullPtr)
  refFalse <- newMVar False
  if (pRes == nullPtr)
    then do
      errno <- mysql_errno pMYSQL
      when (errno /= 0) (handleSqlError pMYSQL)
      return Statement { stmtConn   = conn
		       , stmtClose  = return ()
		       , stmtFetch  = fetch pRes currRow
		       , stmtGetCol = getColValue currRow
		       , stmtFields = []
		       , stmtClosed = refFalse }
    else do
      fieldDefs <- getFieldDefs pRes
      return Statement { stmtConn   = conn
		       , stmtClose  = mysql_free_result pRes
		       , stmtFetch  = fetch pRes currRow
		       , stmtGetCol = getColValue currRow
		       , stmtFields = fieldDefs
		       , stmtClosed = refFalse }

-- |
getColValue :: MVar (MYSQL_ROW, MYSQL_LENGTHS) 
            -> Int 
            -> ColDef 
            -> (ColDef -> CString -> Int -> IO a) 
            -> IO a
getColValue currRow colNumber fieldDef f = do
  (row, lengths) <- readMVar currRow
  pValue <- peekElemOff row colNumber
  len <- fmap fromIntegral (peekElemOff lengths colNumber)
  f fieldDef pValue len

-- |
getFieldDefs pRes = do
  pField <- mysql_fetch_field pRes
  if pField == nullPtr
    then return []
    else do
      name <- (#peek MYSQL_FIELD, name) pField >>= peekCString
      dataType <-  (#peek MYSQL_FIELD, type) pField
      columnSize <-  (#peek MYSQL_FIELD, length) pField
      flags <-  (#peek MYSQL_FIELD, flags) pField
      decimalDigits <-  (#peek MYSQL_FIELD, decimals) pField
      let sqlType = mkSqlType dataType columnSize decimalDigits
      defs <- getFieldDefs pRes
      return ( (name,sqlType,((flags :: Int) .&. (#const NOT_NULL_FLAG)) == 0)
             : defs )

-- |
fetch :: MYSQL_RES 
      -> MVar (MYSQL_ROW, MYSQL_LENGTHS) 
      -> IO Bool
fetch pRes currRow
    | pRes == nullPtr = return False
    | otherwise = modifyMVar currRow $ \(pRow, pLengths) -> do
	pRow <- mysql_fetch_row pRes
	pLengths <- mysql_fetch_lengths pRes
	return ((pRow, pLengths), pRow /= nullPtr)

-- |
mysqlDefaultConnectFlags:: CInt
mysqlDefaultConnectFlags = #const MYSQL_DEFAULT_CONNECT_FLAGS

------------------------------------------------------------------------------
-- routines for handling exceptions
------------------------------------------------------------------------------
-- |
handleSqlError :: MYSQL -> IO a
handleSqlError pMYSQL = do
	errno <- mysql_errno pMYSQL
	errMsg <- mysql_error pMYSQL >>= peekCString
	throw (SqlError "" (fromIntegral errno) errMsg)

