{-| Module      :  Database.HSQL.MySQL
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to MySQL database
-}
module Database.HSQL.MySQL(connect,module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types(Connection(..),Statement(stmtGetCol),ColDef
                          ,SqlType(SqlVarChar),fromSqlCStringLen)
import Foreign(nullPtr,free)
import Foreign.C(newCString,withCString)
import Control.Monad(when)
import Control.Concurrent.MVar(newMVar)

import DB.HSQL.MySQL.Type(MYSQL,MYSQL_RES)
import DB.HSQL.MySQL.Functions(handleSqlError,withStatement,mysql_query
                              ,mysql_close,mysql_use_result,mysql_next_result
                              ,mysql_list_fields,mysql_list_tables
                              ,mysql_init,mysql_real_connect
                              ,mysqlDefaultConnectFlags)

------------------------------------------------------------------------------
-- Connect/Disconnect
------------------------------------------------------------------------------
-- | Makes a new connection to the database server.
connect :: String   -- ^ Server name
        -> String   -- ^ Database name
        -> String   -- ^ User identifier
        -> String   -- ^ Authentication string (password)
        -> IO Connection
connect server database user authentication = do
	pMYSQL <- mysql_init nullPtr
	pServer <- newCString server
	pDatabase <- newCString database
	pUser <- newCString user
	pAuthentication <- newCString authentication
	res <- mysql_real_connect pMYSQL pServer pUser pAuthentication pDatabase 0 nullPtr mysqlDefaultConnectFlags
	free pServer
	free pDatabase
	free pUser
	free pAuthentication
	when (res == nullPtr) (handleSqlError pMYSQL)
	refFalse <- newMVar False
	let connection = Connection
		{ connDisconnect = mysql_close pMYSQL
		, connExecute = mysqlExecute pMYSQL
		, connQuery = mysqlQuery connection pMYSQL
		, connTables = mysqlTables connection pMYSQL
		, connDescribe = mysqlDescribe connection pMYSQL
		, connBeginTransaction = mysqlExecute pMYSQL "begin"
		, connCommitTransaction = mysqlExecute pMYSQL "commit"
		, connRollbackTransaction = mysqlExecute pMYSQL "rollback"
		, connClosed = refFalse	}
	return connection

-- |
mysqlQuery :: Connection -> MYSQL -> String -> IO Statement
mysqlQuery conn pMYSQL query = do
			res <- withCString query (mysql_query pMYSQL)
			when (res /= 0) (handleSqlError pMYSQL)
			pRes <- getFirstResult pMYSQL
			withStatement conn pMYSQL pRes
			where
			  getFirstResult :: MYSQL -> IO MYSQL_RES
			  getFirstResult pMYSQL = do
			    pRes <- mysql_use_result pMYSQL
			    if pRes == nullPtr
			      then do
			        res <- mysql_next_result pMYSQL
			        if res == 0
			          then getFirstResult pMYSQL
			          else return nullPtr
			      else return pRes


-- |
mysqlDescribe :: Connection -> MYSQL -> String -> IO [ColDef]
mysqlDescribe conn pMYSQL table = do
  pRes <- withCString table (\table -> mysql_list_fields pMYSQL table nullPtr)
  stmt <- withStatement conn pMYSQL pRes
  return (getFieldsTypes stmt)


-- |
mysqlTables :: Connection -> MYSQL -> IO [String]
mysqlTables conn pMYSQL = do
  pRes <- mysql_list_tables pMYSQL nullPtr
  stmt <- withStatement conn pMYSQL pRes
  -- SQLTables returns:
  -- Column name     #   Type
  -- Tables_in_xx      0   VARCHAR
  collectRows (\stmt -> do
    mb_v <- stmtGetCol stmt 0 ("Tables", SqlVarChar 0, False) fromSqlCStringLen
    return (case mb_v of { Nothing -> ""; Just a -> a })) stmt

-- |
mysqlExecute :: MYSQL -> String -> IO ()
mysqlExecute pMYSQL query = do
  res <- withCString query (mysql_query pMYSQL)
  when (res /= 0) (handleSqlError pMYSQL)
