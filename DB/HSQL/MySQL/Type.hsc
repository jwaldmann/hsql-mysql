module DB.HSQL.MySQL.Type where

import Foreign(Ptr)
import Foreign.C(CString,CULong)

import Database.HSQL.Types

#include <HsMySQL.h>

-- |
type MYSQL = Ptr ()

type MYSQL_RES = Ptr ()

type MYSQL_FIELD = Ptr ()

type MYSQL_ROW = Ptr CString

type MYSQL_LENGTHS = Ptr CULong

-- |
mkSqlType :: Int -> Int -> Int -> SqlType
mkSqlType (#const FIELD_TYPE_STRING)     size _	   = SqlChar size
mkSqlType (#const FIELD_TYPE_VAR_STRING) size _    = SqlVarChar size
mkSqlType (#const FIELD_TYPE_DECIMAL)    size prec = SqlNumeric size prec
mkSqlType (#const FIELD_TYPE_SHORT)      _    _    = SqlSmallInt
mkSqlType (#const FIELD_TYPE_INT24)      _    _    = SqlMedInt
mkSqlType (#const FIELD_TYPE_LONG)       _    _    = SqlInteger
mkSqlType (#const FIELD_TYPE_FLOAT)      _    _	   = SqlReal
mkSqlType (#const FIELD_TYPE_DOUBLE)     _    _    = SqlDouble
mkSqlType (#const FIELD_TYPE_TINY)       _    _    = SqlTinyInt
mkSqlType (#const FIELD_TYPE_LONGLONG)   _    _    = SqlBigInt
mkSqlType (#const FIELD_TYPE_DATE)       _    _    = SqlDate
mkSqlType (#const FIELD_TYPE_TIME)       _    _    = SqlTime
mkSqlType (#const FIELD_TYPE_TIMESTAMP)  _    _    = SqlTimeStamp
mkSqlType (#const FIELD_TYPE_DATETIME)   _    _    = SqlDateTime
mkSqlType (#const FIELD_TYPE_YEAR)       _    _    = SqlYear
mkSqlType (#const FIELD_TYPE_BLOB)       _    _    = SqlBLOB
mkSqlType (#const FIELD_TYPE_SET)        _    _    = SqlSET
mkSqlType (#const FIELD_TYPE_ENUM)       _    _    = SqlENUM
mkSqlType tp                             _    _    = SqlUnknown tp
