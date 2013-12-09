# CLEVE Data Dump

## Data Dump Version

This version requires the Incursion 1.01 data dump for MySQL:

- http://zofu.no-ip.de/inc101/inc101-mysql5-sql-v1/
- http://zofu.no-ip.de/inc101/inc101-mysql5-v1.sql.bz2

## CLSQL-SQLITE3 Issues

If CLSQL complains it cannot find libsqlite3 then you need to
add a link to wherever it is installed on your system in
CLSQL:*FOREIGN-LIBRARY-PATHS*.  Name it libsqlite3.so.

### SQLite 3 on Windows

Put `sqlite3.dll` in the CLEVE root directory.

## Notes to Self

Interesting tables:

- dgmAttributeCategories
- dgmAttributeTypes
- dgmEffects
- dgmTypeAttributes
- ?: dgmTypeEffects
- eveUnits
- invCategories
- invFlags
- invGroups
- ?: invMarketGroups
- invMetaGroups
- ?: invMetaTypes
- invTypes
