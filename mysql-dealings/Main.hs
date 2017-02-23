-- {-# LANGUAGE OverloadedStrings #-}
--
-- import Database.MySQL.Simple
--
-- main :: IO Int
-- main = do
-- let connDetails = ConnectInfo {
--                     connectHost     = "localhost",
--                     connectPort     = 3306,
--                     connectUser     = "root",
--                     connectDatabase = "ip_brolytics"
--                   }
--   conn <- connect connDetails
--   [Only i] <- query_ conn "select * from status_type"
--   return i
---------------------------------------------


---------------------------------------------
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import Database.MySQL.Simple
--
-- main :: IO Int
-- main = do
--   conn <- connect defaultConnectInfo
--   [Only i] <- query_ conn "select status from status_type"
--   return i
---------------------------------------------

---------------------------------------------
-- Simple Query using one param & expected return:
---------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Database.MySQL.Simple
import qualified Data.Text as Text


myQuery :: Query
myQuery = "SELECT status FROM status_type"

main :: IO ()
main = do
  let connDetails = defaultConnectInfo {
                      connectHost     = "127.0.0.1",
                      connectPort     = 3306,
                      connectUser     = "root",
                      connectPassword = "",
                      connectDatabase = "ip_brolytics"
                    }
  conn <- connect connDetails
  xs <- query_ conn myQuery
  forM_ xs $ \(Only status) ->
    putStrLn $ Text.unpack "Status is: " ++ status

---------------------------------------------
-- Alternate Library:

-- main = do
-- rows <- withRTSSignalsBlocked $ do
--   conn <- connectMySQL defaultMySQLConnectInfo {
--             mysqlHost     = "db1.example.com",
--             mysqlUser     = "scott",
--             mysqlPassword = "tiger"
--           }
--   quickQuery' conn "SELECT 1 + 1" []
-- forM_ rows $ \row -> putStrLn $ show row
-------------------------------------------------------------------------------
-- local_host alias: 127.0.0.1


--- HDBC Library:
-- import Control.Monad
-- import Database.HDBC
-- import Database.HDBC.MySQL
-- main = do conn <- connectMySQL defaultMySQLConnectInfo {
--                        mysqlHost     = "127.0.0.1",
--                        mysqlUser     = "root",
--                        mysqlPassword = "",
--                        mysqlDatabase = "ip_brolytics"
--                     }
--
--           rows <- quickQuery' conn "SELECT * FROM status_type" []
--           forM_ rows $ \row -> putStrLn $ show row
