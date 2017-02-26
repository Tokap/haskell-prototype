stockQuery :: ConnectionDetails -> IO ()
stockQuery connDetails = do
  conn <- makeConnection connDetails
  xs <- query_ conn myQuery
  forM_ xs $ \(Only status) ->
    putStrLn $ Text.unpack "Status is: " ++ status


statusQuery :: ConnectionDetails -> IO [Status]
statusQuery connDetails = do
  conn <- makeConnection connDetails
  xs <- query_ conn myQuery

  return $ map (\(Only status) -> Status status) xs

----- DATA TYPES:

data Address = Address {
  street  :: String,
  city    :: String,
  state   :: String,
  zipcode :: String
} deriving (Show, Generic)
instance ToJSON Address
instance FromJSON Address


data User = User {
  id       :: Int,
  name     :: String,
  username :: String,
  email    :: String,
  address  :: Address,
  phone    :: String
} deriving (Show, Generic)
instance ToJSON User
instance FromJSON User


data CommenterInfo = CommenterInfo {
  commenterNames  :: [String],
  commenterEmails :: [String]
}


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Incompatible with nested structure?
--
-- data Geo = Geo {
--   lat :: String,
--   lng :: String
-- } deriving (Show, Generic)
-- instance ToJSON Geo
-- instance FromJSON Geo
--
--
-- data Address = Address {
--   street  :: String,
--   suite   :: String,
--   city    :: String,
--   zipcode :: String,
--   geo     :: Geo
-- } deriving (Show, Generic)
-- instance ToJSON Address
-- instance FromJSON Address
--
--
-- data Company = Company {
--   name        :: String,
--   catchPhrase :: String,
--   bs          :: String
-- } deriving (Show, Generic)
-- instance ToJSON Company
-- instance FromJSON Company
--
-- data User = User {
--   id       :: Int,
--   name     :: String,
--   username :: String,
--   email    :: String,
--   address  :: Address,
--   phone    :: String,
--   website  :: String,
--   company  :: Company
-- } deriving (Show, Generic)
-- instance ToJSON User
-- instance FromJSON User


-- deets <- stockQuery' connDetails
-- let deets = stockQuery' connDetails // This is bad: would make deets IO String
-- putStrLn deets


-- import qualified Data.Text as Text

-- The below works for printing full post Query:
-- forM_ xs $ \(t, b, i, i2) ->
--   putStrLn $ Text.unpack "Variables are: " ++ t ++ " " ++ b ++ " Some Numbers: " ++ show (i :: Int) ++ " " ++ show (i2 :: Int)
