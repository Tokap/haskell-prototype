{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics
-- http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html


data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id



------ SETTING UP A BASIC SCOTTY SERVER:

-- This commented main is equivalent to that which is below it
-- main :: IO ()
-- main = do
--   putStrLn "Starting Server..."
--   scotty 3000 $ do
--     get "/hello" $ do
--         text "hello world!"


routes :: ScottyM ()
routes = do
  get "/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
  get "/users" $ do
      Web.Scotty.json allUsers
  get "/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers) -- use of filter to find bob

main = do
  putStrLn "Starting Server..."
  scotty 3000 routes

-- Routes could also be written like:
-- routes :: ScottyM ()
-- routes = do
--   get "/hello" hello
--
-- hello :: ActionM ()
-- hello = do
--   text "hello world!"



-- data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
-- instance ToJSON User
-- instance FromJSON User
--
-- bob :: User
-- bob = User { userId = 1, userName = "bob" }
--
-- jenny :: User
-- jenny = User { userId = 2, userName = "jenny" }

-- allUsers :: [User]
-- allUsers = [bob, jenny]







startServer :: IO ()
startServer = do
  putStrLn "Starting Server..."

  scotty 3000 $ do
    get "/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")

    get "/posts" $ do
      json allUsers

    get "/comments" $ do
      json allUsers

    get "/users/:id" $ do -- EXAMPLE OF HOW TO FILTER:
      id <- param "id"
      json (filter (matchesId id) allUsers)

    -- assignment: post user and print it out
    post "/users" $ do
      user <- jsonData :: ActionM User
      json user





------ SUPER BASICS OF HASKELL:

-- greet :: String -> String
-- greet name = "Hello " ++ name ++ "!"
--
-- printNumbers = do
--   putStrLn (show (3+4))
--
-- printConfig = do
--   contents <- readFile "stack.yaml"
--   putStrLn contents
--
-- printTime = do
--   time <- getCurrentTime
--   putStrLn (show time)
--
-- numbers :: [Int]
-- numbers = [1,2,3,4]
--
-- -- Below will print 3x:
-- -- main = do
-- --   let action = putStrLn "Hello World"
-- --   action
-- --   action
-- --   action
-- --   return ()
--
-- -- prints the 'hello' statement & returns IO String of the name
-- sayHello :: IO String
-- sayHello = do
--   name <- getLine
--   putStrLn ("Hello " ++ name)
--   return name
--
-- beCareful :: Maybe Int
-- beCareful = do
--   Just 6
--   Nothing
--   return 5




-- let testPost = makePost "Generic Title" "Fleshed Out Post Body" 11 22
-- let testComment = makeComment "Bill" "bill@bill.com" "A lot of words go here" 1
-- -- statusQuery connDetails
-- -- z <- getPostById connDetails 1
-- -- z <- insertPost' connDetails testPost
-- allPosts <- getAllPosts connDetails
-- -- let allPosts = getAllPosts connDetails
-- print $ allPosts
