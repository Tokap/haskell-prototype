-- https://haskell-lang.org/library/http-client

--------------------------------------------------------------------------------
-- Basic Version:
--------------------------------------------------------------------------------

#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response


--------------------------------------------------------------------------------
-- JSON RETURN VERSION:
-- "https://jsonplaceholder.typicode.com/posts";
--------------------------------------------------------------------------------
#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-5.13 runghc
    --package http-conduit
    --package yaml
-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "https://jsonplaceholder.typicode.com/posts/1"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)


--------------------------------------------------------------------------------
-- More Complex Request Building
--------------------------------------------------------------------------------
#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-5.13 runghc
    --package http-conduit
    --package yaml
-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    request' <- parseRequest "POST http://httpbin.org/post"
    let request
            = setRequestMethod "PUT"
            $ setRequestPath "/put"
            $ setRequestQueryString [("hello", Just "world")]
            $ setRequestBodyLBS "This is my request body"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)


--------------------------------------------------------------------------------
-- Request without parsing
--------------------------------------------------------------------------------
#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-5.13 runghc
    --package http-conduit
    --package yaml
-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    let request
            = setRequestPath "/get"
            $ setRequestHost "httpbin.org"
            $ defaultRequest
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

--------------------------------------------------------------------------------
-- Managing Client: this one can make https requests through tlsManagerSettings
--------------------------------------------------------------------------------
#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-5.13 runghc
    --package http-conduit
    --package yaml
-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple
import           Network.HTTP.Client.TLS -- this import allows us to do https

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let request = setRequestManager manager "https://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response


--------------------------------------------------------------------------------
-- Receiving JSON
--------------------------------------------------------------------------------
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit --package aeson
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "https://jsonplaceholder.typicode.com/posts/1"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- bodyReaderSource (responseBody response)
              $$ sinkParser json
        print value

-- Returns:
-- Object (fromList [("body",String "quia et suscipit\nsuscipit recusandae
-- consequuntur expedita et cum\nreprehenderit molestiae ut ut quas
-- totam\nnostrum rerum est autem sunt rem eveniet architecto"),
-- ("userId",Number 1.0),("id",Number 1.0),("title",String "sunt aut facere
-- repellat provident occaecati excepturi optio reprehenderit")])

--------------------------------------------------------------------------------
-- Sending JSON
--------------------------------------------------------------------------------
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.13 runghc --package http-client-tls --package aeson
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    -- Create the request
    let requestObject = object
            [ "name" .= ("Alice" :: String)
            , "age"  .= (35 :: Int)
            ]
    initialRequest <- parseRequest "http://httpbin.org/post"
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8")
                ]
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
