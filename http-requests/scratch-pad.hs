printFirstPost :: IO ()
printFirstPost = do
    request <- parseRequest "GET https://jsonplaceholder.typicode.com/posts"
    response <- httpJSON request

    let postOutput = getResponseBody response :: [Post]
    print $ postOutput !! 0

printFirstComment :: IO ()
printFirstComment = do
    request <- parseRequest "GET https://jsonplaceholder.typicode.com/comments"
    response <- httpJSON request

    let commentOutput = getResponseBody response :: [Comment]
    print $ commentOutput !! 0
