# haskell-prototype

A simple prototype for a Haskell Web App. MVP Features & Stretch Goals Include:

- able to handle https connections and manipulate results
- able to store and access data through MySQL & handle errors in process
- able to initiate the above processes through an ‘on demand’ endpoint
- able to initiate the above processes through a 0MQ-style messaging ecosystem (Stretch)
- testing for all core functionality (Stretch)

Current Library Choices:

`mysql-simple` -> MySQL Connections
https://hackage.haskell.org/package/mysql-simple-0.4.0.0/docs/Database-MySQL-Simple.html

`http-conduit` -> HTTPS Connections
https://hackage.haskell.org/package/http-conduit-2.2.3

`happstack` -> Server
https://hackage.haskell.org/package/happstack
