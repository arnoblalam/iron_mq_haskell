-- | Main entry point to the application.
module IronMQ where

import Data.Time
import qualified Network.HTTP.Conduit

type HostName = String
type PortID = Int
type Token = String
type ProjectID = String
type Endpoint = String
type Body = String

awsHost :: String
awsHost = "mq-aws-us-east-1.iron.io"

-- Network protocol
data Scheme = HTTP deriving Show

-- HTTP Method
data Method = Get | Post | Put | Delete deriving Show

-- IronIO client
data Client = Client {
    token :: Token,
    projectID :: ProjectID,
    apiVersion:: Int,
    scheme :: Scheme,
    host :: HostName,
    port :: PortID,
    maxRetries :: Int
} deriving Show

-- Create a default Client from a Token and a Project ID
createClient :: Token -> ProjectID -> Client
createClient t p = Client {
    token = t,
    projectID = p,
    apiVersion = 1,
    scheme = HTTP,
    host = awsHost,
    port = 443,
    maxRetries = 5
}

-- Message for IronIO
data Message = Message {
    body :: String,
    timeout :: DiffTime,
    delay :: DiffTime,
    expiresIn :: DiffTime
} deriving Show

-- Create a default message from a body
createMessage :: String -> Message
createMessage b = Message {
    body = b,
    timeout = 60,
    delay = 0,
    expiresIn = 604800
}

type Status = String

-- Make a request
request :: Client -> Method -> Endpoint -> Body -> Network.HTTP.Conduit.Request
request = (Network.HTTP.Conduit.parseUrl url) >> Network.HTTP.Conduit.httpLbs where
    url = "http://www.google.com"

getMessage :: IO Message
getMessage = undefined

getMessages :: IO [Message]
getMessages = undefined

postMessage :: Message -> IO Status
postMessage = undefined

postMessages :: [Message] -> IO Status
postMessages = undefined

deleteMessage :: Message -> IO Status
deleteMessage = undefined