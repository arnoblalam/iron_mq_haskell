{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.IronMQ.Client where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T

-- type Resp = Response (Map String Value)

token = "_B6KOfA16D4AmW2NwSCw12mgVxk"
projectID = "53f691bd45d4960005000082"
server = "mq-aws-us-east-1.iron.io"
apiVersion = "1"

baseurl = concat ["https://", server, "/", apiVersion, "/projects/", projectID]
opts = defaults & header "Content-Type" .~ ["application/json"]

data Queue = Queue {
        id :: String,
        project_id :: String,
        name :: String,
        size :: Maybe Int,
        total_messages :: Maybe Int
} deriving (Show, Generic)

data PushStatus = PushStatus {
        retries_remaining :: Int
} deriving (Show, Generic)

data Message = Message {
        m_id :: String,
        body :: T.Text,
        timeout :: Int,
        reserved_count :: Int,
        push_status :: Maybe PushStatus
} deriving (Show, Generic)

data MessageList = MessageList {
        messages :: [Message]
} deriving (Show, Generic)

instance FromJSON Queue
instance FromJSON PushStatus
instance FromJSON Message where
        parseJSON (Object v) = Message <$>
                v .: "id" <*>
                v .: "body" <*>
                v .: "timeout" <*>
                v .: "reserved_count" <*>
                v .:? "push_status"
        parseJSON _ = mzero

instance FromJSON MessageList

getJSON ::FromJSON a => String -> IO a
getJSON s = do
    let url = baseurl ++ s
        getOpts = opts & param "oauth" .~ [token]
    response <- asJSON =<< getWith getOpts url
    return (response ^. responseBody)

getQueues :: IO [Queue]
getQueues = getJSON "/queues"

getQueue :: String -> IO Queue
getQueue queueName = getJSON ("/queues/" ++ queueName)

getMessages :: String -> IO MessageList
getMessages queueName = getJSON ("/queues/" ++ queueName ++ "/messages")

getMessageById queue messageID = undefined

getMessagePushStatuses queue messageID = undefined

postMessages queue messages = undefined

clear queue = undefined

deleteMessage queue messageID = undefined

deleteMessages queue meessageIDs = undefined

peek queue = undefined

touch queue messageID = undefined

update queue subscribers = undefined

deleteQueue queue = undefined

addAlerts queue alerts = undefined

updateAlerts queue alerts = undefined

removeAlerts queue alertID = undefined

removeAlert queue alertID = undefined

addSubsrubers queue subscribers = undefined

removeSubscribers queue subscribers = undefined

deletMessagePushStatus queue messageID = undefined

queues = undefined


main = getMessages "default"

-- makeRequest method endpoint body =
--     let url = concat ["https://", server, "/", apiVersion, "/projects/", projectID, endpoint]
--         opts = defaults & header "Content-Type" .~ ["application/json"] in
--     case method of
--         "get" -> do
--             let getOpts = opts & param "oauth" .~ [token] & makeBody "get" body
--             getWith getOpts url
--         "post" -> do
--             let postOpts = opts & header "OAuth" .~ [encodeUtf8 token]
--             postWith postOpts url makeBody "post" body

-- makeBody method body = undefiend

-- main = makeRequest "post" "/queues" "something"