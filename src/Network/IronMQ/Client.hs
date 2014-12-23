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

-- type Resp = Response (Map String Value)

token = "_B6KOfA16D4AmW2NwSCw12mgVxk"
projectID = "53f691bd45d4960005000082"
server = "mq-aws-us-east-1.iron.io"
apiVersion = "1"

baseurl = concat ["https://", server, "/", apiVersion, "/projects/", projectID]
opts = defaults & header "Content-Type" .~ ["application/json"]

data QueueSummary = QueueSummary {
        q_id :: String,
        q_project_id :: String,
        q_name :: String
} deriving (Show)

data Queue = Queue {
        id :: String,
        project_id :: String,
        name :: String,
        size :: Maybe Int,
        total_messages :: Maybe Int
} deriving (Show, Generic)

getJSON ::FromJSON a => String -> IO a
getJSON s = do
    let url = baseurl ++ s
        getOpts = opts & param "oauth" .~ [token]
    response <- asJSON =<< getWith getOpts url
    return (response ^. responseBody)

instance FromJSON Queue

getQueues :: IO [Queue]
getQueues = getJSON "/queues"

getQueue :: String -> IO Queue
getQueue queueName = getJSON ("/queues/" ++ queueName)

clear queue = undefined

deleteMessage queue messageID = undefined

deleteMessages queue meessageIDs = undefined

postMessages queue messages = undefined

getMessage queue = undefined

getMessageById queue messageID = undefined

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

getMessagePushStatuses queue messageID = undefined

deletMessagePushStatus queue messageID = undefined

queues = undefined


main = getQueue "default"

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