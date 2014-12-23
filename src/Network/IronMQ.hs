{-# LANGUAGE OverloadedStrings #-}
module Network.IronMQ (module Network.IronMQ, Network.IronMQ.Types.Client(..)) where

import Network.Wreq
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Network.IronMQ.Types

getJSONWithOpts :: FromJSON a => Client -> String -> [(Text, Text)] -> IO a
getJSONWithOpts client s ps = do
    let baseurl = concat ["https://", server client, "/", apiVersion client, "/projects/", projectID client]
        url = baseurl ++ s
        getOpts = defaults & header "Content-Type" .~ ["application/json"] & params .~ ("oauth", token client) : ps
    response <- asJSON =<< getWith getOpts url
    return (response ^. responseBody)

getJSON ::FromJSON a => Client -> String -> IO a
getJSON client s = getJSONWithOpts client s []

getQueues :: Client -> IO [QueueSummary]
getQueues client = getJSON client "/queues"

getQueue :: Client -> String -> IO Queue
getQueue client queueName = getJSON client ("/queues/" ++ queueName)

getMessages :: Client -> String -> IO MessageList
getMessages client queueName = getJSON client ("/queues/" ++ queueName ++ "/messages")

getMessageById :: Client -> String -> String -> IO Message
getMessageById client queueName messageID = getJSON client ("/queues/" ++ queueName ++ "/messages/" ++ messageID)

getMessagePushStatuses :: Client -> String -> String -> IO PushStatus
getMessagePushStatuses client queue messageID = undefined

postMessages :: Client -> String -> MessageList -> IO IronResponse
postMessages client queue messageList = undefined

clear :: Client -> String -> IO IronResponse
clear client queue = undefined

deleteMessage :: Client -> String -> String -> IO IronResponse
deleteMessage client queue messageID = undefined

deleteMessages :: Client -> String -> [String] -> IO IronResponse
deleteMessages client queue meessageIDs = undefined

peek :: Client -> String -> IO IronResponse
peek client queue = undefined

touch :: Client -> String -> String -> IO IronResponse
touch client queue messageID = undefined


update client queue subscribers = undefined

deleteQueue client queue = undefined

addAlerts client queue alerts = undefined

updateAlerts client queue alerts = undefined

removeAlerts client queue alertID = undefined

removeAlert client queue alertID = undefined

addSubsrubers client queue subscribers = undefined

removeSubscribers client queue subscribers = undefined

deletMessagePushStatus queue messageID = undefined

queues = undefined