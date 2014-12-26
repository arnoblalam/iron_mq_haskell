{-# LANGUAGE OverloadedStrings #-}
module Network.IronMQ (module Network.IronMQ, Network.IronMQ.Types.Client(..)) where

import Network.Wreq
import Network.Wreq.Types (Postable)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text, append, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.IronMQ.Types
import Network.HTTP.Client (RequestBody(..))


-- | Some convenient type synonyms to help keel track of things

type Url = Text
type Endpoint = Text
type Param = (Text, Text)
type QueueName = Text
type ID = Text -- could be a message ID, subscriber ID or whatever

-- | Some convenience functions to make HTTP requests easier

-- | Construct a base URL for HTTP requests from a client
baseurl :: Client -> Text
baseurl client = "https://" `append` server client `append` "/" `append` apiVersion client
                            `append` "/projects/" `append` projectID client
-- | An empty body for POST/PUT requests
emptyBody :: Payload
emptyBody = Raw "application/json" $ RequestBodyLBS ""

-- | Make a GET request to an endpoint using connection info from client and
-- query string set to parameters. Return the JSON results
getJSONWithOpts :: FromJSON a => Client -> Endpoint -> [Param] -> IO a
getJSONWithOpts client endpoint parameters = do
    let url = baseurl client `append` endpoint
        getOpts = defaults & header "Content-Type" .~ ["application/json"]
                           & params .~ ("oauth", token client) : parameters
    response <- asJSON =<< getWith getOpts (unpack url)
    return (response ^. responseBody)

-- | Make a GET request to an endpoint using the connection info from client.
-- Return the JSON results.
getJSON ::FromJSON a => Client -> Endpoint -> IO a
getJSON client s = getJSONWithOpts client s []

-- | Make a POST a request to an endpoint using connection info from client
-- and the body provided. Return the JSON response.
postJSONWithBody :: (Postable a, FromJSON b) => Client -> Endpoint -> a -> IO b
postJSONWithBody client endpoint body = do
    let url = baseurl client `append` endpoint
        postOpts = defaults & header "Authorization" .~
            [encodeUtf8 ("OAuth " `append` token client)]
    response <- asJSON =<< postWith postOpts (unpack url) body
    return (response ^. responseBody)

-- | Make a POST request to an endpoint using the connection into from client
-- and an empty body. Returb the JSON response.
postJSON :: (ToJSON b, FromJSON b) => Client -> Endpoint -> IO b
postJSON client endpoint = postJSONWithBody client endpoint emptyBody

-- | Get a list of queues available to the client
queues :: Client -> IO [QueueSummary]
queues client = getJSON client "/queues"

-- | Get a queue from the client
getQueue :: Client -> QueueName -> IO Queue
getQueue client queueName = getJSON client ("/queues/" `append` queueName)

-- | Get a list of messages on a queue
getMessages :: Client -> QueueName -> IO MessageList
getMessages client queueName = getJSON client
    ("/queues/" `append` queueName `append` "/messages")

getMessageById :: Client -> QueueName -> ID -> IO Message
getMessageById client queueName messageID = getJSON client
    ("/queues/" `append` queueName `append` "/messages/" `append` messageID)

-- | Get the push status of a message
getMessagePushStatus :: Client -> QueueName -> ID -> IO PushStatus
getMessagePushStatus client queueName messageID = undefined

-- | Post messages to a queue
postMessages :: Client -> QueueName -> [Message] -> IO IronResponse
postMessages client queueName messageList = undefined

-- | Clear all messages from a queue
clear :: Client -> QueueName -> IO IronResponse
clear client queueName = postJSON client ("/queues/" `append` queueName  `append` "/clear")

{-
-- | Delete a queue
deleteQueue :: Client -> QueueName -> IO IronResponse
deleteQueue client queueName = undefined

-- | Delete a message from a queue
deleteMessage :: Client -> QueueName -> ID -> IO IronResponse
deleteMessage client queueName messageID = undefined

-- | Delete several messages from a queue
deleteMessages :: Client -> QueueName -> [ID] -> IO IronResponse
deleteMessages client queueName meessageIDs = undefined

-- | Delete the message push status of a message
deleteMessagePushStatus :: Client -> QueueName -> ID -> IO IronResponse
deleteMessagePushStatus client queueName messageID = undefined

-- | Remove alerts from a queue
deleteAlerts :: Client -> QueueName -> [ID] -> IO IronResponse
deleteAlerts client queueName alertIDs = undefined

-- | Remove an alert from a queue
deleteAlert :: Client -> QueueName -> ID -> IO IronResponse
deleteAlert client queueName alertID = undefined

-- Remove subscribers from a queue
deleteSubscribers client queueName subscribers = undefined

-- | Take a look at the next item on the queue
peek :: Client -> QueueName -> IO IronResponse
peek client queueName = undefined

-- | Touch a message on the queue
touch :: Client -> QueueName -> ID -> IO IronResponse
touch client queueName messageID = undefined

-- | Update a
update client queueName subscribers = undefined

-- | Add alerts to a queue
addAlerts client queueName alerts = undefined

-- | Update alerts on a queue
updateAlerts client queueName alerts = undefined

-- | Add subscribers to a queue
addSubsrubers client queueName subscribers = undefined
-}