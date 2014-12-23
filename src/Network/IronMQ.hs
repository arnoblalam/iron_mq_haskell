{-# LANGUAGE OverloadedStrings #-}
module Network.IronMQ (module Network.IronMQ, Network.IronMQ.Types.Client(..)) where

import Network.Wreq
import Network.Wreq.Types (Postable)
import Control.Lens
import Data.Aeson (FromJSON)
import Data.Text (Text, append)
import Data.Text.Encoding (encodeUtf8)
import Network.IronMQ.Types
import Control.Monad (liftM)
import Network.HTTP.Client (RequestBody(..))


-- | Some conv

-- | Construct a base URL from a client
baseurl :: Client -> String
baseurl client = concat ["https://", server client, "/", apiVersion client,
                            "/projects/", projectID client]

-- | An empty body for POST/PUT requests
emptyBody = Raw "application/json" $ RequestBodyLBS ""

-- | Make a get request to an endpoint using connection info from client and
-- query string set to parameters. Return the JSON results
getJSONWithOpts :: FromJSON a => Client -> String -> [(Text, Text)] -> IO a
getJSONWithOpts client endpoint parameters = do
    let url = baseurl client ++ endpoint
        getOpts = defaults & header "Content-Type" .~ ["application/json"]
                           & params .~ ("oauth", token client) : parameters
    response <- asJSON =<< getWith getOpts url
    return (response ^. responseBody)

-- | Make a JSON post request to an endpoint using connection info from client
-- and body set to body
postJSONWithBody :: (Postable a, FromJSON b) => Client -> String -> a -> IO b
postJSONWithBody client endpoint body = do
    let url = baseurl client ++ endpoint
        postOpts = defaults & header "Authorization" .~ [encodeUtf8 ("OAuth " `append` token client)]
    response <- asJSON =<< postWith postOpts url body
    return (response ^. responseBody)

postJSON :: FromJSON b => Client -> String -> IO b
postJSON client endpoint = postJSONWithBody client endpoint emptyBody

-- | Make a get request to an endpoint using the connection info from client.
-- Return the JSON results.
getJSON ::FromJSON a => Client -> String -> IO a
getJSON client s = getJSONWithOpts client s []

-- | Get a list of queues available to the client
queues :: Client -> IO [QueueSummary]
queues client = getJSON client "/queues"

-- | Get a queue from the client
getQueue :: Client -> String -> IO Queue
getQueue client queueName = getJSON client ("/queues/" ++ queueName)

-- | Get a list of messages on a queue
getMessages :: Client -> String -> IO MessageList
getMessages client queueName = getJSON client
    ("/queues/" ++ queueName ++ "/messages")

getMessageById :: Client -> String -> String -> IO Message
getMessageById client queueName messageID = getJSON client
    ("/queues/" ++ queueName ++ "/messages/" ++ messageID)

-- | Get the push status of a message
getMessagePushStatuses :: Client -> String -> String -> IO PushStatus
getMessagePushStatuses client queueName messageID = undefined

-- | Post messages to a queue
postMessages :: Client -> String -> MessageList -> IO IronResponse
postMessages client queue messageList = undefined

-- | Clear all messages from a queue
clear :: Client -> String -> IO IronResponse
clear client queueName = postJSON client ("/queues/" ++ queueName  ++ "/clear")

-- | Delete a queue
deleteQueue :: Client -> String -> IO IronResponse
deleteQueue client queueName = undefined

-- | Delete a message from a queue
deleteMessage :: Client -> String -> String -> IO IronResponse
deleteMessage client queueName messageID = undefined

-- | Delete several messages from a queue
deleteMessages :: Client -> String -> [String] -> IO IronResponse
deleteMessages client queueName meessageIDs = undefined

-- | Delete the message push status of a message
deleteMessagePushStatus :: String -> String -> IO IronResponse
deleteMessagePushStatus queueName messageID = undefined

-- | Remove alerts from a queue
deleteAlerts :: Client -> String -> [String] -> IO IronResponse
deleteAlerts client queueName alertIDs = undefined

-- | Remove an alert from a queue
deleteAlert :: Client -> String -> String -> IO IronResponse
deleteAlert client queueName alertID = undefined

-- Remove subscribers from a queue
deleteSubscribers client queueName subscribers = undefined

-- | Take a look at the next item on the queue
peek :: Client -> String -> IO IronResponse
peek client queueName = undefined

-- | Touch a message on the queue
touch :: Client -> String -> String -> IO IronResponse
touch client queueName messageID = undefined

-- | Update a
update client queueName subscribers = undefined

-- | Add alerts to a queue
addAlerts client queueName alerts = undefined

-- | Update alerts on a queue
updateAlerts client queueName alerts = undefined

-- | Add subscribers to a queue
addSubsrubers client queueName subscribers = undefined