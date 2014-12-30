{-# LANGUAGE OverloadedStrings #-}
module Network.IronMQ (
    Client(..),
    queue,
    message,
    queues,
    getQueue,
    getMessages',
    getMessages,
    getMessageById,
    postMessages,
    clear,
    deleteQueue,
    deleteMessage,
    peek',
    peek,
    touch,
    release,
    update
) where

import Network.Wreq
import Network.Wreq.Types (Postable)
import Control.Lens
import Data.Aeson (FromJSON, toJSON)
import Data.Map (fromList, Map)
import Data.Text (Text, unpack, pack, concat)
import Data.Text.Encoding (encodeUtf8)
import Network.IronMQ.Types
import Network.HTTP.Client (RequestBody(..))
import qualified Network.Wreq.Session as S


-- * Some type synonyms to help keel track of things

type Endpoint = Text

type Param = (Text, Text)

type QueueName = Text

type ID = Text -- could be a message ID, subscriber ID or whatever

-- * Some functions to make HTTP requests easier

-- | Construct a base URL for HTTP requests from a client
baseurl :: Client -> Text
baseurl client = Data.Text.concat ["https://", server client,  "/", apiVersion client
                           , "/projects/", projectID client]
-- | An empty body for POST/PUT requests
emptyBody :: Payload
emptyBody = Raw "application/json" $ RequestBodyLBS ""

-- | Make a GET request to an endpoint using connection info from client and
-- query string set to parameters. Return the JSON results
getJSONWithOpts :: FromJSON a => Client -> S.Session -> Endpoint -> [Param] -> IO a
getJSONWithOpts client sess endpoint parameters =  do
    let url = Data.Text.concat [baseurl client, endpoint]
        getOpts = defaults & header "Content-Type" .~ ["application/json"]
                           & params .~ ("oauth", token client) : parameters
    response <- asJSON =<< S.getWith getOpts sess (unpack url)
    return (response ^. responseBody)

-- | Make a GET request to an endpoint using the connection info from client.
-- Return the JSON results.
getJSON ::FromJSON a => Client -> S.Session -> Endpoint -> IO a
getJSON client sess s = getJSONWithOpts client sess s []

-- | Make a POST a request to an endpoint using connection info from client
-- and the body provided. Return the JSON response.
postJSONWithBody :: (Postable a, FromJSON b) => Client -> S.Session -> Endpoint -> a -> IO b
postJSONWithBody client sess endpoint body =  do
    let url = Data.Text.concat [baseurl client, endpoint]
        postOpts = defaults
                & header "Content-Type" .~ ["application/json"]
                & header "Authorization" .~ [encodeUtf8 (Data.Text.concat ["OAuth ", token client])]
    response <- asJSON =<< S.postWith postOpts sess (unpack url) body
    return (response ^. responseBody)

-- | Make a POST request to an endpoint using the connection into from client
-- and an empty body. Returb the JSON response.
postJSON :: (FromJSON b) => Client -> S.Session -> Endpoint -> IO b
postJSON client sess endpoint = postJSONWithBody client sess endpoint emptyBody

{-
deleteJSONWithBody :: (Postable a, FromJSON b) => Client ->Endpoint -> Postable a -> IO b
deleteJSONWithBody client endpoint body = do
        let url = baseurl client `append` endpoint
            deleteOpts = defaults
                & header "Content-Type" .~ ["application/json"]
                & header "Authorization" .~ [encodeUtf8 ("OAuth " `append` token client)]
        response <- asJSON =<< deleteWith deleteOpts (unpack url)
        return (response ^. responseBody)
-}

deleteJSON :: FromJSON a => Client -> S.Session -> Endpoint -> IO a
deleteJSON client sess endpoint =  do
        let url = Data.Text.concat [baseurl client, endpoint]
            deleteOpts = defaults
                & header "Content-Type" .~ ["application/json"]
                & header "Authorization" .~ [encodeUtf8 (Data.Text.concat ["OAuth ", token client])]
        response <- asJSON =<< S.deleteWith deleteOpts sess (unpack url)
        return (response ^. responseBody)

-- * The public API

-- | Get a list of queues available to the client
queues :: Client -> S.Session -> IO [QueueSummary]
queues client sess = getJSON client sess "/queues"

-- | Get a queue from the client
getQueue :: Client -> S.Session -> QueueName -> IO Queue
getQueue client sess queueName = getJSON client sess (Data.Text.concat ["/queues/", queueName])


-- | Get a list of messages on the queue (allowing specification of number of messages and delay)
getMessages' :: Client -> S.Session -> QueueName -> Maybe Int -> Maybe Int -> IO MessageList
getMessages' client sess queueName max_ timeout = getJSONWithOpts client sess endpoint params' where
    endpoint = Data.Text.concat ["/queues/", queueName, "/messages"]
    params' = case (max_, timeout) of
                (Nothing, Nothing)      ->      []
                (Just x, Nothing)       ->      [("n", pack (show x))]
                (Nothing, Just y)       ->      [("wait", pack (show y))]
                (Just x, Just y)        ->      [("n", pack (show x)), ("wait", pack (show y))]

-- | Get a list of messages on a queue
getMessages :: Client -> S.Session -> QueueName -> IO MessageList
getMessages client sess queueName = getMessages' client sess queueName Nothing Nothing

-- | Get a message by ID
getMessageById :: Client -> S.Session -> QueueName -> ID -> IO Message
getMessageById client sess queueName messageID = getJSON client sess
    (Data.Text.concat ["/queues/", queueName, "/messages/", messageID])
{-
-- | Get the push status of a message
getMessagePushStatus :: Client -> QueueName -> ID -> IO PushStatus
getMessagePushStatus client queueName messageID = undefined
-}
-- | Post messages to a queue
postMessages :: Client -> S.Session -> QueueName -> [Message] -> IO IronResponse
postMessages client sess queueName messages_ = postJSONWithBody client sess endpoint body where
        endpoint = Data.Text.concat ["/queues/", queueName, "/messages"]
        body = toJSON MessageList {mlMessages = messages_}


-- | Clear all messages from a queue
clear :: Client -> S.Session -> QueueName -> IO IronResponse
clear client sess queueName = postJSON client sess endpoint where
    endpoint = Data.Text.concat ["/queues/", queueName , "/clear"]

-- | Delete a queue
deleteQueue :: Client -> S.Session -> QueueName -> IO IronResponse
deleteQueue client sess queueName = deleteJSON client sess endpoint where
        endpoint = Data.Text.concat ["/queues/", queueName]

-- | Delete a message from a queue
deleteMessage :: Client -> S.Session -> QueueName -> ID -> IO IronResponse
deleteMessage client sess queueName messageID = deleteJSON client sess endpoint where
        endpoint = Data.Text.concat ["/queues/", queueName, "/messages/", messageID]

{-
-- | Delete several messages from a queue
deleteMessages :: Client -> QueueName -> [ID] -> IO IronResponse
deleteMessages client queueName meessageIDs = deleteJSON client endpoint


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
-}

-- | Take a look at the next item on the queue
peek' :: Client -> S.Session -> QueueName -> Maybe Int -> IO MessageList
peek' client sess queueName max_ = getJSONWithOpts client sess endpoint opts where
        opts = case max_ of
                Nothing -> []
                Just x -> [("n", pack (show x))]
        endpoint = Data.Text.concat ["/queues/", queueName, "/messages/peek"]

peek :: Client -> S.Session -> QueueName -> IO MessageList
peek client sess queueName = peek' client sess queueName Nothing

-- | Touch a message on the queue
touch :: Client -> S.Session -> QueueName -> ID -> IO IronResponse
touch client sess queueName messageID = postJSON client sess endpoint where
        endpoint = Data.Text.concat ["/queues/", queueName, "/messages/", pack (show messageID), "/touch"]

-- | Release a reserved meesage
release :: Client -> S.Session -> QueueName -> ID -> Maybe Int -> IO IronResponse
release client sess queueName messageID delay = postJSONWithBody client sess endpoint body where
        endpoint = Data.Text.concat ["/queues/", queueName, "/messages/", pack (show messageID), "/release"]
        body = case delay of
                Nothing -> toJSON (fromList []::Map Text Int)
                Just x -> toJSON (fromList [("delay", x)]::Map Text Int)

-- | Update a queue.
update :: Client -> S.Session -> QueueName -> Queue -> IO IronResponse
update client sess queueName queue_ = postJSONWithBody client sess endpoint body where
        endpoint = Data.Text.concat ["/queues/", queueName]
        body = toJSON queue_
{-
-- | Add alerts to a queue
addAlerts :: Client -> QueueName -> [Alert] -> IO IronResponse
addAlerts client queueName alerts = postJSONWithBody client endpoint where
    endpoint = "/queues/" `append` queueName `append`

-- | Update alerts on a queue
updateAlerts :: Client -> QueueName -> [Alert] -> IO IronResponse
updateAlerts client queueName alerts = undefined

-- | Add subscribers to a queue
addSubscribers :: Client -> QueueName -> [Subscriber] -> IO IronResponse
addSubscribers client queueName subscribers = undefined
-}