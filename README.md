Haskell language binding for IronMQ. [IronMQ](http://www.iron.io/products/mq) is an elastic message queue for managing data and event flow within cloud applications and between systems. [See How It Works](http://www.iron.io/products/mq/how)

# Getting Started

## Get credentials

To start using iron_mq_python, you need to sign up and get an OAuth2 token.

1. Go to http://iron.io/ and sign up.
2. Get an OAuth2 Token at http://hud.iron.io/tokens

## Install iron_mq_haskell

```sh
cabal install iron-mq
```
## Configure

Create a client which stores your authentication information and server settings/

```haskell
client = Client {
    server = "mq-aws-us-east-1.iron.io",
    porjectID = "500f7b....b0f302e9",
    token = "Et1En7.....0LuW39Q",
    api_version="1"
}
```

## The Basics

### Listing queues

```haskell
queues :: Client -> QueueSummary
queues client
```
returns list of queue names available to the client.

we get a specific queue by name:

```haskell
getQueue :: Client -> QueueName -> IO Queue
myQueue = getQueue client "test_queue"
```

### Push messages on the queue

We can change the default options of a message like so:

```haskell
postMessages :: Client -> QueueName -> [Message] -> IO IronResponse
postMessages client "queueName" [message {body = "message1"}, message {body = "message2"}]
```

We can change the default settings for a message like so:

```haskell
unorthoDoxMessage = message {
    body = "axxon body",
    timeout = 120 -- Timeout, in seconds. After timeout, item will be placed back on queue. Defaults to 60.
    delay = 5 -- The item will not be available on the queue until this many seconds have passed. Defaults to 0.
    expiresIn = 2*24*3600 -- How long, in seconds, to keep the item on the queue before it is deleted.
}

main = postMessage "queueName" [unorthodoxMessage]
```

### Pop messages off the queue

```haskell
getMessages :: Client -> QueueName -> IO MessageList
getMessages client "queneName"
{- 
MessageList 
    {
        messages = [Message {mId = Just "...", mBody = "Word up!", mTimeout = Just 60, mReservedCount = Just 1}]
    }
-}
```

Set max to the number of messages to return, 1 by default. A `timeout` parameter can be used to specify a per-message timeout, or the timeout the message was posted with will be used.

When you pop/get a message from the queue, it will NOT be deleted.
It will eventually go back onto the queue after a timeout if you don't delete it (default timeout is 60 seconds).

### Get message by id
```haskell
getMessageByID :: Client -> QueueName -> ID -> IO Message
main = getMessageById client "test_queue" "1234567789abcdef"
{-
    Message {mId = Just "...", mBody = "Hey yo!", mTimeout = Just 60, mReservedCount = Just 1}
-}
```

### Delete messages from the queue

```haskell
deleteMessages :: Client -> QueueName -> [ID] -> IO IronResponse
deleteMessages client "test_queue" ["123456789abcdef", "fedcba987654321"]
```

### Clear a queue

```haskell
clear :: Client -> QueueName -> IO IronResponse
clear client "test_queue"
```

### Get queue ***size***, ***id***, ***total_messages*** and whole ***info***
```haskell
main = do
-- | getQueue client queueName
myQueue <-  getQueue client queueName
{-
Queue {
    qId = Just "541451a958a847405bfa6316",
    qProjectId = "53f691bd45d4960005000082",
    qName = "test_queue",
    qSize = Just 1,
    qTotalMessages = Just 8,
    qSubscribers = Nothing,
    qRetries = Nothing,
    qPushType = Nothing,
    qRetriesDelay = Nothing
}
-}

-- | qSize queue
qSize myQueue -- Just 1

-- | qName queue
qName myQueue -- "test_queue"

-- | qTotalMessages queue
qTotalMessages -- Just 8

-- | qID queue
qID myQueue -- "541451a958a847405bfa6316"
```

### Peek at messages

To view messages without reserving them, use peek:

```haskell
peek :: Client -> QueueName -> Int -> IO MessageList
peek client "test_queue" 10
{-
    MessageList {messages = [Message {mId = Just "...", mBody = "Word up!", mTimeout = Just 60, mReservedCount = Just 1}]}
-}
```

The third parameter is an integer specifying the maximum number of messages to retrieve.

### Touch a message

To extend the reservation on a reserved message, use touch. The message reservation will be extended by the message's `timeout`.

```haskell
touch :: Client -> QueueName -> ID -> IO IronResponse
touch client "test_queue" messageID
```

### Release a reserved message

To release a message that is currently reserved, use release:

```haskell
release :: Client -> QueueName -> ID -> Int -> IO IronResponse
release client "test_queue" "123456789abcdef" 120 -- message will be released after delay seconds
```

The last parameter is the delay time before the message is released.

### Delete a queue

To delete a queue, use `deleteQueue`:

```haskell
-- | deleteQueue client queueName
deleteQueue client "test_queue"
```

## Push Queues

### Update Queue Information

To update the queue's push type and subscribers, use update:

```haskell
update :: Client -> QueueName -> [Subscriber] -> String -> IO Response
update client "test_queue" [subscriber {url = "http://endpoint1.com"}, subscriber {url = "https://end.point.com/2"}] "unicast"
```

The last parameter is the broadcast type (either "unicast" or "multicast")

### Add subscribers to a push queue

```haskell
addSubscribers :: Client -> QueueName -> [Subscriber] -> IO IRonResponse
addSubscribers client test_queue [subscriber {url = "http://endpoint1.com"}, subscriber {url = "https://end.point.com/2"}])
```

### Remove subscribers from a push queue

```haskell
removeSubscribers :: Client -> QueueName -> [Subscriber]
main = removeSubscribers client "test_queue" [subscriber {url = "http://endpoint1.com"}, subscriber {url ="https://end.point.com/2"})
```

### Get the push statuses of a message

```haskell
getMessagePushStatuses :: Client -> QueueName -> ID -> IO SubscriberList
getMessagePushStatuses client "test_queue" "123456789abcdef"
{-
    subscriberList {
    subscribers = [Subscriber {retriesDelay = Just 60, retriesRemaining" = Just 2, statusCode = Just 200, status = Just "deleted", "url": "http://endpoint1.com", "id": "52.."}, ...]}
-}
```

### Delete a pushed message

If you respond with a 202 status code, the pushed message will be reserved, not deleted, and should be manually deleted. You can get the message ID and subscriber ID from the push message's headers.

```haskell
deleteMessagePushStatus :: Client -> QueueName -> MessageID -> [SuscriberID] -> IO IronResponse
main = deleteMessagePushStatus client "test_queue" "123456789abcdef" "987654321fedcba"
```

## Pull queues

### Add alerts to a queue

```haskell
fixed_desc_alert = alert {type = "fixed", direction = "desc", trigger = 1000}
progressive_asc_alert = alert {type = "progressive", direction = "asc", trigger= 10000}
addAlerts :: Client -> QueueName -> [Alert] -> IO IronResponse
addAlerts client "test_queue" ([fixed_desc_alert, progressive_asc_alert])
```

### Update alerts in a queue

```haskell
progressive_asc_alert = alert {type = "progressive", direction = "asc", trigger = 5000, queue = "q"}
updateAlerts :: Client -> QueueName -> [Alert] -> IO IronResponse
updateAlerts client "test_queue" ([progressive_asc_alert])
```

### Remove alerts from a queue

```haskell
removeAlerts :: Client -> QueueName -> [AlertID] -> IO IronResponse
main = removeAlerts client "test_queue" (['5305d3b5a3e920763013c796', '513015d32b5a3e763013c796'])
```

# Full Documentation

You can find more documentation here:

* http://iron.io
* http://dev.iron.io