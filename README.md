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
queues client
```
returns list of queue names available to the client

we get a specific queue by name:

```haskell
myQueue = queue client "test_queue"
```

### **Push** a message(s) on the queue:

We can change the default options of a message like so:

```haskell
main = postMessage "queueName" [message {body = "message1", message {body = "message2"}]
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

### **Pop** messages off the queue:

```haskell
-- | getMessage queueName max timeout
getMessages "queneName" 100, 10) -- MessageList {messages = [Message {mId = Just "...", mBody = "Word up!", mTimeout = Just 60, mReservedCount = Just 1}]}
```

Set max to the number of messages to return, 1 by default. An optional `timeout` parameter can be used to specify a per-message timeout, or the timeout the message was posted with will be used.

When you pop/get a message from the queue, it will NOT be deleted.
It will eventually go back onto the queue after a timeout if you don't delete it (default timeout is 60 seconds).

### Get message by id
```haskell
getMessageById client "xxxxxxxx" -- Message {mId = Just "...", mBody = "Hey yo!", mTimeout = Just 60, mReservedCount = Just 1}
```

### **Delete** message from the queue:


```haskell
deleteMessages client [messageIDs]
```

### ***Clear*** a queue:

```haskell
clear queueName
```

### Get queue ***size***, ***id***, ***total_messages*** and whole ***info***
```haskell
main = do
myQueue =  getQueue client queueName
{-
Queue {
    qId = Just "541451a958a847405bfa6316",
    qProjectId = "53f691bd45d4960005000082",
    qName = "default",
    qSize = Just 1,
    qTotalMessages = Just 8,
    qSubscribers = Nothing,
    qRetries = Nothing,
    qPushType = Nothing,
    qRetriesDelay = Nothing
}
-}
qSize myQueue -- Just 1
qName myQueue -- "default"
qTtalMessages -- Just 8
qId myQueue -- "541451a958a847405bfa6316"
```

### Peek at messages:

To view messages without reserving them, use peek:

```haskll
main = peek client queueName max -- MessageList {messages = [Message {mId = Just "...", mBody = "Word up!", mTimeout = Just 60, mReservedCount = Just 1}]}
```

### Touch a message:

To extend the reservation on a reserved message, use touch. The message reservation will be extended by the message's `timeout`.

```haskell
main = touch client queue msg_id
```

### Release a reserved message:

To release a message that is currently reserved, use release:

```haskell
main = release client queueName msg_id delay -- message will be released after delay seconds
```

### Delete a queue

To delete a queue, use `delete_queue`:

```haskell
main = deleteQueue client queueName
```

## Push Queues

### Update Queue Information

To update the queue's push type and subscribers, use update:

```hasekll
update client queueName [subscriber {url = "http://endpoint1.com", subscriber {url = "https://end.point.com/2"}] "unicast"
```

### Add subscribers to a push queue

```haskell
addSubscribers client queueName [subscriber "http://endpoint1.com", subscriber "https://end.point.com/2"])
```

### Remove subscribers from a push queue

```haskell
removeSubscribers client queueName [subscriber {url = "http://endpoint1.com"}, subscriber {url ="https://end.point.com/2"})
```

### Get the push statuses of a message

```haskell
getMessagePushStatuses client queueName messageID -- subscriberList {subscribers = [Subscriber {retriesDelay = Just 60, retriesRemaining" = Just 2, statusCode = Just 200, status = Just "deleted", "url": "http://endpoint1.com", "id": "52.."}, ...]}
```

### Delete a pushed message

If you respond with a 202 status code, the pushed message will be reserved, not deleted, and should be manually deleted. You can get the message ID and subscriber ID from the push message's headers.

```haskell
deleteMessagePushStatus client queueName messageID, subscriberID
```

## Pull queues

### Add alerts to a queue:

```haskell
fixed_desc_alert = alert {'type' = 'fixed', 'direction' = 'desc', 'trigger' = 1000}
progressive_asc_alert = alter {'type' = 'progressive', 'direction'= 'asc', 'trigger'= 10000}
main = addAlerts client queue ([fixed_desc_alert, progressive_asc_alert])
```

### Update alerts in a queue:

```haskell
progressive_asc_alert = alert {'type': 'progressive', 'direction': 'asc', 'trigger': 5000, 'queue': 'q'}
updateAlerts client queue ([progressive_asc_alert])
```

### Remove alerts from a queue:

```haskell
removeAlerts client queue (['5305d3b5a3e920763013c796', '513015d32b5a3e763013c796'])
```

### Remove single alert from a queue:

# Full Documentation

You can find more documentation here:

* http://iron.io
* http://dev.iron.io