{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Text (append, pack)

import Network.IronMQ
import Network.IronMQ.Types

main = defaultMain [bench "get queue info, post a message, get messages, delete message" $ nfIO (doStuff)]

testClient = Client {
    token = "_B6KOfA16D4AmW2NwSCw12mgVxk",
    projectID = "53f691bd45d4960005000082",
    server = "mq-aws-us-east-1.iron.io",
    apiVersion = "1"
}

doStuff = do
    qs <- queues testClient
    postMessages testClient "default" [message{mBody = "This is message number "}]
    messageList <- getMessages testClient "default"
    let messageID = mId (head (messages messageList))
    case messageID of
        Just x -> deleteMessage testClient "default" x
    return ()