{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.IronMQ

testClient = Client {
    token = "_B6KOfA16D4AmW2NwSCw12mgVxk",
    projectID = "53f691bd45d4960005000082",
    server = "mq-aws-us-east-1.iron.io",
    apiVersion = "1"
}

main = getMessages testClient "default"