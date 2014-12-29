{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IronMQ.Types where

import Data.Aeson.TH
import Data.Aeson.Types (camelTo)
import Data.Text (Text)
import Data.Char (toLower)

data Client = Client {
    token :: Text,
    projectID :: Text,
    server :: Text,
    apiVersion :: Text
} deriving (Show)

data QueueSummary = QueueSummary {
        qsId :: Text,
        qsProjectId :: Text,
        qsName :: Text
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''QueueSummary)


data Subscriber = Subscriber {
    sUrl :: Text,
    sHeaders :: Maybe Text,
    sRetriesRemaining :: Maybe Int,
    sStatusCode :: Maybe Int,
    sStatus :: Maybe Text,
    sId :: Maybe Text
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''Subscriber)

data Queue = Queue {
        qId :: Maybe Text,
        qProjectId :: Text,
        qName :: Text,
        qSize :: Maybe Int,
        qTotalMessages :: Maybe Int,
        qSubscribers :: Maybe [Subscriber],
        qRetries :: Maybe Int,
        qPushType :: Maybe Text,
        qRetriesDelay :: Maybe Int
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''Queue)

data PushStatus = PushStatus {
    psSubscribers :: [Subscriber]
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''PushStatus)

data Message = Message {
        mId :: Maybe Text,
        mBody :: Text,
        mTimeout :: Maybe Int,
        mReservedCount :: Maybe Int
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''Message)

data MessageList = MessageList {
        mlMessages :: [Message]
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''MessageList)

data IronResponse = IronResponse {
        irIds :: Maybe [Text],
        irMsg :: Text
} deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 3.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''IronResponse)

data Alert = Alert {
        aType :: Text,
        aTrigger::Int,
        aQueue::Text,
        aDirection::Maybe Text,
        aSnooze::Maybe Int
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.camelTo '_', constructorTagModifier = map toLower, omitNothingFields = True} ''Alert)

-- * Some default constructors for convinience

-- | A default constructor for a subscriber
subscriber :: Subscriber
subscriber = Subscriber {
    sUrl = "",
    sRetriesRemaining = Nothing,
    sHeaders = Nothing,
    sStatusCode = Nothing,
    sStatus = Nothing,
    sId = Nothing
}

-- | A default constructor for a queue
queue :: Queue
queue = Queue {
    qId = Nothing,
    qProjectId = "",
    qName = "",
    qSize = Nothing,
    qTotalMessages = Nothing,
    qSubscribers = Nothing,
    qRetries = Nothing,
    qPushType = Nothing,
    qRetriesDelay = Nothing
}

-- | A default constructor for message
message :: Message
message = Message {
    mId = Nothing,
    mBody = "",
    mTimeout = Nothing,
    mReservedCount = Nothing
}