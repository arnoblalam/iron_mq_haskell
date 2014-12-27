{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.IronMQ.Types where

import GHC.Generics
import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text)

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

instance FromJSON QueueSummary where
        parseJSON (Object v) = QueueSummary <$>
                v .: "id" <*>
                v .: "project_id" <*>
                v .: "name"
        parseJSON _ = mzero


data Subscriber = Subscriber {
    sUrl :: Text,
    sHeaders :: Maybe Text,
    sRetriesRemaining :: Maybe Int,
    sStatusCode :: Maybe Int,
    sStatus :: Maybe Text,
    sId :: Maybe Text
} deriving (Show, Generic)

instance FromJSON Subscriber where
    parseJSON (Object v) = Subscriber <$>
        v .: "url" <*>
        v .:? "headers" <*>
        v .:? "retries_remaining" <*>
        v .:? "status_code" <*>
        v .:? "status" <*>
        v .:? "id"
    parseJSON _ = mzero

instance ToJSON Subscriber

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
} deriving (Show, Generic)

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

instance FromJSON Queue where
        parseJSON (Object v) = Queue <$>
                v .:? "id" <*>
                v .: "project_id" <*>
                v .: "name" <*>
                v .:? "size" <*>
                v .:? "total_messages" <*>
                v .:? "subscribers" <*>
                v .:? "retries" <*>
                v .:? "push_type" <*>
                v .:? "retries_delay"
        parseJSON _ = mzero

instance ToJSON Queue

data QueueInfo = QueueInfo {
        qiSize :: Int
} deriving (Show, Generic)

instance FromJSON QueueInfo where
        parseJSON (Object v) = QueueInfo <$>
                v .: "size"
        parseJSON _ = mzero

data PushStatus = PushStatus {
    psSubscribers :: [Subscriber]
} deriving (Show)

instance FromJSON PushStatus where
        parseJSON (Object v) = PushStatus <$>
                v .: "subscribers"
        parseJSON _ = mzero

data Message = Message {
        mId :: Maybe Text,
        mBody :: Text,
        mTimeout :: Maybe Int,
        mReservedCount :: Maybe Int
} deriving (Show)

-- | A default constructor for message
message :: Message
message = Message Nothing "" Nothing Nothing

instance FromJSON Message where
        parseJSON (Object v) = Message <$>
                v .:? "id" <*>
                v .: "body" <*>
                v .:? "timeout" <*>
                v .:? "reserved_count"
        parseJSON _ = mzero
instance ToJSON Message where
        toJSON (Message _ body _ _) = object ["body" Data.Aeson..= body]

data MessageList = MessageList {
        messages :: [Message]
} deriving (Show, Generic)

instance FromJSON MessageList

instance ToJSON MessageList

data IronResponse = IronResponse {
        irIds :: Maybe [Text],
        irMsg :: Text
} deriving (Show, Generic)

instance FromJSON IronResponse where
        parseJSON (Object v) = IronResponse <$>
                v .:? "ids" <*>
                v .: "msg"
        parseJSON _ = mzero

data Alert = Alert {
        aType :: Text, 
        aTrigger::Int, 
        aQueue::Text, 
        aDirection::Maybe Text, 
        aSnooze::Maybe Int
} deriving (Show, Generic)

instance FromJSON Alert where
        parseJSON (Object v) = Alert <$>
                v .: "type" <*>
                v .: "trigger" <*>
                v .: "queue" <*>
                v .:? "direction" <*>
                v .:? "snooze"
        parseJSON _ = mzero

instance ToJSON Alert