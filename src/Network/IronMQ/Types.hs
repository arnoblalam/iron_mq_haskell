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
    projectID :: String,
    server :: String,
    apiVersion :: String
}

data QueueSummary = QueueSummary {
        qsId :: String,
        qsProjectId :: String,
        qsName :: String
} deriving (Show)

instance FromJSON QueueSummary where
        parseJSON (Object v) = QueueSummary <$>
                v .: "id" <*>
                v .: "project_id" <*>
                v .: "name"
        parseJSON _ = mzero
data Subscribers = Subscribers {
    url :: String,
    headers :: String
} deriving (Show, Generic)

instance FromJSON Subscribers


data Queue = Queue {
        qId :: Maybe String,
        qProjectId :: String,
        qName :: String,
        qSize :: Maybe Int,
        qTotalMessages :: Maybe Int,
        qSubscribers :: Maybe Subscribers,
        qRetries :: Maybe Int,
        qPushType :: Maybe String,
        qRetriesDelay :: Maybe Int
} deriving (Show)

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

data QueueInfo = QueueInfo {
        qiSize :: Int
} deriving (Show, Generic)

instance FromJSON QueueInfo where
        parseJSON (Object v) = QueueInfo <$>
                v .: "size"
        parseJSON _ = mzero

data PushStatus = PushStatus {
        retriesRemaining :: Int
} deriving (Show)

instance FromJSON PushStatus where
        parseJSON (Object v) = PushStatus <$>
                v .: "retries_remaining"
        parseJSON _ = mzero

data Message = Message {
        mId :: Maybe String,
        mBody :: Text,
        mTimeout :: Maybe Int,
        mReservedCount :: Maybe Int
} deriving (Show)

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
        ids :: Maybe String,
        msg :: String
} deriving (Show, Generic)

instance FromJSON IronResponse