module Client where

import Settings
import Network.HTTP.Client
import qualified Data.ByteString as B
import qualified Data.ByteString as LBS
import Network.HTTP.Client.TLS
import Data.Aeson


doRequest method server projectID endpoint token body = do manager <- newManager tlsManagerSettings
                                                           request <- makeRequest url token method body
                                                           httpLbs request manager
                                                           where
                                                                url = concat ["https://", server, "/1/projects/", projectID, "/", endpoint]
                                                                makeRequest url token method body = do initReq <- parseUrl url
                                                                                                       let request = initReq {
                                                                                                           requestHeaders = [
                                                                                                           ("Authorization", (B.append "OAuth " token)), 
                                                                                                           ("Content-Type", "application/json")],
                                                                                                           method = method
                                                                                                       }
                                                                                                       return request

request method endpoint body = doRequest method defaultServer defaultProjectID endpoint defaultToken body

queues = do response <- request "GET" "queues" ""
            let json = (decode $ responseBody response)::Maybe Array
            case json of
                Just something -> print something
                Nothing -> return ()


main = queues