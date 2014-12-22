{-# LANGUAGE OverloadedStrings #-}
module FileViewer where
 
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           Prelude                        (IO, read, ($), print)
import           System.Environment             (getEnv)
 
fileViewer :: IO a -> IO ()
fileViewer action = do
    void $ forkIO $ void action
    portS <- getEnv "PORT"
    let port = read portS
    run port $ staticApp $ defaultFileServerSettings "."

main :: IO()
main = fileViewer $ print ""