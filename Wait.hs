{-# LANGUAGE GADTs #-}
module Wait where
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Network (PortID(PortNumber), connectTo)
import System.Directory (doesFileExist)

class Show w => Waitable w where
    waitFor :: Maybe FilePath -> w -> IO Bool

newtype Port = Port Int deriving Show
instance Waitable Port where
    waitFor errorFile (Port n) = waitForPort $ fromInteger $ toInteger n
        where
          waitForPort p =
              do goOn <- maybe (return True) doesFileExist errorFile
                 if goOn then
                     do tryResult <- try $ connectTo "localhost" (PortNumber p)
                        threadDelay 1000000
                        case tryResult of
                          Left e -> let _ = e :: IOException in waitForPort p
                          Right _ -> return True
                 else
                     return False

data Wait where Wait :: Waitable w => w -> Wait
instance Show Wait where show (Wait w) = show w

