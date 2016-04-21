{-# LANGUAGE GADTs #-}
module Wait(Wait(Wait), readWaits, waitFor) where
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Network (PortID(PortNumber), connectTo)
import System.Directory (doesFileExist)

import Yaml

class Show w => Waitable w where
    waitFor :: Maybe FilePath -> w -> IO Bool
    readWait :: YamlMap -> YamlM w

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
    readWait yMap = fmap Port $ yamlGetInt yMap "port"

data Wait where Wait :: Waitable w => w -> Wait
instance Show Wait where show (Wait w) = show w

knownWaits :: [String]
knownWaits = ["port"]

readWaits :: YamlMap -> YamlM [Wait]
readWaits yMap =
    fmap catMaybes $ forM knownWaits $ \name ->
        case name of
          "port" ->
              do mw <- recoverMaybeYamlM $ readWait yMap
                 return $ fmap Wait (mw :: Maybe Port)
          _ -> return Nothing
              