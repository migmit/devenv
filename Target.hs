{-# LANGUAGE GADTs #-}
module Target where
import Data.Map (Map)
import Data.Set (Set)
import Data.Yaml.YamlLight (YamlLight)

import FailureOr

type TargetName = String

data ExecResultDetails = ERDOK | ERDFailed | ERDFailedInit | ERDAlreadyRuns
data ExecResult = ExecResult {erContinue :: Bool, erDetails :: ExecResultDetails}

class Show t => IsTarget t where
    readYamlTarget :: TargetName -> FilePath -> Map YamlLight YamlLight -> FailureOr t
    dependencies :: t -> Set TargetName
    checkTarget :: t -> TargetName -> IO (Maybe String)
    executeOneTarget :: Bool -> Bool -> String -> TargetName -> t -> IO ExecResult
    cleanOneTarget :: t -> IO ()

data Target where Target :: IsTarget t => t -> Target

executeTarget :: IsTarget t => Bool -> Bool -> String -> TargetName -> t -> IO Bool
executeTarget skipRunning firstTime sessionName key t =
    do putStrLn $ key ++ " starting"
       result <- executeOneTarget skipRunning firstTime sessionName key t
       case erDetails result of
         ERDOK -> putStrLn $ key ++ " runs"
         ERDFailed -> putStrLn $ key ++ " failed"
         ERDFailedInit -> putStrLn $ key ++ " failed on init"
         ERDAlreadyRuns -> putStrLn $ key ++ " already runs"
       return $ erContinue result
