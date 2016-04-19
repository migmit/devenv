module ScreenTarget where
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (Set, fromList)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), hClose, openFile)
import System.Process
        (StdStream(CreatePipe), callProcess, createProcess, proc, std_out, waitForProcess)

import FailureOr
import Target
import Wait
import Yaml

type CommandLine = String
data ScreenTarget = ScreenTarget {
      targetDirectory :: FilePath,
      targetDependencies :: S.Set TargetName,
      targetShell :: CommandLine,
      targetWait :: [Wait],
      targetInit :: Maybe CommandLine,
      targetTitle :: String,
      targetError :: Maybe FilePath,
      targetPause :: Maybe Int
    } deriving Show

instance IsTarget ScreenTarget where
    readYamlTarget key baseDir yamlMap =
        do directory <- getStringValue yamlMap "directory"
           shell <- getStringValue yamlMap "shell"
           let pause = recoverMaybe $ getIntValue yamlMap "pause"
           let title = recoverFromFailure key $ getStringValue yamlMap "title"
           let deps = fromMaybe [] $ getStrings yamlMap "dependencies"
           let wait = fromMaybe [] $ getMapValue yamlMap "wait" >>= readWait
           let tInit = recoverMaybe $ getStringValue yamlMap "init"
           let err = recoverMaybe $ getStringValue yamlMap "error"
           let dir = baseDir </> directory
           return ScreenTarget {
                        targetDirectory = dir,
                        targetDependencies = S.fromList deps,
                        targetShell = shell,
                        targetWait = wait,
                        targetInit = tInit,
                        targetTitle = title,
                        targetError = fmap (dir </>) err,
                        targetPause = pause
                      }
    dependencies = targetDependencies
    checkTarget t key =
        do let dir = targetDirectory t
           e <- doesDirectoryExist dir
           if e then return Nothing
           else return $ Just $ "target directory " ++ dir ++ " for " ++ key ++ " doesn't exist"
    executeOneTarget skipRunning firstTime sessionName name target =
        let args =
                if firstTime then ["-dmS", sessionName] else ["-dRS", sessionName, "-X", "screen"]
            techArgs =
                ["-t", targetDirectory target, targetShell target] ++
                maybe [] (:[]) (targetError target)
        in do needRun <- checkNeedRun skipRunning target
              if needRun then
                  do putStrLn $ name ++ "> run: " ++ targetShell target
                     thisPath <- getExecutablePath
                     let targetArgs = ["-t", targetTitle target, thisPath]
                     callProcess "screen" $ args ++ targetArgs ++ techArgs
                     success <- waitAll name (targetError target) (targetWait target)
                     if success then
                         do forM_ (targetPause target) $ \n ->
                                do putStrLn $ name ++ "> pause: " ++ show n
                                   threadDelay $ n * 1000000
                            merr <-
                                forM (targetInit target) $ \t ->
                                    do putStrLn $ name ++ "> init: " ++ t
                                       let cp = proc "bash" ["-c", t]
                                       (_, _, _, ph) <- createProcess cp {std_out = CreatePipe}
                                       waitForProcess ph
                            if fromMaybe ExitSuccess merr == ExitSuccess
                            then return ExecResult {erContinue = True, erDetails = ERDOK}
                            else return ExecResult {erContinue = False, erDetails = ERDFailed}
                     else return ExecResult {erContinue = False, erDetails = ERDFailed}
              else return ExecResult {erContinue = True, erDetails = ERDAlreadyRuns}
    cleanOneTarget t =
        forM_ (targetError t) $ \fileToRemove ->
            do exists <- doesFileExist fileToRemove
               if exists then removeFile fileToRemove else return ()

checkNeedRun :: Bool -> ScreenTarget -> IO Bool
checkNeedRun skipRunning t =
    case targetError t of
      Nothing -> return True
      Just err ->
          do errExists <- doesFileExist err
             if errExists then
                 do guardIO skipRunning $ err ++ " already exists"
                    return False
             else do
               h <- openFile err WriteMode
               hClose h
               return True

waitAll :: TargetName -> Maybe FilePath -> [Wait] -> IO Bool
waitAll _ _ [] = return True
waitAll key errorFile (Wait w:ws) =
    do putStrLn $ key ++ "> waiting for: " ++ show w
       result <- waitFor errorFile w
       if result then waitAll key errorFile ws else return False
                            
