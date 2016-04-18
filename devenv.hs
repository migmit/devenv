{-# LANGUAGE DeriveFunctor, GADTs #-}
module Main(main) where

import Control.Concurrent (threadDelay)
import Control.Monad (ap, forM, forM_, liftM2)
import Control.Exception (IOException, throwIO, try, tryJust)
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import qualified Data.Map as M
import qualified Data.Set as S (Set, empty, fromList, insert, isSubsetOf, member, toList)
import Data.Maybe (fromMaybe)
import Data.Yaml.YamlLight (YamlLight(YStr), parseYamlFile, unMap, unSeq, unStr)
import Network (PortID(PortNumber), connectTo)
import qualified Options.Applicative as O
import Options.Applicative ((<>), (<|>))
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode(WriteMode), hClose, openFile)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, isUserError)
import System.Process
    (callProcess, createProcess, cwd, proc, readProcessWithExitCode, waitForProcess)
import Text.Read (readMaybe)

data FailureOr a = FailureOr String | NoFailure a deriving Functor
instance Applicative FailureOr where
    pure = return
    (<*>) = ap
instance Monad FailureOr where
    fail = FailureOr
    return = NoFailure
    FailureOr s >>= _ = FailureOr s
    NoFailure a >>= f = f a
instance Monoid a => Monoid (FailureOr a) where
    mempty = return mempty
    mappend = liftM2 mappend
recoverFromFailure :: a -> FailureOr a -> a
recoverFromFailure d (FailureOr _) = d
recoverFromFailure _ (NoFailure a) = a
recoverMaybe :: FailureOr a -> Maybe a
recoverMaybe (FailureOr _) = Nothing
recoverMaybe (NoFailure a) = return a
                                                          
type TargetName = String
type CommandLine = String
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
data Target = Target {
      targetDirectory :: FilePath,
      targetDependencies :: S.Set TargetName,
      targetShell :: CommandLine,
      targetWait :: [Wait],
      targetInit :: Maybe CommandLine,
      targetTitle :: String,
      targetError :: Maybe FilePath,
      targetPause :: Maybe Int
    } deriving Show

data Config = Config {configTargets :: M.Map TargetName Target}

orErr :: String -> Maybe a -> FailureOr a
orErr a Nothing = fail a
orErr _ (Just b) = return b

data ReadYamlError = FileDoesNotExist | YamlParsingError String
exceptionToReadYamlError :: IOException -> Maybe ReadYamlError
exceptionToReadYamlError ioe =
    if isDoesNotExistError ioe then Just FileDoesNotExist
    else if isUserError ioe then Just $ YamlParsingError (ioeGetErrorString ioe)
         else Nothing

getStringValue :: M.Map YamlLight YamlLight -> String -> FailureOr String
getStringValue yamlMap key =
    do yamlValue <-
           orErr ("key " ++ key ++ " not found") $ M.lookup (YStr $ U.fromString key) yamlMap
       bsValue <- orErr ("value for key " ++ key ++ " isn't a string") $ unStr yamlValue
       return $ U.toString bsValue

getIntValue :: M.Map YamlLight YamlLight -> String -> FailureOr Int
getIntValue yamlMap key =
    do stringValue <- getStringValue yamlMap key
       orErr ("not a number: " ++ stringValue) $ readMaybe stringValue

getStrings :: M.Map YamlLight YamlLight -> String -> Maybe [String]
getStrings yamlMap key =
    do yamlValue <- M.lookup (YStr $ U.fromString key) yamlMap
       seqValue <- unSeq yamlValue
       values <- sequence $ map unStr seqValue
       return $ map U.toString values

getMapValue :: M.Map YamlLight YamlLight -> String -> Maybe (M.Map YamlLight YamlLight)
getMapValue yamlMap key =
    do yamlValue <- M.lookup (YStr $ U.fromString key) yamlMap
       unMap yamlValue

readWait :: M.Map YamlLight YamlLight -> Maybe [Wait]
readWait =
    flip M.foldlWithKey (return []) $ \waits key val ->
    do ws <- waits
       keyStr <- unStr key
       w <-
           case U.toString keyStr of
             "port" ->
                 do portStr <- unStr val
                    port <- readMaybe $ U.toString portStr
                    return $ Wait $ Port port
             _ -> Nothing
       return $ w : ws

yamlToConfig :: YamlLight -> FilePath -> FailureOr Config
yamlToConfig yaml configFile =
    do firstMap <- orErr "not a map" $ unMap yaml
       let fileDirName = takeDirectory configFile
       let baseName = recoverFromFailure "" $ getStringValue firstMap "basedir"
       secondMap <- orErr "no targets" $ getMapValue firstMap "targets"
       targetsList <-
           flip M.foldMapWithKey secondMap $ \yamlKey yamlTarget ->
               do keyBS <- orErr "key is not a string" $ unStr yamlKey
                  let key = U.toString keyBS
                  target <- orErr ("target " ++ key ++ " is not a map") $ unMap yamlTarget
                  directory <- getStringValue target "directory"
                  shell <- getStringValue target "shell"
                  let pause = recoverMaybe $ getIntValue target "pause"
                  let title = recoverFromFailure key $ getStringValue target "title"
                  let dependencies = fromMaybe [] $ getStrings target "dependencies"
                  let wait = fromMaybe [] $ getMapValue target "wait" >>= readWait
                  let tInit = recoverMaybe $ getStringValue target "init"
                  let err = recoverMaybe $ getStringValue target "error"
                  let dir = fileDirName </> baseName </> directory
                  let result =
                          Target {
                        targetDirectory = dir,
                        targetDependencies = S.fromList dependencies,
                        targetShell = shell,
                        targetWait = wait,
                        targetInit = tInit,
                        targetTitle = title,
                        targetError = fmap (dir </>) err,
                        targetPause = pause
                      }
                  return $ M.singleton key result
       return $ Config {configTargets = targetsList}
                          
readConfig :: FilePath -> IO (FailureOr Config)
readConfig configFile =
    do yamlOrErr <- tryJust exceptionToReadYamlError $ parseYamlFile configFile
       case yamlOrErr of
         Left FileDoesNotExist -> return $ FailureOr $ "file " ++ configFile ++ "doesn't exist"
         Left (YamlParsingError s) -> return $ FailureOr $ "parsing error: " ++ s
         Right yaml ->
             return $ yamlToConfig yaml configFile

getTargetList :: Config -> S.Set TargetName -> Maybe [(TargetName, Target)]
getTargetList config targets =
    fmap (reverse . snd) $ getTargetList' S.empty [] config $ S.toList targets where
        getTargetList'
            :: S.Set TargetName
            -> [(TargetName, Target)]
            -> Config
            -> [TargetName]
            -> Maybe (S.Set TargetName, [(TargetName, Target)])
        getTargetList' processed result _ [] = return (processed, result)
        getTargetList' processed result conf (t:ts) =
            if S.member t processed then getTargetList' processed result conf ts
            else do
              target <- M.lookup t $ configTargets conf
              let deps = targetDependencies target
              (newProcessed, newList) <- getTargetList' processed result conf $ S.toList deps
              getTargetList' (S.insert t newProcessed) ((t, target):newList) conf ts

getSimpleTargetList :: Config -> [TargetName] -> Maybe [(TargetName, Target)]
getSimpleTargetList config targets =
    forM targets $ \t -> fmap ((,) t) $ M.lookup t $ configTargets config  

checkConfigForLoops :: Config -> Bool
checkConfigForLoops = checkConfig' S.empty . configTargets where
    checkConfig' :: S.Set TargetName -> M.Map TargetName Target -> Bool
    checkConfig' found conf =
        let filtered = M.filter (\target -> S.isSubsetOf (targetDependencies target) found) conf
        in case M.minViewWithKey filtered of
             Nothing -> M.null conf
             Just ((name, _), _) ->
                 checkConfig' (S.insert name found) $ M.delete name conf

checkConfigDirectories :: Config -> IO (Maybe (String, String))
checkConfigDirectories = checkConfigDirectories' . M.assocs . configTargets where
    checkConfigDirectories' [] = return Nothing
    checkConfigDirectories' ((n,t):nts) =
        do e <- doesDirectoryExist $ targetDirectory t
           if e then checkConfigDirectories' nts else return $ Just (n, targetDirectory t)

executeOneTarget :: Bool -> String -> TargetName -> Target -> IO Bool
executeOneTarget firstTime sessionName name target =
    let args = if firstTime then ["-dmS", sessionName] else ["-dRS", sessionName, "-X", "screen"]
        errorFile = targetError target
        techArgs = ["-t", targetDirectory target, targetShell target] ++ maybe [] (:[]) errorFile
    in do putStrLn $ name ++ " starting"
          putStrLn $ name ++ "> run: " ++ targetShell target
          forM_ errorFile $ \err ->
              do errExists <- doesFileExist err
                 guardIO (not errExists) $ err ++ " already exists"
                 h <- openFile err WriteMode
                 hClose h
          thisPath <- getExecutablePath
          let targetArgs = ["-t", targetTitle target, thisPath]
          callProcess "screen" $ args ++ targetArgs ++ techArgs
          success <- fmap and $ forM (targetWait target) $ \(Wait w) ->
              do putStrLn $ name ++ "> waiting for: " ++ show w
                 waitFor (targetError target) w
          if success then
              do forM_ (targetPause target) $ \n ->
                     do putStrLn $ name ++ "> pause: " ++ show n
                        threadDelay $ n * 1000000
                 merr <- forM (targetInit target) $ \t ->
                     do putStrLn $ name ++ "> init: " ++ t
                        (exitCode, _, _) <- readProcessWithExitCode "bash" ["-c", t] ""
                        return exitCode
                 let isOK = fromMaybe ExitSuccess merr == ExitSuccess
                 putStrLn $ name ++ if isOK then " runs" else " failed on init"
                 return isOK
          else
              do putStrLn $ name ++ " failed"
                 return False

runWhileOK :: String -> (a -> IO Bool) -> [a] -> IO ()
runWhileOK _ _ [] = return ()
runWhileOK err run (a:as) =
    do ok <- run a
       guardIO ok err
       runWhileOK err run as
                  
startTargets :: String -> Config -> RunOptions -> IO ()
startTargets sessionName config runOptions =
    let execOne (n, t) = executeOneTarget False sessionName n t in
    if runOnly runOptions
    then case getSimpleTargetList config $ runTargets runOptions of
           Nothing -> return ()
           Just ts -> runWhileOK "task failed" execOne ts
    else case getTargetList config $ S.fromList $ runTargets runOptions of
           Nothing -> return ()
           Just [] -> return ()
           Just ((n,t):ts) ->
               do firstSuccess <- executeOneTarget True sessionName n t
                  guardIO firstSuccess "task failed"
                  runWhileOK "task failed" execOne ts

data ProgramOptions =
    ProgramOptions {
      optConfigFile :: Maybe FilePath,
      optSessionName :: Maybe String,
      optOperation :: OperationMode
    }

data RunOptions =
    RunOptions {
      runOnly :: Bool,
      runTargets :: [TargetName]
    }

data OperationMode = RunMode RunOptions | KillMode

data TechnicalOptions =
    TechnicalOptions {
      techDir :: FilePath,
      techShell :: String,
      techErrFile :: Maybe FilePath
    }

data FullOptions = PO ProgramOptions | TO TechnicalOptions

configHelpMessage :: String
configHelpMessage = "List of possible targets in YAML"

sessionHelpMessage :: String
sessionHelpMessage = "Name of screen session to use"

runHelpMessage :: String
runHelpMessage = "Don't run target dependencies"

runOptionsParser :: O.Parser RunOptions
runOptionsParser =
    RunOptions <$>
    O.switch (O.short 'o' <> O.help runHelpMessage) <*>
    O.some (O.argument O.str (O.metavar "Target..."))
                        
optionsParser :: O.Parser ProgramOptions
optionsParser =
    ProgramOptions <$>
    O.optional
         (O.strOption (O.short 'f' <> O.metavar "CONFIG" <> O.help configHelpMessage)) <*>
    O.optional
         (O.strOption (O.short 's' <> O.metavar "SESSION" <> O.help sessionHelpMessage)) <*>
    (
     RunMode <$> runOptionsParser
     <|> KillMode <$ O.flag' () (O.short 'k' <> O.help "Kill everything and clean up")
    )

technicalParser :: O.Parser TechnicalOptions
technicalParser =
    TechnicalOptions <$>
    O.argument O.str (O.hidden <> O.internal) <*>
    O.argument O.str (O.hidden <> O.internal) <*>
    O.optional (O.argument O.str (O.hidden <> O.internal))

fullParser :: O.Parser FullOptions
fullParser =
    PO <$> optionsParser <|>
    TO <$ (O.flag' () (O.short 't' <> O.hidden <> O.internal)) <*> technicalParser

optionsParserInfo :: O.ParserInfo FullOptions
optionsParserInfo =
    O.info (O.helper <*> fullParser) $
    O.fullDesc <>
    O.progDesc "Run targets in screen" <>
    O.header "devenv - start all projects quickly"

guardIO :: Bool -> String -> IO ()
guardIO False s = throwIO $ userError s
guardIO True _ = return ()

maybeError :: Maybe a -> (a -> String) -> IO ()
maybeError me h = forM_ me $ \a -> throwIO $ userError $ h a

main :: IO ()
main =
    do fullOptions <- O.execParser optionsParserInfo
       case fullOptions of
         PO options -> userProcessing options
         TO options -> techProcessing options

userProcessing :: ProgramOptions -> IO ()
userProcessing options =
    do homeDir <- getHomeDirectory
       let configFile = fromMaybe (homeDir </> ".devenv.yml") $ optConfigFile options
       eConfig <- readConfig configFile
       config <- case eConfig of
         FailureOr s -> throwIO $ userError s
         NoFailure c -> return c
       let sessionName = fromMaybe "devenv" $ optSessionName options
       case optOperation options of
         RunMode targets -> 
             do guardIO (checkConfigForLoops config) $ "loops in config"
                notExisting <- checkConfigDirectories config
                maybeError notExisting $ \(n, d) ->
                    "target directory " ++ d ++ " for " ++ n ++ " doesn't exist"
                startTargets sessionName config targets
         KillMode ->
             do let cmdArgs = ["-S", sessionName, "-X", "quit"]
                _ <- readProcessWithExitCode "screen" cmdArgs ""
                forM_ (configTargets config) $ \t ->
                    forM_ (targetError t) $ \fileToRemove ->
                        do exists <- doesFileExist fileToRemove
                           if exists then removeFile fileToRemove else return ()

techProcessing :: TechnicalOptions -> IO ()
techProcessing options =
    do let cp = proc "bash" ["-c", techShell options]
       (_, _, _, handle) <- createProcess (cp {cwd = Just (techDir options)})
       exitCode <- waitForProcess handle
       forM_ (techErrFile options) removeFile
       exitWith exitCode