module Main where

import Prelude hiding (catch)
import Control.Exception ( SomeException(..),
                           AsyncException(..),
                           catch, handle, throw)
import System.Posix.Signals
import Control.Concurrent

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.Version
import Data.List
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Regex.Posix

import System.IO
import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import Language.Egison
import Language.Egison.Util
import qualified Paths_egison_tutorial as P

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optPrompt = prompt} -> do
                env <- initialEnv
                case nonOpts of
                    [] -> showBanner >> repl env prompt
                    _ -> printHelp

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optPrompt :: String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optPrompt = "> "
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "set prompt string"
  ]

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egison-tutorial [options]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --prompt string       Set prompt of the interpreter"
  putStrLn ""
  exitWith ExitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion P.version 
  exitWith ExitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Tutorial for Version " ++ showVersion P.version ++ " (C) 2013-2014 Satoshi Egi"
  putStrLn $ "http://www.egison.org"
  putStrLn $ "Welcome to Egison Tutorial!"

showFinishMessage :: IO ()
showFinishMessage = do
  putStrLn $ "You have finished this section."
  putStrLn $ "Thank you!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Tutorial.\nByebye."

yesOrNo :: String -> IO Bool
yesOrNo question = do
  putStr $ question
  putStr $ " (Y/n): "
  hFlush stdout
  input <- getLine
  case input of
   [] -> return True
   ('y':_) -> return True
   ('Y':_) -> return True
   ('n':_) -> return False
   ('N':_) -> return False
   _ -> yesOrNo question

nth n = head . drop (n - 1)

selectSection :: Tutorial -> IO Section
selectSection tutorial@(Tutorial sections) = do
  putStrLn $ take 30 $ repeat '='
  putStrLn $ "List of sections in the tutorial"
  putStrLn $ show tutorial
  putStrLn $ take 30 $ repeat '='
  putStrLn $ "Choose a section to learn."
  putStr   $ "(1-" ++ show (length sections) ++ "): "
  hFlush stdout
  n <- getNumber
  return $ nth n sections

getNumber :: IO Int
getNumber = do
  input <- getLine
  case input of
    ('1':_) -> return 1
    ('2':_) -> return 2
    ('3':_) -> return 3
    ('4':_) -> return 4
    ('5':_) -> return 5
    ('6':_) -> return 6
    ('7':_) -> return 7
    ('8':_) -> return 8
    _ -> do
      putStrLn "Invalid input!"
      getNumber

onAbort :: EgisonError -> IO (Either EgisonError a)
onAbort e = do
  let x = show e
  return $ Left e

repl :: Env -> String -> IO ()
repl env prompt = do
  home <- getHomeDirectory
  sections <- selectSection tutorial
  case sections of
    Section _ contents -> liftIO $ runInputT (settings home) $ loop contents env
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = do
    setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_tutorial_history") }
  
  loop :: [Content] -> Env -> InputT IO ()
  loop [] env = do
    liftIO $ showFinishMessage
    liftIO $ repl env prompt
  loop contents@(content:rest) env = do
    liftIO $ putStrLn $ show content
    _ <- liftIO $ installHandler keyboardSignal (Catch (do {putStr "^C"; hFlush stdout})) Nothing
    input <- getEgisonExpr prompt
    tid <- liftIO $ myThreadId
    _ <- liftIO $ installHandler keyboardSignal (Catch (throwTo tid UserInterruption)) Nothing
    case input of
      Nothing -> return ()
      Just (Left topExpr) -> do
        result <- liftIO $ handle onAbort $ evalEgisonTopExpr env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop contents env
          Right env' -> loop contents env'
      Just (Right expr) -> do
        result <- liftIO $ handle onAbort $ evalEgisonExpr env expr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop contents env
          Right val -> do
            liftIO $ putStrLn $ show val
            loop contents env

data Tutorial = Tutorial [Section]

-- |title and contents
data Section = Section String [Content]

-- |explanation, examples, and exercises
data Content = Content String [String] [String]

instance Show Tutorial where
  show = showTutorial

instance Show Section where
  show = showSection

instance Show Content where
  show = showContent

showTutorial :: Tutorial -> String
showTutorial (Tutorial sections) =
  let n = length sections in
  intercalate "\n" $ map (\(n, section) -> show n ++ ": " ++ show section) $ zip [1..n] sections

showSection :: Section -> String
showSection (Section title _) = title

showContent :: Content -> String
showContent (Content msg examples exercises) =
  "====================\n" ++
  msg ++ "\n" ++
  (case examples of
     [] -> ""
     _ -> "examples:\n" ++ (intercalate "\n" (map (\example -> "  " ++ example) examples)) ++ "\n") ++
  (case exercises of
     [] -> ""
     _ -> "exercises:\n" ++ (intercalate "\n" (map (\exercise -> "  " ++ exercise) exercises)) ++ "\n") ++
  "===================="

tutorial :: Tutorial
tutorial = Tutorial 
 [Section "Calculate numbers"
   [ 
    Content "We can do arithmetic operations with '+', '-', '*', and '/'."
     ["(+ 1 2)", "(* 10 20)"]
     [],
    Content "We can write nested expressions."
     ["(+ (* 10 20) 2)", "(/ (* 10 20) (+ 10 20))"]
     ["Try to calculate '(100 - 1) * (100 + 1)'."],
    Content "We are supporting rational numbers."
     ["(+ 2/3 1/5)", "(/ 42 84)"]
     [],
    Content "We are supporting floats, too."
     ["(+ 10.2 1.3)", "(* 10.2 1.3)"]
     [],
    Content "you can convert a rational number to a float number with 'rtof'."
     ["(rtof 1/5)", "(rtof 1/100)"]
     [],
    Content "We can handle collections of numbers.\n We construct collections with '{}'."
     ["{}", "{10}", "{1 2 3 4 5}"]
     [],
    Content "With a 'take' function, we can extract a head part of the collection.\nWe can construct a collection with '{}'."
     ["(take 0 {1 2 3 4 5})", "(take 3 {1 2 3 4 5})"]
     [],
    Content "We can handle infinite lists.\nFor example, 'nats' is an infinite list that contains all natural numbers.\nGet a collection of natural numbers of any length you like."
     ["(take 100 nats)"]
     ["Get first 1000 numbers from nats."],
    Content "With a 'map' function, we can operate each element of the collection at onece."
     ["(take 100 (map (* $ 2) nats))", "(take 100 (map (modulo $ 3) nats))"]
     [],
    Content "We can create a \"partial\" function using '$' as an argument."
     ["((+ $ 10) 1)"]
     [],
    Content "With a 'foldl' function, we can gather together all elements of the collection using an operator you like."
     ["(foldl + 0 {1 2 3 4 5})", "(foldl * 1 {1 2 3 4 5})"]
     ["Try to get a sum of from 1 to 100?"],
    Content "Try to create a sequce of numbers '{1 1/2 1/3 1/4 ... 1/100}'."
     []
     [],
    Content "Try to calculate '1 + 1/2 + 1/3 + 1/4 + ... + 1/100'.\nPlease remember that you can convert a rational number to a float number with 'rtof'."
     ["(rtof 2/3)"]
     [],
    Content "Try to calculate '1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ... + (1/100)^2'."
     []
     []
    ]
 ]
