module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.Version
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
import System.Console.Haskeline
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import Language.Egison
import qualified Paths_egison_tutorial as P

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optPrompt = prompt} -> do
                env <- primitiveEnv >>= loadLibraries
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
  putStrLn $ "Egison Tutorial for Version " ++ showVersion P.version ++ " (C) 2013 Satoshi Egi"
  putStrLn $ "http://www.egison.org"
  putStrLn $ "Welcome to Egison Tutorial!"

showFinishMessage :: IO ()
showFinishMessage = do
  putStrLn $ "You have finished this chapter."
  putStrLn $ "Thank you!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Tutorial.\nByebye."

askUser :: String -> IO Bool
askUser question = do
  putStr $ question
  putStr $ " (Y/n): "
  hFlush stdout
  input <- getLine
  case input of
   [] -> return True
   ('y':_) -> return True
   ('Y':_) -> return True
   ('n':_) -> return False
   _ -> askUser question

selectSection :: Tutorial -> IO [(String, [String])]
selectSection (Sections secs)  = do
  putStrLn "Select a section to learn."
  foldM (\x sec -> do
          putStr $ "  " ++ show x ++ ": "
          putStrLn (fst sec)
          return (x + 1))
        1 secs
  let m = length secs
  putStr $ "(1-" ++ show m ++ "): "
  hFlush stdout
  input <- getLine
  let n = (read input :: Int)
  let sec = head $ drop (n - 1) secs
  case snd sec of
    Contents contents -> return contents
    _ -> undefined

printTutorial :: (String, [String]) -> IO ()
printTutorial (msg, examples) = do
  putStrLn "===================="
  putStrLn msg
  case examples of
    [] -> return ()
    _ -> do
      putStrLn "e.g."
      mapM_ (\example -> do
                putStr "  "
                putStrLn example)
        examples
  putStrLn "===================="

repl :: Env -> String -> IO ()
repl env prompt = do
  home <- getHomeDirectory
  contents <- selectSection tutorial
  liftIO (runInputT (settings home) $ loop env prompt "" contents True)
  where
    settings :: MonadIO m => FilePath -> Settings m
    settings home = defaultSettings { historyFile = Just (home </> ".egison_tutorial_history") }
    
    loop :: Env -> String -> String -> [(String, [String])] -> Bool -> InputT IO ()
    loop env prompt' _ [] _ = do
      liftIO $ showFinishMessage
      contents <- liftIO $ selectSection tutorial
      loop env prompt' "" contents True
    loop env prompt' rest ts@(t:rs) True = do
      liftIO $ printTutorial t
      loop env prompt' rest ts False
    loop env prompt' rest ts@(t:rs) False = do
      input <- getInputLine prompt'
      case input of
        Nothing -> do
          response1 <- liftIO $ askUser "Do you want to proceed next?"
          case response1 of
            True -> loop env prompt' rest rs True
            False -> do
              response2 <- liftIO $ askUser "Do you want to quit egison-tutorial?"
              case response2 of
                True -> do
                  liftIO $ showByebyeMessage
                  return ()
                False -> loop env prompt' rest ts False
        Just "quit" -> do
          liftIO $ showByebyeMessage
          return () 
        Just "" ->
          case rest of
            "" -> do
              response1 <- liftIO $ askUser "Do you want to proceed next?"
              case response1 of
                True -> loop env prompt' rest rs True
                False -> loop env prompt' rest ts False
            _ -> loop env (take (length prompt) (repeat ' ')) rest ts False
        Just input' -> do
          let newInput = rest ++ input'
          result <- liftIO $ runEgisonTopExpr env newInput
          case result of
            Left err | show err =~ "unexpected end of input" -> do
              loop env (take (length prompt) (repeat ' ')) (newInput ++ "\n") ts False
            Left err | show err =~ "expecting (top-level|\"define\")" -> do
              result <- liftIO $ fromEgisonM (readExpr newInput) >>= either (return . Left) (evalEgisonExpr env)
              case result of
                Left err | show err =~ "unexpected end of input" -> do
                  loop env (take (length prompt) (repeat ' ')) (newInput ++ "\n") ts False
                Left err -> do
                  liftIO $ putStrLn $ show err
                  loop env prompt "" ts False
                Right val -> do
                  liftIO $ putStrLn $ show val
                  loop env prompt "" ts False
            Left err -> do
              liftIO $ putStrLn $ show err
              loop env prompt "" ts False
            Right env' ->
              loop env' prompt "" ts False

data Tutorial =
    Sections [(String, Tutorial)]
 |  Contents [(String, [String])]

tutorial :: Tutorial
tutorial =
  Sections [
    ("Buildin Data",
     Contents [
       ("You can do arithmetic operations with `+`, `-`, `*`, `div`.", ["(+ 1 2)", "(* 10 20)"]),
       ("You can bind a value to a variable with a `define` expression.\nYou can easily get the value you binded to the variable.", ["(define $x 10)", "x"]),
       ("You can do boolean operations with `and`, `or`, `not`.", ["(and #t #f)", "(or #t #f)", "(not #t)"]),
       ("You can construct a tuple with `[]`.", ["[1 2]", "[1 2 3]"]),
       ("A tuple which consists of only one elment is equal with that element itself.", ["[1]", "[[[1]]]"]),
       ("You can construct a collection with `{}`.", ["{1}", "{1 2 3}"]),
       ("The collection after `@` in a collection is called a subcollection.", ["{1 @{2 3}}", "{1 @{2 3} @{4 @{5}} 6}"])
       ]),
    ("Functions",
     Contents [
       ("You can define a function. Let's define a function and test it.", ["(define $f (lambda [$x] (+ x 1)))", "(f 10)"]),
       ("You can define local variables with a `let` expression.", ["(let {[$x 10] [$y 20]} (+ x y))"]),
       ("Let's try `if` expressions.", ["(if #t 1 2)", "(let {[$x 10]} (if (eq? x 10) 1 2))"]),
       ("Now, you can define a factorial function that gets a natural number 'n' and returns 'n * n-1 * n-2 * ... * 1'. Let's try!", [])
       ]),
    ("Pattern-Matching",
     Contents [
       ("You can do pattern-matching against multisets.", ["(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])"]),
       ("You can do non-linear pattern-matching. Try the following expression with various targets.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x <cons ,x _>> x])"]),
       ("A pattern that has `^' ahead of which is called a not-pattern.\nA not-pattern matches when the target does not match against the pattern.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x ^<cons ,x _>> x])"]),
       ("You can change the way of pattern-matching by changing \"matcher\".\nTry following expressions.", ["(match-all {1 2 3} (list integer) [<cons $x $xs> [x xs]])", "(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])"])
       ])
  ]

