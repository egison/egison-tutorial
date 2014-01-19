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
  putStrLn $ "You have finished this section."
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

selectSection :: Tutorial -> IO [Content]
selectSection tutorial  = selectSectionHelper [] tutorial

selectSectionHelper :: [(Int, String)] -> Tutorial -> IO [Content]
selectSectionHelper hs (Sections secs)  = do
  putStrLn "===================="
  putStrLn "Select a section to learn."
  foldM (\x sec -> do
          putStr $ "" ++ show x ++ ": "
          putStrLn (fst sec)
          return (x + 1))
        1 secs
  putStrLn "===================="
  let m = length secs
  n <- readNumber m
  let (title, t) = head $ drop (n - 1) secs
  case t of
    Contents contents -> return contents
    Sections _ -> selectSectionHelper (hs ++ [(n, title)]) t

readNumber :: Int -> IO Int
readNumber m = do
  putStr $ "(1-" ++ show m ++ "): "
  hFlush stdout
  input <- getLine
--  let n = (read input :: Int)
  case input of
    ('1':_) -> return 1
    ('2':_) -> return 2
    ('3':_) -> return 3
    ('4':_) -> return 4
    ('5':_) -> return 5
    _ -> do
      putStrLn "Invalid input!"
      readNumber m


printTutorial :: Content -> IO ()
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
    
    loop :: Env -> String -> String -> [Content] -> Bool -> InputT IO ()
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
  | Contents [Content]

type Content = (String, [String]) 

tutorial :: Tutorial
tutorial =
  Sections [
    ("Lv1 - Buildin data",
      Contents [
        ("There are two boolean value '#t' and '#f'.", ["#t", "#f"]),
        ("You can do boolean operations with 'and', 'or', 'not'.", ["(and #t #f)", "(or #t #f)", "(not #t)"]),
        ("You can do arithmetic operations with `+', '-', '*'.", ["(+ 1 2)", "(* 10 20)"]),
        ("We have predicates for numbers.", ["(eq-i? 1 1)", "(gt-i? 1 1)", "(lt-i? 1 1)",  "(gte-i? 1 1)", "(lte-i? 1 1)"]),
        ("We are supporting rational numbers.", ["(+ 2/3 1/5)", "(/ 42 84)"]),
        ("We are supporting floats, too.", ["(+ 10.2 1.3)", "(rtof 1/5)"]),
        ("We have predicates for floats.", ["(eq-f? 1.0 1.0)", "(gt-f? 1.0 1.0)", "(lt-i? 1.0 1.0)",  "(gte-i? 1.0 1.0)", "(lte-i? 1.0 1.0)"]),
        ("You can construct a tuple with '[]'.", ["[1 2]", "[1 2 3]"]),
        ("A tuple which consists of only one elment is equal with that element itself.", ["[1]", "[[[1]]]"]),
        ("You can construct a collection with '{}'.", ["{1}", "{1 2 3}"]),
        ("The collection after '@' in a collection is called a subcollection.", ["{1 @{2 3}}", "{1 @{2 3} @{4 @{5}} 6}"]),
        ("We can define an array as follow. We can access the element of the array using '_'.", ["(define $a [| 11 22 33 |])", "a_2"]),
        ("We can define an hash as follow. We can access the element of the hash using '_' as arrays.", ["(define $h {| [1 11] [2 22] [3 33] |})", "h_2"])
        ]),
    ("Lv2 - Functional programming",
     Contents [
       ("You can bind a value to a variable with a 'define' expression.\nYou can easily get the value you binded to the variable.", ["(define $x 10)", "x"]),
       ("You can define a function. Let's define a function and test it.", ["(define $f (lambda [$x] (+ x 1)))", "(f 10)", "(define $g (lambda [$x $y] (* x y)))", "(g 10 20)"]),
       ("You can define local variables with a 'let' expression.", ["(let {[$x 10] [$y 20]} (+ x y))"]),
       ("Let's try 'if' expressions.", ["(if #t 1 2)", "(let {[$x 10]} (if (eq? x 10) 1 2))"])
       ]),
    ("Lv3 - Pattern-matching",
     Contents [
       ("You can do pattern-matching against multisets.", ["(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])"]),
       ("You can do non-linear pattern-matching. Try the following expression with various targets.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x <cons ,x _>> x])"]),
       ("You can change the way of pattern-matching by changing \"matcher\".\nTry the following expressions.", ["(match-all {1 2 3} (list integer) [<cons $x $xs> [x xs]])", "(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])", "(match-all {1 2 3} (set integer) [<cons $x $xs> [x xs]])"]),
       ("'list' has a special pattern-constructor 'join'.\n'join' divides a collection into two collections.\nTry the following expressions.", ["(match-all {1 2 3 4 5} (list integer) [<join $xs $ys> [xs ys]])", "(match-all {1 2 3 4 5} (list integer) [<join _ <cons $x <join _ <cons $y _>>>> [x y]])"]),
       ("We can do pattern-matching against a collection of collections as follow.", ["(match-all {{1 2 3 4 5} {4 5 1} {6 1 7 4}} (list (multiset integer)) [<cons <cons $n _> <cons <cons ,n _> <cons <cons ,n _> <nil>>>> n])"]),
       ("A pattern that has '^' ahead of which is called a not-pattern.\nA not-pattern matches when the target does not match against the pattern.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x ^<cons ,x _>> x])"]),
       ("An and-pattern matches when the all patterns matches the target.\nIt can be used like an as-pattern.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x (& ^<cons ,x _> $xs)> [x xs]])"]),
       ("An or-pattern matches when one of the patterns matches the target.", ["(match-all {1 2 1 3} (multiset integer) [<cons $x (| <cons ,x _> ^<cons ,x _>)> x])"])
       ]),
    ("Lv4 - Infinite collections (Play with prime numbers)",
     Contents [
       ("First load a library for prime numers.", ["(load \"lib/math/prime.egi\")"]),
       ("Get elements from the sequence of prime numebers using 'take' function.", ["(take 10 primes)"]),
       ("We can get twin primes or triplet primes using pattern-matching as follow.", ["(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 2) _>>> [n (+ n 2)]]))", "(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 2) <cons ,(+ n 6) _>>>> [n (+ n 2) (+ n 6)]]))", "(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 4) <cons ,(+ n 6) _>>>> [n (+ n 2) (+ n 6)]]))"]),
       ("With predicate-patterns and and-patterns, we can do interesting things as follow.", ["(take 1 (match-all primes (list integer) [<join _ <cons $n <cons (& ?(gt-i? $ (+ 10 n)) $m) _>>> [n m]]))", "(take 1 (match-all primes (list integer) [<join _ <cons $n <cons (& ?(gt-i? $ (+ 20 n)) $m) _>>> [n m]]))"]),
       ("Play freely with the sequence of prime numbers.", [])
       ]),
    ("Lv5 - Loop-patterns",
     Contents [
       ("We can write a pattern that include '...'. The following are demonstrations.", ["(match-all {1 2 3 4 5} (list integer) [(loop $i [1 ,2] <cons $a_i ...> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 ,3] <cons $a_i ...> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 ,2] <join _ <cons $a_i ...>> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 ,3] <join _ <cons $a_i ...>> _) a])"])
       ])
--    ("Lv6 (preparing) - Matcher definition (Play with graphs)",
--     Contents [
--       ("Sorry, we are creating this section now.", [])
--       ])
  ]

