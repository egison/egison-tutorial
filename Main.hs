module Main where

import Prelude hiding (catch)
import Control.Exception ( AsyncException(..), catch )
import Control.Monad.Error

import Data.Version
import Data.List

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
  putStrLn $ "** Info **"
  putStrLn $ "We can use a \'Tab\' key to complete keywords on the interpreter."
  putStrLn $ "If we type a \'Tab\' key after a closed parenthesis, the next closed parenthesis will be completed."

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
  n <- getNumber (length sections)
  return $ nth n sections

getNumber :: Int -> IO Int
getNumber n = do
  putStr   $ "(1-" ++ show n  ++ "): "
  hFlush stdout
  input <- getLine
  case input of
    ('1':_) -> return 1
    ('2':_) -> return 2
    ('3':_) -> return 3
    ('4':_) -> return 4
    ('5':_) -> return 5
    ('6':_) -> return 6
    ('7':_) -> return 7
    ('9':_) -> return 9
    _ -> do
      putStrLn "Invalid input!"
      getNumber n

repl :: Env -> String -> IO ()
repl env prompt = do
  section <- selectSection tutorial
  case section of
    Section _ cs -> loop env cs True
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }
    
  loop :: Env -> [Content] -> Bool -> IO ()
  loop env [] _ = do
    liftIO $ showFinishMessage
    liftIO $ repl env prompt
  loop env (content:contents) b = (do
    if b
      then liftIO $ putStrLn $ show content
      else return ()
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExprOrNewLine prompt
    case input of
      Left Nothing -> do
        b <- yesOrNo "Do you want to quit?"
        if b
          then return ()
          else do
            b <- yesOrNo "Do you want to procced next?"
            if b
              then loop env contents True
              else loop env (content:contents) False
      Left (Just "") -> do
        b <- yesOrNo "Do you want to procced next?"
        if b
          then loop env contents True
          else loop env (content:contents) False
      Right (Left (topExpr, _)) -> do
        result <- liftIO $ runEgisonTopExpr env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env (content:contents) False
          Right env' -> loop env' (content:contents) False
      Right (Right (expr, _)) -> do
        result <- liftIO $ runEgisonExpr env expr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env (content:contents) False
          Right val -> do
            liftIO $ putStrLn $ show val
            loop env (content:contents) False)
    `catch`
    (\e -> case e of
             UserInterrupt -> putStrLn "" >> loop env (content:contents) False
             StackOverflow -> putStrLn "Stack over flow!" >> loop env (content:contents) False
             HeapOverflow -> putStrLn "Heap over flow!" >> loop env (content:contents) False
             _ -> putStrLn "error!" >> loop env (content:contents) False
     )

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
     _ -> "\nExamples:\n" ++ (intercalate "\n" (map (\example -> "  " ++ example) examples)) ++ "\n") ++
  (case exercises of
     [] -> ""
     _ -> "\nExercises:\n" ++ (intercalate "\n" (map (\exercise -> "  " ++ exercise) exercises)) ++ "\n") ++
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
    Content "We can handle collections of numbers.\nWe construct collections with '{}'."
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
    Content "Try to calculate '1 + 1/2 + 1/3 + 1/4 + ... + 1/100'.\nRemember that you can convert a rational number to a float number with 'rtof'."
     ["(rtof 2/3)"]
     [],
    Content "Try to calculate '1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ... + (1/100)^2'."
     []
     []
    ],
  Section "Basics of functional programming"
   [
    Content "We can compare numbers using functions that return '#t' or '#f'.\n'#t' means the true.\n#f means the false.\nFunctions that return '#t' or '#f' are called \"predicates\"."
     ["(eq? 1 1)", "(gt? 1 1)", "(lt? 1 1)",  "(gte? 1 1)", "(lte? 1 1)"]
     [],
    Content "With a 'while' function, we can extract all head elements that satisfy the predicate.\n'primes' is a infinites list that contains all prime numbers."
     ["(while (lt? $ 100) primes)", "(while (lt? $ 1000) primes)"]
     [],
    Content "With a 'filter' function, we can extract all elements that satisfy the predicate.\n'We extract all prime numbers that are congruent to 1 modulo 4."
     ["(take 100 (filter (lambda [$p] (eq? (modulo p 4) 1)) primes))", "(take 200 (filter (lambda [$p] (eq? (modulo p 4) 1)) primes))"]
     [],
    Content "We use 'lambda' expressions to create functions.\nHere are simple 'lambda' examples."
     ["((lambda [$x] (+ x 1)) 10)", "((lambda [$x] (* x x)) 10)", "((lambda [$x $y] (* x y)) 10 20)"]
     [],
    Content "With a 'map2' function, we can combine two lists as follow."
     ["(take 100 (map2 * nats nats))", "(take 100 (map2 (lambda [$n $p] [n p]) nats primes))"]
     [],
    Content "We combine numbers using '[]'.\nThese things are called 'tuples'."
     ["[1 2]", "[1 2 3]"]
     [],
    Content "Note that a tuple that consists of only one elment is equal with that element itself."
     ["[1]", "[[[1]]]"]
     [],
    Content "Try to create a sequce of tuples '{[1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8] [1 9]}'."
     []
     [],
    Content "Try to create a collections of sequce of tuples as follow.\n{{[1 1] [1 2] ... [1 9]}\n {[2 1] [2 2] ... [2 9]}\n ...\n {[9 1] [9 2] ... [9 9]}}"
     []
     [],
    Content "Try to create the multiplication table.\n{{[[1 1 1] [1 2 2] ... [1 9 9]}\n {[2 1 2] [2 2 4] ... [2 9 18]}\n ...\n {[9 1 9] [9 2 18] ... [9 9 81]}}}"
     []
     []
    ],
  Section "Define your own functions"
   [
    Content "We can bind a value to a variable with a 'define' expression.\nWe can easily get the value we bound to the variable."
     ["(define $x 10)", "x"]
     [],
    Content "We can define a function. Let's define a function and test it."
     ["(define $f (lambda [$x] (+ x 1)))", "(f 10)", "(define $g (lambda [$x $y] (* x y)))", "(g 10 20)"]
     [],
    Content "We can write a recursive definition. Let's try that."
     ["(define $odds {1 @(map (+ $ 2) odds)})", "(take 10 odds)"]
     [],
    Content "Try to define 'evens' referring to 'odds' example above."
     []
     [],
    Content "We can define local variables with a 'let' expression."
     ["(let {[$x 10] [$y 20]} (+ x y))"]
     [],
    Content "Let's try 'if' expressions."
     ["(if #t 1 2)", "(let {[$x 10]} (if (eq? x 10) 1 2))"]
     [],
    Content "Using 'define' and 'if', we can write recursive functions as follow."
     ["(define $your-take (lambda [$n $xs] (if (eq? n 0) {} {(car xs) @(your-take (- n 1) (cdr xs))})))", "(your-take 10 nats)"]
     [],
    Content "Try to write a 'your-map' function.\nWe may need 'empty?' function inside 'your-map' function."
     ["(empty? {})"]
     [],
    Content "We can view all library functions on collections at \"http://www.egison.org/libraries/core/collection.html\"."
     []
     []
    ],
  Section "Basic of pattern-matching"
   [
    Content "We can do pattern-matching against multisets."
     ["(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])"]
     [],
    Content "We can do non-linear pattern-matching.\nTry the following expression with various targets."
     ["(match-all {1 2 1 3} (multiset integer) [<cons $x <cons ,x _>> x])"]
     [],
    Content "We can change the way of pattern-matching by changing \"matcher\".\nTry the following expressions."
     ["(match-all {1 2 3} (list integer) [<cons $x $xs> [x xs]])", "(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])", "(match-all {1 2 3} (set integer) [<cons $x $xs> [x xs]])"]
     [],
    Content "We can do pattern-matching against a collection of collections as follow."
     ["(match-all {{1 2 3 4 5} {4 5 1} {6 1 7 4}} (list (multiset integer)) [<cons <cons $n _> <cons <cons ,n _> <cons <cons ,n _> _>>> n])"]
     [],
    Content "A pattern that has '^' ahead of which is called a not-pattern.\nA not-pattern matches when the target does not match against the pattern."
     ["(match-all {1 2 1 3} (multiset integer) [<cons $x ^<cons ,x _>> x])"]
     [],
    Content "An and-pattern matches when the all patterns matches the target.\nIt can be used like an as-pattern."
     ["(match-all {1 2 1 3} (multiset integer) [<cons $x (& ^<cons ,x _> $xs)> [x xs]])"]
     [],
    Content "An or-pattern matches when one of the patterns matches the target."
     ["(match-all {1 2 1 3} (multiset integer) [<cons $x (| <cons ,x _> ^<cons ,x _>)> x])"]
     [],
    Content "'list' has a special pattern-constructor 'join'.\n'join' divides a collection into two collections.\nTry the following expressions."
     ["(match-all {1 2 3 4 5} (list integer) [<join $xs $ys> [xs ys]])"]
     [],
    Content "We can enumerate two combination of numbers as follow."
     ["(match-all {1 2 3 4 5} (list integer) [<join _ <cons $x <join _ <cons $y _>>>> [x y]])"]
     ["Try to enumerate three combination of numbers."],
    Content "Did we think how about \"n\" comination of the elements of the collection?\nWe already have a solution.\nWe can write a pattern that include '...' as the following demonstrations."
     ["(match-all {1 2 3 4 5} (list integer) [(loop $i [1 4] <join _ <cons $a_i ...>> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 5] <join _ <cons $a_i ...>> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 $n] <join _ <cons $a_i ...>> _) [n a]])"]
     [],
    Content "We can view a lot of demonstration of pattern-matching at \"http://www.egison.org/demonstrations/\"."
     []
     []
    ],
  Section "Pattern-matching against infinite collections"
   [
    Content "We can write a pattern-matching against infinite lists even if that has infinite results.\nNote that Egison really enumurate all pairs of two natural numbers in the following example."
     ["(take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))"]
     [],
    Content "We can enumerate all two combinations of natural numbers as follow."
     ["(define $two-combs (match-all nats (list integer) [<join _ (& <cons $x _> <join _ <cons $y _>>)> [x y]]))", "(take 100 two-combs)"]
     [],
    Content "We can enumerate all pythagoras numbers as follow."
     ["(define $pyths (map (lambda [$x $y] (+ (* x x) (* y y))) two-combs))", "(take 100 pyths)"]
     [],
    Content "We have an infinite list of prime numers in 'primes'.\nCheck it with a 'take' function."
     ["(take 10 primes)"]
     ["What is the 100th prime number?"],
    Content "We can get twin primes or triplet primes using pattern-matching as follow."
     ["(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 2) _>>> [n (+ n 2)]]))", "(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 2) <cons ,(+ n 6) _>>>> [n (+ n 2) (+ n 6)]]))", "(take 10 (match-all primes (list integer) [<join _ <cons $n <cons ,(+ n 4) <cons ,(+ n 6) _>>>> [n (+ n 2) (+ n 6)]]))"]
     ["What are the 100th twin primes?"],
    Content "We prepared the 'p-f' function that prime-factorize a number.\nWe can play freely with numbers a lot of time."
     ["(take 100 (map p-f nats))"]
     ["Are there three successive natural numbers all of whose prime-factorization contain three primes? For example, '27=3*3*3' and '28=2*2*7' but '29=29', so the sequence '27', '28' and '29' is not that."]
    ],
  Section "Writing scripts in Egison"
   [
    Content "Let's write a famous Hello world program in Egison.\nTry the following expression.\nIt is evaluated to the 'io-function'.\nTo execute an io-function, we use 'io' primitive as follow."
     ["(io (print \"Hello, world!\"))"]
     [],
    Content "We can execute multiple io-functions in sequence as follow.\nThe io-functions is executed from the head."
     ["(io (do {[(print \"a\")] [(print \"b\")] [(print \"c\")]} []))", "(io (do {[(write-string \"Type your name: \")] [(flush)] [$name (read-line)] [(print {@\"Hello, \" @name @\"!\"})]} []))"]
     [],
    Content "The following is a hello world program in Egison.\nTry to create a file with the following content and save it as \"hello.egi\", and execute it in the terminal as '% egison hello.egi'\n"
     ["(define $main (lambda [$args] (print \"Hello, world!\")))"]
     []
    ]
  ]
