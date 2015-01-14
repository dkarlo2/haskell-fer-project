-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash (runScript, runInteractive) where

import Commands
import Control.Exception
import Exec
import HashParser
import System.Console.Haskeline
import System.Directory
import System.Exit

-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = do
  wd <- getCurrentDirectory
  script <- readFile fp
  case parseProgram (removeComments script ++ ";") of
       Left err -> do
         putStrLn $ "!!! Parse error: " ++ err
       Right xs -> do
         runHashProgram commands (Left wd) xs
         return ()

-- Communicates with the user and performs hash commands line by line.
runInteractive :: IO ()
runInteractive = do
  wd <- getCurrentDirectory
  runInteractive' (Left wd)
    where runInteractive' :: Either FilePath ScriptState -> IO ()
          runInteractive' fpss = do
            line <- case fpss of
                         Left wd -> getNextLine wd
                         Right (ScriptState _ wd _) -> getNextLine wd
            case parseProgram line of
              Left err -> do
                putStrLn $ "!!! Parse error: " ++ err
                runInteractive' fpss
              Right xs -> do
                tmp <- try (runHashProgram commands fpss xs)
                  :: IO (Either (ExitCode) ScriptState)
                case tmp of
                     Left ExitSuccess -> return ()
                     Left err -> throw err
                     Right state -> runInteractive' (Right state)

-- Gets next line from user input.
getNextLine :: FilePath -> IO String
getNextLine wd = do
  line <- runInputT defaultSettings $ getInputLine $ wd ++ "% "
  case line of
       Just l -> return $ (removeComment l) ++ ";"
       Nothing -> error "Error on user input"

