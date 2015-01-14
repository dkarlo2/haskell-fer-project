module Commands (commands) where

import Control.Monad
import Data.Char
import qualified Data.Map as M
import Data.List
import Exec
import Numeric
import System.Directory
import System.Exit
import System.Path (copyDir)

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = foldr (\(x,y) -> M.insert x y) M.empty [("echo", cmdEcho),
  ("mv", cmdCpMv True), ("cp", cmdCpMv False), ("rm", cmdRm),
  ("create", cmdCreate), ("mvdir", cmdCpMvDir True),
  ("cpdir", cmdCpMvDir False), ("rndir", cmdRnDir), ("mkdir", cmdMkDir),
  ("rmdir", cmdRmDir), ("ls", cmdLs), ("pwd", cmdPwd), ("cd", cmdCd),
  ("cat", cmdCat), ("hexdump", cmdHexDump), ("exit", cmdExit)]

-- Constructs new ScriptState.
ss :: String -> FilePath -> VarTable -> ScriptState
ss out wd vars = ScriptState (return out :: IO String) wd vars


-- Executes "echo" command.
cmdEcho :: Command
cmdEcho xs (ScriptState _  wd vars) = do
  return $ ss (concat xs) wd vars

-- Executes "cp" or "mv" command.
cmdCpMv :: Bool -> Command
cmdCpMv mv xs (ScriptState _ wd vars) =
  if length xs < 2
     then do
       putStrLn "At least two arguments expected."
       return $ ss "" wd vars
     else do
      isDir <- doesDirectoryExist target
      if null target || isDir then cmdCpMv1 mv (init xs) (toDir target) wd vars
                              else cmdCpMv2 mv (init xs) target wd vars
  where target = last xs
        toDir :: FilePath -> FilePath
        toDir p = if null p || last p == '/' then p
                                             else p ++ "/"

-- Command '{cp/mv} src ... directory'.
cmdCpMv1 :: Bool -> [FilePath] -> FilePath -> FilePath -> VarTable ->
            IO ScriptState
cmdCpMv1 mv files dir wd vars = do
  moveAll files dir
  putStrLn $ messageCpMv mv (length files)
  return $ ss "" wd vars
  where moveAll :: [FilePath] -> FilePath -> IO ()
        moveAll (x:xs) dir = do
          copyFile x (dir ++ name x)
          when mv $ removeFile x
          moveAll xs dir
        moveAll [] _ = return ()

-- Returns the name of the file with a given path.
name :: FilePath -> String
name f = reverse $ takeWhile (/='/') $ reverse f

-- Command '{cp/mv} src target'.
cmdCpMv2 :: Bool -> [FilePath] -> FilePath -> FilePath -> VarTable ->
          IO ScriptState
cmdCpMv2 mv [src] dst wd vars = do
  copyFile src dst
  when mv $ removeFile src
  putStrLn $ messageCpMv mv 1
  return $ ss "" wd vars
cmdMv2 _ _ wd vars = do
  putStrLn "One source and one target file expected."
  return $ ss "" wd vars

-- Generates mv/cp output message.
messageCpMv :: Bool -> Int -> String
messageCpMv mv howMany =
  (if howMany > 1 then "Files " else "File ") ++
    (if mv then "moved " else "copied ") ++ "successfully."

-- Executes "rm" command.
cmdRm :: Command
cmdRm xs (ScriptState _ wd vars) = do
  removeAll xs
  putStrLn "File(s) successfully removed."
  return $ ss "" wd vars
    where removeAll :: [FilePath] -> IO ()
          removeAll (x:xs) = do
            removeFile x
            removeAll xs
          removeAll [] = return ()

-- Executes "create" command.
cmdCreate :: Command
cmdCreate xs (ScriptState _ wd vars) = do
  createAll xs
  putStrLn "File(s) successfully created."
  return $ ss "" wd vars
    where createAll :: [FilePath] -> IO ()
          createAll (x:xs) = do
            exists <- doesFileExist x
            when (not exists) $ writeFile x ""
            createAll xs
          createAll [] = return ()

-- Command '{cpdir/mvdir} dir ... directory'.
cmdCpMvDir :: Bool -> Command
cmdCpMvDir mv xs (ScriptState _ wd vars) = do
  moveAll dirs dir
  putStrLn $ messageCpMvDir mv (length dirs)
  return $ ss "" wd vars
  where dirs = init xs
        dir' = last xs
        dir = dir' ++ if null dir' || last dir' == '/' then ""
                                                       else "/"
        moveAll :: [FilePath] -> FilePath -> IO ()
        moveAll (x:xs) dir = do
          copyDir x (dir ++ name x)
          when mv $ removeDirectoryRecursive x
          moveAll xs dir
        moveAll [] _ = return ()

-- Generates mvdir/cpdir output message.
messageCpMvDir :: Bool -> Int -> String
messageCpMvDir mv howMany =
  (if howMany > 1 then "Directories " else "Directory ") ++
    (if mv then "moved " else "copied ") ++ "successfully."

-- Executes "rndir" command.
cmdRnDir :: Command
cmdRnDir [x,y] (ScriptState _ wd vars) = do
  copyDir x y
  removeDirectoryRecursive x
  putStrLn "Directory renamed successfully.\n"
  return $ ss "" wd vars
cmdRnDir _ (ScriptState _ wd vars) = do
  putStrLn "Two arguments expected."
  return $ ss "" wd vars

-- Executes "mkdir" command.
cmdMkDir :: Command
cmdMkDir xs (ScriptState _ wd vars) = do
  makeAll xs
  putStrLn "Directories created successfully."
  return $ ss "" wd vars
    where makeAll :: [FilePath] -> IO ()
          makeAll (x:xs) = do
            createDirectory x
            makeAll xs
          makeAll [] = return ()

-- Executes "rmdir" command.
cmdRmDir :: Command
cmdRmDir xs (ScriptState _ wd vars) = do
  removeAll xs
  putStrLn "Directories removed successfully."
  return $ ss "" wd vars
    where removeAll :: [FilePath] -> IO ()
          removeAll (x:xs) = do
            removeDirectoryRecursive x
            removeAll xs
          removeAll [] = return ()

-- Executes "ls" command.
cmdLs :: Command
cmdLs [] (ScriptState _ wd vars) = do
  contents <- getDirectoryContents wd
  return $ ss (unlines (sort contents)) wd vars
cmdLs [x] (ScriptState _ wd vars) = do
  contents <- getDirectoryContents x
  return $ ss (unlines (sort contents)) wd vars
cmdLs _ (ScriptState _ wd vars) = do
  putStrLn "Invalid number of arguments."
  return $ ss "" wd vars

-- Executes "pwd" command
cmdPwd :: Command
cmdPwd [] (ScriptState _ wd vars) = return $ ss wd wd vars
cmdPwd _ (ScriptState _ wd vars) = do
  putStrLn "No arguments expected."
  return $ ss "" wd vars

-- Executes "cd" command
cmdCd :: Command
cmdCd [] (ScriptState _ _ vars) = do
  home <- getHomeDirectory
  setCurrentDirectory home
  return $ ss "" home vars
cmdCd [dir] (ScriptState _ _ vars) = do
  setCurrentDirectory dir
  absDir <- getCurrentDirectory
  return $ ss "" absDir vars
cmdCd _ (ScriptState _ wd vars) = do
  putStrLn "Invalid number of arguments."
  return $ ss "" wd vars

-- Executes "cat" command
cmdCat :: Command
cmdCat [] (ScriptState inStream wd vars) = do
  files <- inStream
  contents <- catAll $ lines files
  return $ ss contents wd vars
cmdCat xs (ScriptState _ wd vars) = do
  contents <- catAll xs
  return $ ss contents wd vars

-- Helper function for cmdCat.
catAll :: [FilePath] -> IO String
catAll (x:xs) = do
  s <- readFile x
  ss <- catAll xs
  return $ "File " ++ (name x) ++ "\n" ++ s ++ "\n" ++ ss
catAll _ = return ""

-- Executes "hexdump" command.
cmdHexDump :: Command
cmdHexDump [] (ScriptState contents' wd vars) = do
  contents <- contents'
  return $ ss (hexDump contents "") wd vars
cmdHexDump _ (ScriptState _ wd vars) = do
  putStrLn "No arguments expected."
  return $ ss "" wd vars

-- Generates hexdump of a string.
hexDump :: String -> String -> String
hexDump (x:xs) ys = hexDump xs (ys ++ toHex x)
hexDump [] ys = ys

-- Represents char as two digit hexadecimal number.
toHex :: Char -> String
toHex c = (if length h == 1 then "0" else "") ++ h
  where h = showHex (ord c) ""

-- Executes "exit" command.
cmdExit :: Command
cmdExit _ _ = do
  exitSuccess

