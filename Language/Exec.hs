module Exec (runHashProgram, Command, VarTable,
            ScriptState(ScriptState)) where

import Control.Monad
import qualified Data.Map as M
import Expressions

-- A model of a command which is waiting for arguments, a state to run,
-- and the input stream.
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState {
  output :: IO String
  , wd :: FilePath
  , vartable :: VarTable
  }


-- Represents dummy output.
dummyOut :: IO String
dummyOut = error "Dummy output."

-- Returns the value of a given Expr.
getValue :: VarTable -> Expr -> IO String
getValue vars (Var var) =
  case M.lookup var vars of
       Just val -> do return val
       Nothing -> do
         putStrLn $ "??? Warning: Variable '$" ++ var ++ "' not assigned!"
         return ""
getValue _ (Str str) = do return str

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command's execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr]
  -> IO ScriptState
runHashProgram cmdTable (Left wd) xs =
  runHashProgram cmdTable (Right $ ScriptState dummyOut wd M.empty) xs
runHashProgram cmdTable (Right state) (x:xs) = do
  ScriptState _ wd vars <- runTopLevel cmdTable state x
  runHashProgram cmdTable (Right (ScriptState dummyOut wd vars)) xs
runHashProgram _ (Right state) _ = do
  return state

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel cmdTable state (TLCnd cond) = runConditional cmdTable state cond
runTopLevel cmdTable state (TLFor for) = runFor cmdTable state for
runTopLevel cmdTable state (TLAssign assign) = runAssign cmdTable state assign
runTopLevel cmdTable state (TLPiped piped) = runPiped cmdTable state piped True

-- Calculates the result of piped commands execution.
runPiped :: CommandTable -> ScriptState -> Piped -> Bool -> IO ScriptState
runPiped cmdTable (ScriptState _ w v) (Piped cs inDir outDir append) print = do
  newState <- case inDir of
    Just dir -> do
      file <- getValue v dir
      runCommands cmdTable (state' (readFile file)) cs
    Nothing -> runCommands cmdTable (state' getLine) cs
  out <- output newState
  when (not (null out)) $ case outDir of
    Just dir -> do
      file <- getValue v dir
      if append then appendFile file (out ++ "\n")
                else writeFile file (out ++ "\n")
    Nothing -> when print $ putStrLn out
  return newState
  where state' :: IO String -> ScriptState
        state' out = ScriptState out w v
runPiped _ state Empty _ = return state

-- Executes piped commands.
runCommands :: CommandTable -> ScriptState -> [Cmd] -> IO ScriptState
runCommands cmdTable state@(ScriptState _ wd vars) ((Cmd name args):cs) = do
  cmdName <- getValue vars name
  cmdArgs <- mapM (getValue vars) args
  newState <- case M.lookup cmdName cmdTable of
    Just command -> command cmdArgs state
    Nothing -> do
      putStrLn $ "!!! Error: Invalid command '" ++ cmdName ++ "'!"
      return $ ScriptState (return "") wd vars
  runCommands cmdTable newState cs
runCommands _ state [] = return state


-- Calculates the result of Assign execution.
runAssign :: CommandTable -> ScriptState -> Assign -> IO ScriptState
runAssign _ (ScriptState _ wd vars) (Assign (Var var) (Left (Str val))) = do
  return $ ScriptState dummyOut wd (M.insert var val vars)
runAssign cmdTable st@(ScriptState _ wd vars) (Assign (Var var) (Right p)) = do
  newState <- runPiped cmdTable st p False
  val <- output newState
  return $ ScriptState dummyOut wd (M.insert var val vars)
runAssign ct st@(ScriptState _ wd vars) (Assign v@(Var var) (Left varIn)) = do
  val <- getValue vars varIn
  runAssign ct st (Assign v (Left (Str val)))
runAssign _ _ _ = error "Can only assign value to variable!"

-- Calculates the result of a for loop execution.
runFor :: CommandTable -> ScriptState -> For -> IO ScriptState
runFor cmdTable s@(ScriptState out wd v) (For (Var var) start end loop) = do
  if start <= end
     then do
          newState <- runHashProgram cmdTable (Right state') loop
          runFor cmdTable newState (For (Var var) (start+1) end loop)
     else return s
  where state' = ScriptState out wd (M.insert var (show start) v)

-- Calculates the result of a conditional execution.
runConditional :: CommandTable -> ScriptState -> Conditional -> IO ScriptState
runConditional cmdTable state@(ScriptState _ _ v) (If cond cthen) = do
  e <- eval v cond
  if e then runHashProgram cmdTable (Right state) cthen
       else return state
runConditional cmdTable state@(ScriptState _ _ v) (IfElse cond cthen celse) = do
  e <- eval v cond
  if e then runHashProgram cmdTable (Right state) cthen
       else runHashProgram cmdTable (Right state) celse

-- Evaluates the result of a given predicate.
eval :: VarTable -> Pred -> IO Bool
eval vars (Pred comp) = evalComp vars comp
eval vars (Not pred) = do
  e <- eval vars pred
  return $ not e
eval vars (And pred1 pred2) = do
  e1 <- eval vars pred1
  e2 <- eval vars pred2
  return $ e1 && e2
eval vars (Or pred1 pred2) = do
  e1 <- eval vars pred1
  e2 <- eval vars pred2
  return $ e1 || e2
eval vars (Parens pred) = eval vars pred

-- Evaluates the result of a given comparison.
evalComp :: VarTable -> Comp -> IO Bool
evalComp vars (CEQ e1 e2) = checkBool (==) vars e1 e2
evalComp vars (CNE e1 e2) = checkBool (/=) vars e1 e2
evalComp vars (CGE e1 e2) = checkBool (>=) vars e1 e2
evalComp vars (CGT e1 e2) = checkBool (>) vars e1 e2
evalComp vars (CLE e1 e2) = checkBool (<=) vars e1 e2
evalComp vars (CLT e1 e2) = checkBool (<) vars e1 e2
evalComp vars (CLI e) = do
  e' <- getValue vars e
  return $ not $ null e'

-- Compares two expressions with using given function.
checkBool :: (String -> String -> Bool) -> VarTable -> Expr -> Expr -> IO Bool
checkBool f vars x y = do
  x' <- getValue vars x
  y' <- getValue vars y
  return $ f x' y'

