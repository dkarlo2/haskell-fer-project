-- Contains the data structures describing the structure of the language itself

module Expressions where

-- A command which takes arguments and performs something.
data Cmd = Cmd {
    name :: Expr -- The command name (can be a variable)
    , args :: [Expr] -- The command arguments
  }
  deriving Show

-- An assignment.
data Assign = Assign {
    var :: Expr -- Assignment target
    , val :: Either Expr Piped -- A value or piped commands output
                               -- to assign to a variable
  }
  deriving Show

-- A bottom-level expression
data Expr
  = Var String -- A named variable
  | Str String -- A mere string, the peasant of expressions
  deriving (Eq, Show)

-- A comparison operation
data Comp
  = CEQ Expr Expr -- ==
  | CNE Expr Expr -- /=
  | CGE Expr Expr -- >=
  | CGT Expr Expr -- >
  | CLE Expr Expr -- <=
  | CLT Expr Expr -- <
  | CLI Expr -- A wrapped expression literal - True if nonempty
  deriving (Eq, Show)

-- Something that evaluates to a truth value
data Pred
  = Pred Comp -- A wrapped comparison
  | Not Pred -- Negation
  | And Pred Pred -- A binary logical and
  | Or Pred Pred -- A binary logical or
  | Parens Pred -- An expression in parentheses
  deriving (Eq, Show)

-- A conditional branching expression - if-then or if-then-else
  -- If-then with a condition and a list of actions
data Conditional = If {
    cond :: Pred -- Predicate to satisfy
    , cthen :: [TLExpr] -- Actions if satisfied
  }
  -- An if-then-else with a condition and two possible paths
  | IfElse {
    cond :: Pred -- Predicate to satisfy
    , cthen :: [TLExpr] -- Actions if satisfied
    , celse :: [TLExpr] -- Actions otherwise
  }
  deriving Show

-- A for loop.
data For = For {
    varL :: Expr -- Loop variable.
    , start :: Int -- Start value.
    , end :: Int -- End value.
    , loop :: [TLExpr] -- Top-level expressions to loop over.
  }
  deriving Show

-- Expression representing piped commands.
data Piped = Piped {
    commands :: [Cmd] -- List of piped commands.
    , inDir :: Maybe Expr -- A redirected input fp
    , outDir :: Maybe Expr -- A redirected output fp
    , append :: Bool -- If redirected, is it appending?
  }
  | Empty
  deriving Show

-- A top-level expression, wrapping either a conditional expression, a for loop,
-- piped top-level expressions, or an assignment.
data TLExpr
  = TLCnd Conditional
  | TLPiped Piped
  | TLFor For
  | TLAssign Assign
  deriving Show

