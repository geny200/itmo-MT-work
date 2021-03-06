module Lab2.Tree
  ( -- * Tree constructors
    Tree (..)
   
    -- * Function
  , evaluateTree
  )
where

import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Maybe (catMaybes)
import Lab2.Token (Token (..))

-- | Date type to describe the tree structure of the parser.
data Tree
  = Leaf Token      -- ^ A tree leaf with a token
  | Node [Tree]     -- ^ A tree node containing a list of subnodes
  deriving (Show, Eq)

-- | Creates a tree view of the result of parsing the input @List@ of @Token@.
-- (or an error message, if the token sequence wasn't valid).
evaluateTree ::
  [Token] ->        -- ^ List of tokens to parse
  Either String (Maybe Tree)
evaluateTree tokens = toRevers tokens (evalState (runExceptT startPrime) tokens)

---------------------------------------
--    Recursive descent parser       --
-- !This recursive parsing conforms  --
-- to the grammar specified in the   --
--        Lab2/README.md file        --
---------------------------------------

startPrime :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
startPrime =
  do
    result <- nodeE
    s <- lift get
    if null s
      then return result
      else throwError ("extra token", length s)

nodeE :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeE =
  do
    l <- nodeE'
    casePred isOr nodeO l

nodeO :: Maybe Tree -> ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeO left =
  do
    correctToken <- getToken (\x -> x == Or || x == Xor)
    tokenE <- nodeE'
    casePred isOr nodeO (joinNode [left, correctToken, tokenE])

nodeA :: Maybe Tree -> ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeA left =
  do
    correctToken <- getToken (== And)
    tokenN <- nodeN
    casePred isAnd nodeA (joinNode [left, correctToken, tokenN])

nodeE' :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeE' =
  do
    tokenN <- nodeN
    casePred isAnd nodeA tokenN

nodeN :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeN =
  do
    correctToken <- getToken (== Not)
    tokenN <- nodeN
    return $ joinNode [correctToken, tokenN]
    `catchError` const nodeT

nodeT :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeT =
  do
    correctTokenOne <- getToken (== BrOpen)
    tokenE <- nodeE
    correctTokenTwo <- getToken (== BrClose)
    return $ joinNode [correctTokenOne, tokenE, correctTokenTwo]
    `catchError` (\_ -> getToken (== Var))
    
--------------------
---    Utils     ---
--------------------

toRevers ::
  [Token] ->
  Either (String, Int) (Maybe Tree) ->
  Either String (Maybe Tree)
toRevers tokens (Left (str, num)) =
  Left (str ++ " at lextoken position " ++ show (length tokens - num))
toRevers _ (Right x) = Right x

joinNode :: [Maybe Tree] -> Maybe Tree
joinNode list = return $ Node (catMaybes list)

casePred ::
  ([Token] -> Bool) ->
  (Maybe Tree -> ExceptT (String, Int) (State [Token]) (Maybe Tree)) ->
  Maybe Tree ->
  ExceptT (String, Int) (State [Token]) (Maybe Tree)
casePred pr f left =
  do
    s <- lift get
    if pr s
      then f left
      else return left

isAnd :: [Token] -> Bool
isAnd (And : _) = True
isAnd _ = False

isOr :: [Token] -> Bool
isOr (Or : _) = True
isOr (Xor : _) = True
isOr _ = False

checkAndGetToken ::
  (Token -> Bool) ->
  [Token] ->
  ExceptT (String, Int) (State [Token]) (Maybe Tree)
checkAndGetToken _ [] = throwError ("unexpected token - empty", 0)
checkAndGetToken f (x : res)
  | f x = do
    lift (put res)
    return . Just . Leaf $ x
  | otherwise = throwError ("unexpected token - " ++ show x, length (x : res))

getToken ::
  (Token -> Bool) ->
  ExceptT (String, Int) (State [Token]) (Maybe Tree)
getToken predicate =
  do
    s <- lift get
    checkAndGetToken predicate s
