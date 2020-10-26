module Tree
  ( 
    Tree (..)
  , nodeE
  , evaluateTree
  )
where

import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Maybe (catMaybes)
import Token (Token (..))

toRevers :: [Token] -> Either (String, Int) (Maybe Tree) -> Either (String, Int) (Maybe Tree)
toRevers tokens (Left (str, num)) = Left (str, length tokens - num)
toRevers _ x = x

evaluateTree :: [Token] -> Either (String, Int) (Maybe Tree)
evaluateTree tokens = toRevers tokens (evalState (runExceptT nodeE) tokens)

data Tree
  = Leaf Token
  | Node [Tree]
  deriving (Show, Eq)

checkToken :: (Token -> Bool) -> [Token] -> Bool
checkToken _ [] = True
checkToken f (x : _)
  | f x = False
  | otherwise = True

getToken :: (Token -> Bool) -> ExceptT (String, Int) (State [Token]) (Maybe Tree)
getToken predicate =
  do
    s <- lift get
    if checkToken predicate s
      then throwError ("unexpected token - " ++ show (head s), length s)
      else lift (put . tail $ s)
    return . Just . Leaf $ head s

predOrXorE :: Token -> Bool
predOrXorE Or = True
predOrXorE Xor = True
predOrXorE _ = False

joinNode :: [Maybe Tree] -> Maybe Tree
joinNode list = return $ Node (catMaybes list)

nodeE :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeE =
  do
    l <- nodeE'
    nodeO l

nodeO :: Maybe Tree -> ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeO left =
  do
    correctToken <- getToken predOrXorE
    tokenE <- nodeE'
    nodeO (joinNode [left, correctToken, tokenE])
    `catchError` (\_ -> return left)

nodeA :: Maybe Tree -> ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeA left =
  do
    correctToken <- getToken (== And)
    tokenN <- nodeN
    nodeA (joinNode [left, correctToken, tokenN])
    `catchError` (\_ -> return left)

nodeE' :: ExceptT (String, Int) (State [Token]) (Maybe Tree)
nodeE' =
  do
    tokenN <- nodeN
    nodeA tokenN

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
