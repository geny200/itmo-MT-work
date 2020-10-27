{-# LANGUAGE BlockArguments #-}

module Graph
  ( 
    evaluateGraph
  )
where

import Control.Monad.State.Lazy   (State, evalState, get, put)
import System.Process             (callCommand)
import Token                      (lexicalAnalyzer)
import Tree                       (Tree (..), evaluateTree)

evaluateGraph :: String -> IO ()
evaluateGraph expr =
  do
    let tree = lexicalAnalyzer expr >>= evaluateTree
    createFileGraph tree
    callCommand "dot -Tjpg tree.gv -o tree.jpg"
    callCommand "tree.jpg"

createFileGraph :: Either String (Maybe Tree) -> IO ()
createFileGraph (Right tree) =
  do
    let str = evalState (toGraph tree) 0
    writeFile "tree.gv" ("graph \"\" {\n" ++ str ++ "}\n")
createFileGraph (Left str) =
  do
    putStrLn str
    fail "parse error"

toGraph :: Maybe Tree -> State Integer String
toGraph Nothing = return []
toGraph (Just tree) = _toGraph tree

_toGraph :: Tree -> State Integer String
_toGraph (Leaf x) =
  do
    num <- get
    put (num + 1)
    return ("n" ++ show num ++ " [label=\"" ++ show x ++ "\"];\n")
_toGraph (Node []) =
  do
    num <- get
    put (num + 1)
    return ("n" ++ show num ++ " [label=\"node\"];\n")
_toGraph (Node (x : res)) =
  do
    numCur <- get
    rest <- _toGraph (Node res)
    numChild <- get
    cur <- _toGraph x
    return ("n" ++ show numCur ++ " -- n" ++ show numChild ++ ";" ++ cur ++ rest)
