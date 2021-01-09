{-# LANGUAGE BlockArguments #-}

module Lab2.Graph
  ( -- * Function for start draw
    evaluateGraph
  )
where

import Control.Monad.State.Lazy (State, evalState, get, put)
import Lab2.Token (lexicalAnalyzer)
import Lab2.Tree (Tree (..), evaluateTree)
import System.Process (callCommand)

-- | Renders the tree structure of the parser.
-- Also creates a representation of the graph 
-- in the tree.gv file as temporary files, 
-- and saves the image in the file tree.jpg 
-- (in the root of the project)
evaluateGraph 
  :: String       -- ^ String for parsing by the parser
  -> IO ()
evaluateGraph expr =
  do
    let tree = lexicalAnalyzer expr >>= evaluateTree
    createFileGraph tree
    callCommand "dot -Tjpg tree.gv -o tree.jpg"
    callCommand "tree.jpg"
    
--------------------------------------------
----         Auxiliary functions         ---
--------------------------------------------

createFileGraph :: Either String (Maybe Tree) -> IO ()
createFileGraph (Right tree) =
  do
    let str = evalState (toGraph tree) 0
    writeFile "tree.gv" ("graph \"\" {\n" ++ str ++ "}\n")
createFileGraph (Left str) =
  do
    putStrLn str
    error "parse error"

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
