import Control.Monad
import qualified Data.Set as Set
import System.Environment
import System.IO (isEOF)

import MyArray

type Graph = Array Int [Int]

reachable :: Graph -> Int -> [Int]
reachable graph vert =
  Set.elems (reachable graph vert Set.empty)
  where
    reachable graph vert visited =
      if (Set.member vert visited)
      then visited
      else let new_visited = Set.insert vert visited
               new_verts = filter (\e -> Set.notMember e new_visited) (graph ! vert)
           in
             if null new_verts
             then new_visited
             else foldl (\a e -> reachable graph e a) new_visited new_verts

-- main = do
--   done <- isEOF
--   unless done $ do
--     inp <- getLine
--     putStrLn inp
--     main

g1 = (1, [2 :: Int])
g2 = (2, [3, 4])
g3 = (3, [2])
g4 = (4, [])

graph = MyArray.array (1 :: Int, 4) [g1, g2, g3, g4]



