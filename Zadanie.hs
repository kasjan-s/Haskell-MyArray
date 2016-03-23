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

fromFile fname = do
  contents <- readFile fname
  return ()

readInt = read :: String -> Int

main = do
  args <- getArgs
  contents <- if null args then getContents else readFile (args !! 0)
  if null contents
     then do putStrLn "Empty data!"
             return ()
    else do let lists = lines contents
            let processed = map words lists
            let transformed = map (map readInt) processed
            let final = foldl (\a e -> if null e then a else (head e, tail e):a) [] transformed
            let maxVert =
                  foldl (\a e -> foldl (\a2 e2 -> max a2 e2) a e) 1 transformed
            let minVert =
                  foldl (\a e -> foldl (\a2 e2 -> min a2 e2) a e) (head $ head transformed) transformed
            -- let rng =
            --       foldl
            --       (\a e -> foldl (\a2 e2 -> (min (fst a2) e2, max (snd a2) e2)) a e)
            --       (v, v)
            --       transformed
            --       where v = head $ head transformed
            let graph = MyArray.array (minVert, maxVert) final
            print $ reachable graph 1
