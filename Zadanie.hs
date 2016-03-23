import Control.Monad
import qualified Data.Set as Set
import System.Environment

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

readInt = read :: String -> Int

main = do
  args <- getArgs
  contents <- if null args then getContents else readFile (args !! 0)
  if null contents
    then do putStrLn "Empty data!"
    else do
            let ints = map (map readInt) (map words (lines contents))
            let lists = foldl (\a e -> if null e then a else (head e, tail e):a) [] ints
            -- Sprawdzam czy wierzcholek 1 jest poprawnie zdefiniowany.
            -- Jesli go nie ma, to funkcja reachable zwrocila by blad, gdyz
            -- (!) zwraca blad dla indeksow bez zdefiniowanych wartosci.
            -- Tresc zadania sugeruje, ze mozna to pominac i uznac, ze taki wierzcholek
            -- jest niepoprawny, ale nie jestem pewien tej interpretacji.
            let vert1present = foldr (\e a -> (head e) == 1 || a) False ints
            if vert1present
              then do let v = head $ head ints
                      let maxVert =
                            foldl (\a e -> foldl (\a2 e2 -> max a2 e2) a e) v ints
                      let minVert =
                            foldl (\a e -> foldl (\a2 e2 -> min a2 e2) a e) v ints
                      let graph = MyArray.array (minVert, maxVert) lists
                      print $ reachable graph 1
              else do print ([] :: [Int])


