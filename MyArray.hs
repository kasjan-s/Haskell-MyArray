module MyArray
       (Array,
        range,
        index,
        inRange,
        rangeSize,
        listArray,
        elems,
        array,
        (!),
        (//)
       ) where

class Ord a => Ix a where
  -- | range (lo, hi) daje liste wszystkicch indeksow
  --   pomiedzy lo a hi
  range :: (a, a) -> [a]
  -- | index (lo, hi) i daje numer kolejny indeksu i w zakresie
  --  (od 0)
  --  np index (7,9) 8 = 1; index ('a', 'd') 'd' = 3
  -- komunikat o bledzie jesli indeks poza zakresem
  index :: (a, a) -> a -> Int
  index (lo, hi) i | i > hi = error "Index out of range (too big)"
                   | i < lo = error "Index out of range (too small)"
                   | otherwise = index (range (lo, hi)) i 0 where
                       index (h:t) n a | h == n = a
                                       | otherwise = index t n (a+1)
  inRange :: (a, a) -> a -> Bool
  inRange (lo, hi) i = lo <= i && i <= hi
  rangeSize :: (a,a) -> Int
  rangeSize (lo, hi) | hi >= lo = 0
                     | otherwise = length $ range (lo, hi)

instance Ix Char where
  range (lo, hi) = [lo..hi]
  index (lo, hi) i | i > hi = error "Index out of range (too big)"
                   | i < lo = error "Index out of range (too small)"
                   | otherwise = fromEnum i - fromEnum lo
  rangeSize (lo, hi) | hi >= lo = fromEnum hi - fromEnum lo + 1
                     | otherwise = 0

instance Ix Int where
  range (lo, hi) = [lo..hi]
  index (lo, hi) i | i > hi = error "Index out of range (too big)"
                   | i < lo = error "Index out of range (too small)"
                   | otherwise = i - lo
  rangeSize (lo, hi) | hi >= lo = hi - lo + 1
                     | otherwise = 0

instance Ix Integer where
  range (lo, hi) = [lo..hi]
  index (lo, hi) i | i > hi = error "Index out of range (too big)"
                   | i < lo = error "Index out of range (too small)"
                   | otherwise = fromIntegral i - fromIntegral lo
  rangeSize (lo, hi) | hi >= lo = fromIntegral hi - fromIntegral lo + 1
                     | otherwise = 0

instance (Ix a, Ix b) => Ix (a, b) where
  range ((lo1, hi1), (lo2, hi2)) = [(a, b) | a <- range (lo1, lo2), b <- range (hi1, hi2)]
  rangeSize ((lo1, hi1), (lo2, hi2))
    | hi2 >= hi1 && lo2 >= lo1 = rangeSize (hi1, hi2) * rangeSize (lo1, lo2)
    | otherwise = 0



-- Array implementation

data Array i e = Array { tree :: Tree i e, arrayRange :: (i, i) }

-- | Buduje tablice dla danego zakresu i listy elementow
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (lo, hi) elements = Array {tree = t, arrayRange=(lo,hi)}
  where t = foldl (\a (i, e) -> insert i e a) Leaf values
        values = zip (range (lo, hi)) (elements)

-- | Daje element tablicy o podanym indeksie
(!) :: Ix i => Array i e -> i -> e
(!) array i | inRange (arrayRange array) i = getelem i (tree array)
            | otherwise = error "Index out of range"

-- | Daje liste elementow tablicy (w kolejnosci indeksow)
elems :: Ix i => Array i e -> [e]
elems array = treeElems (tree array)

-- | Buduje tablice z podanej listy par (indeks, wartosc)
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array (lo, hi) values = Array { tree = t, arrayRange = (lo, hi)}
  where t =
          foldl
          (\a (i, e) -> if inRange (lo, hi) i
                        then insert i e a
                        else error "Index out of range")
          Leaf values

-- | Daje tablice bedaca wariantem danej, zmieniona pod podanym indeksem
update :: Ix i => i -> e -> Array i e -> Array i e
update i e array
  | inRange (arrayRange array) i =
    Array { tree = insert i e (tree array), arrayRange = (arrayRange array)}
  | otherwise = error "Index out of range"

-- | Daje tablice bedaca wariantem danej, zmieniona pod podanymi indeksami
(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) array values = foldl (\a (i, e) -> update i e array) array values


-- RedBlack Tree implementation (Okasaki)

data Color = Red | Black deriving (Eq, Show)
data Tree i e = Leaf | Node Color i e (Tree i e) (Tree i e) deriving Show

member :: Ord i => i -> Tree i e -> Bool
member _ Leaf = False
member x (Node _ y _ l r) | x == y = True
                          | x < y = member x l
                          | x > y = member x r

insert :: Ord i => i -> e -> Tree i e -> Tree i e
insert i1 e t = makeBlack (ins t)
  where ins Leaf = Node Red i1 e Leaf Leaf
        ins node@(Node c i2 e2 l r) | i1 == i2 = Node c i2 e l r
                                    | i1 < i2 = balance c i2 e2 (ins l) r
                                    | i1 > i2 = balance c i2 e2 l (ins r)
        makeBlack (Node _ i2 e l r) = Node Black i2 e l r

balance :: Color -> i -> e -> Tree i e -> Tree i e -> Tree i e
balance Black gi ge (Node Red pi pe (Node Red i e l r) pr) uncle =
  Node Red pi pe (Node Black i e l r) (Node Black gi ge pr uncle)
balance Black gi ge (Node Red pi pe pl (Node Red i e l r)) uncle =
  Node Red i e (Node Black pi pe pl l) (Node Black gi ge r uncle)
balance Black gi ge uncle (Node Red pi pe pl (Node Red i e l r)) =
  Node Red pi pe (Node Black gi ge uncle pl) (Node Black i e l r)
balance Black gi ge uncle (Node Red pi pe (Node Red i e l r) pr) =
  Node Red i e (Node Black gi ge uncle l) (Node Black pi pe r pr)
balance color i e l r = Node color i e l r


height Leaf = (0, 0)
height (Node _ _ _ l r) = (max (fst $ height l) (fst $ height r) + 1, min (snd $ height l) (snd $ height r) + 1)

black_height Leaf = (0, 0)
black_height (Node c _ _ l r) = (max (fst $ black_height l) (fst $ black_height r) + val, min (snd $ black_height l) (snd $ black_height r) + val) where val = if c == Black then 1 else 0

getelem :: Ord i => i -> Tree i e -> e
getelem _ Leaf = error "No element at that index"
getelem i (Node _ j e l r) | i == j = e
                           | i < j = getelem i l
                           | i > j = getelem i r

foldTree :: (a -> a -> e -> a) -> a -> Tree i e -> a
foldTree f a Leaf = a
foldTree f a (Node _ _ e l r) = f (foldTree f a l) (foldTree f a r) e

treeElems :: Tree i e -> [e]
treeElems tree = treeElems tree []
  where treeElems Leaf acc = acc
        treeElems (Node _ _ e l r) acc = treeElems l (e:treeElems r acc)
