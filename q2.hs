data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)
  
inorder :: Tree t -> [t]
inorder Nilt = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

converte::[Int ]->[Int]
converte [] = []
converte (a:ax) = (a `mod` 5) : converte ax

base ::[Int]->[Char]
base [] = []
base (a:ax)|a ==0 = 'E' : base ax
            | a == 1 ='M': base ax
            | a ==  2 ='A' : base ax
            | a == 3 = 'C' :base ax
            |otherwise = 'S' : base ax
splitArray :: [Char] -> [String]
splitArray [] = []
splitArray xs = take 8 xs : splitArray (drop 8 xs)

dna1 :: Tree Int -> [String]
dna1 t = splitArray (base(converte(inorder t)))


main :: IO ()
main = do

  input <- getLine

  let result = dna1 (read input :: Tree Int)

  print result
