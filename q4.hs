data Tree t = Nilt |
              Node t (Tree t) (Tree t) deriving (Read)

isBST :: Ord t => Tree t -> Bool 
isBST Nilt = True
isBST (Node a b c) = let convert_three_list Nilt = []
                         convert_three_list (Node n b c) = convert_three_list b ++ [n] ++ convert_three_list c
                         in ( isBST b && isBST c ) && all (> a) (convert_three_list c) && all (<a) (convert_three_list b)

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result