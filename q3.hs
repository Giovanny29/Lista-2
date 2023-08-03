data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read )
move1 :: String ->Int ->Int -> String    --Backspace
move1 [] a  c = []
move1 vet a c= partEsq ++ partDir 
               where partEsq = take (a-c) vet
                     partDir = drop (a) vet

move2 :: String -> Int ->Int-> String   --dELETE
move2 [] a b  = []
move2 vet a c= partEsq ++ partDir 
              where partEsq = take (a) vet
                    partDir = drop (a+ c) vet

move3 :: String -> String -> Int -> String 
move3 a b c = partEsq ++ b ++ partDir 
              where partEsq = take (c ) a
                    partDir = drop (c ) a

rea_func ::(String , Int) -> Cmd ->(String ,Int)
rea_func (str,a) (Cursor b) = (str, a+b)
rea_func (str,a) (Backspace b) = ((move1 str a b ) , (a-b))
rea_func (str,a) (Delete b) = ((move2 str a b ) , a)
rea_func (str,a) (Insert b) = ((move3 str b  a ) ,a)
                              
editText :: String -> [Cmd] -> String
editText [] a = []
editText a [] = a
editText a b = fst(foldl rea_func (a , 0) b) 
main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result
       
