data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

newDirec :: Direction -> Direction
newDirec North = South
newDirec South = North
newDirec West = East
newDirec East = West

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

walk :: Direction -> Command -> Direction
walk a (Forward _) = newDirec a
walk a (Backward _) = newDirec (newDirec (newDirec a))
walk a TurnLeft = turnLeft a
walk a TurnRight = turnRight a

faces :: Direction -> [Command] -> Direction
faces a b = foldl walk a b

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result