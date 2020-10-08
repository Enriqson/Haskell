data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
    deriving (Read, Show)

    
faces :: Direction -> [Command] -> Direction

faces East as = facesAux as 0
faces North as = facesAux as 90
faces West as = facesAux as 180
faces South as = facesAux as 270

facesAux :: [Command]->Int->Direction

facesAux ((TurnLeft):as) dir = facesAux as (mod (dir+90) 360)
facesAux ((TurnRight):as) dir = facesAux as (mod (dir-90) 360) 

facesAux ((Forward n):as) dir = facesAux as dir 
facesAux ((Backward n):as) dir = facesAux as dir 

facesAux [] 0 = East
facesAux [] 90 = North
facesAux [] 180 = West
facesAux [] 270 = South


