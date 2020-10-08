data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)


destination :: (Int,Int) -> [Command] -> (Int,Int) 

destination t cm = destinationAux t cm 90

destinationAux :: (Int,Int)->[Command]->Int->(Int,Int)

destinationAux (x,y) ((TurnLeft):as) dir = destinationAux (x,y) as (mod (dir+90) 360)
destinationAux (x,y) ((TurnRight):as) dir = destinationAux (x,y) as (mod (dir-90) 360) 

destinationAux (x,y) ((Forward n):as) dir = destinationAux (x+(getMovementX dir n),y+(getMovementY dir n)) as dir 
destinationAux (x,y) ((Backward n):as) dir = destinationAux (x-(getMovementX dir n),y-(getMovementY dir n)) as dir 

destinationAux t [] _ = t

getMovementX :: Int->Int->Int

getMovementX 0 n = n
getMovementX 180 n = -n
getMovementX _ _ = 0

getMovementY :: Int->Int->Int

getMovementY 90 n = n
getMovementY 270 n = -n
getMovementY _ _ = 0

