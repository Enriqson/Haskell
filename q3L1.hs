isReplica:: String-> Int -> Char -> Bool

isReplica [] n _ 
    | n==0 = True
    | otherwise = False

isReplica (a:as) n c
    | a/=c = False
    | n<0 = False
    | otherwise = isReplica as (n-1) c