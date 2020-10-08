btoi :: String -> Int

btoi [] = 0
btoi (a:as) = ((read (a:[])::Int)*(2^(length as))) + btoi as