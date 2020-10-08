decEnigma:: String->[(Char,Char)]-> String

decEnigma [] _ = ""
decEnigma str [] = str

decEnigma str (a:as) = decEnigma (translate str a) as

translate:: String->(Char,Char)->String

translate [] _ = ""
translate (a:as) (x,y)
    | a==x = y : translate as (x,y)
    | otherwise = a : translate as (x,y)
