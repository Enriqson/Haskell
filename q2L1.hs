parseString:: String->[(String,String,String)]

parseString [] = []
parseString str = (getAux str,getAux drop1,getAux drop2): parseString drop3
    where drop1 = dropAux str
          drop2 = dropAux (dropAux str)
          drop3 = dropAux (dropAux (dropAux str))


getAux:: String->String

getAux [] = []
getAux (a:as) 
    | a==';' = []
    | otherwise = a:getAux as

dropAux:: String->String

dropAux [] = [] 
dropAux (a:as) 
    | a==';' = as
    | otherwise = dropAux as

parseData:: (String,String,String)->Double

parseData (a,b,c) =read c::Double



minMaxCartao :: String -> (Double, Double)
minMaxCartao logCartao = (minimum filtrado,maximum filtrado)
    where filtrado = map parseData (parseString logCartao)


