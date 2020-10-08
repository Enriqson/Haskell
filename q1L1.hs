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

parseData:: (String,String,String)->(String,Double)

parseData (a,b,c) =([a!!(len-3),a!!(len-2),a!!(len-1)],read c::Double)
    where len = length a



logMes::String->String->Double
logMes mes logCartao = foldl (+)  0 filtrado
    where filtrado = filtrar (map parseData (parseString logCartao))  mes


filtrar:: [(String,Double)]->String->[Double]

filtrar [] _ = []
filtrar (a:as) mes
    | fst(a)==mes = snd(a):(filtrar as mes)
    | otherwise = (filtrar as mes)
