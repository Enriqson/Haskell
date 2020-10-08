type Comando = String
type Valor = Int

executa ::[(Comando,Valor)]->Int

executa as = executaAux as 0

executaAux :: [(Comando,Valor)]->Int->Int

executaAux [] n = n
executaAux (a:as) n = executaAux as (efetua a n)

efetua :: (Comando,Valor)->Int->Int

efetua ("Multiplica",v) n = n*v
efetua ("Soma",v) n = n+v
efetua ("Subtrai",v) n = n-v
efetua ("Divide",0) n = -666
efetua ("Divide",v) n = n `div` v