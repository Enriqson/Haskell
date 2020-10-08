data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Show)

insertList :: Ord t => Tree t -> [t] -> Tree t

insertList t (a:as) = insertList (insertAux t a) as
insertList t [] = t


insertAux :: Ord t=> Tree t-> t-> Tree t

insertAux(Nilt) input = (Node input Nilt Nilt) 

insertAux(Node v t1 t2) input | input<v = (Node v (insertAux t1 input) t2)
                              | otherwise = (Node v t1 (insertAux t2 input))
