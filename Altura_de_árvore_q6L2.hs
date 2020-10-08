data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int

alturaArvore Nilt = 0

alturaArvore (Node t t1 t2) = 1 + (max (alturaArvore t1) (alturaArvore t2))