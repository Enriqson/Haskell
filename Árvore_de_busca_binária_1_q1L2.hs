data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

isBST :: Ord t => Tree t -> Bool

isBST(Node v Nilt Nilt) = True
isBST(Node v (Node v1 t1 t2) Nilt) =  (v>maxValue(Node v1 t1 t2)) && isBST (Node v1 t1 t2)
isBST(Node v Nilt (Node v1 t1 t2)) =  (v<minValue(Node v1 t1 t2)) && isBST (Node v1 t1 t2)
isBST(Node v (Node v1 t11 t12) (Node v2 t21 t22)) =  (v>maxValue(Node v1 t11 t12))&&(v<minValue(Node v2 t21 t22)) && isBST (Node v1 t11 t12) && isBST (Node v2 t21 t22)
isBST(Nilt) = True

minValue :: Ord t => Tree t -> t


minValue (Node v (Nilt) (Nilt)) = v

minValue (Node v (Nilt) t2) | (v<minValue(t2)) = v
                            | otherwise = minValue(t2)

minValue (Node v t1 (Nilt)) | (v<minValue(t1)) = v
                            | otherwise = minValue(t1)

minValue (Node v t1 t2) | ((v<minValue(t1))&&(v<minValue(t2))) = v
                        | ((v>minValue(t1))&&(minValue(t1)<minValue(t2))) = minValue(t1)
                        | otherwise = minValue(t2)


maxValue :: Ord t => Tree t -> t

maxValue (Node v (Nilt) (Nilt)) = v
                          
maxValue (Node v (Nilt) t2) | (v>maxValue(t2)) = v
                            | otherwise = maxValue(t2)

maxValue (Node v t1 (Nilt)) | (v>maxValue(t1)) = v
                            | otherwise = maxValue(t1)

maxValue (Node v t1 t2) | ((v>maxValue(t1))&&(v>maxValue(t2))) = v
                        | ((v<maxValue(t1))&&(maxValue(t1)>maxValue(t2))) = maxValue(t1)
                        | otherwise = maxValue(t2)