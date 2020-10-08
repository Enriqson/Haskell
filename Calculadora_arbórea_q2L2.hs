data Ops = SUM | MUL | SUB
           deriving (Read)

getFunction:: Ops->Int->Int->Int

getFunction (SUM) = (+)
getFunction (MUL) = (*)
getFunction (SUB) = (-)

data IntTree = Nilt Int |
    Node Ops IntTree IntTree
    deriving (Read)

evalTree:: IntTree-> Int

evalTree (Nilt a) = a

evalTree (Node op t1 t2) = getFunction(op) (evalTree t1) (evalTree t2)