data Tree = Leaf
          | Branch { left, right :: Tree
                     , split :: Int }
  deriving (Show)

--
tinsert :: Tree -> Int -> Tree
tinsert (Leaf) n = Branch Leaf Leaf n
tinsert (Branch left right split) n | split == n = (Branch left right n)
                                    | n < split = (Branch (tinsert left n) right split)
                                    | n > split = (Branch left (tinsert right n) split)

tlookup :: Tree -> Int -> Maybe Int
tlookup (Leaf) n = Nothing
tlookup (Branch left right split) n | split == n = Just split
                                    | n < split = tlookup left n
                                    | n > split = tlookup right n

tshow :: Int -> Tree -> [String]
tshow i (Leaf) = []
tshow i (Branch left right split) = tshow (i+1) left ++ [indent i++ show split] ++ tshow (i+1) right

tsum :: Tree -> Int
tsum (Leaf) = 0
tsum (Branch left right split) = split + tsum left + tsum right

tprod :: Tree -> Int
tprod (Leaf) = 1
tprod (Branch left right split) = split * tprod left * tprod right

-- tfold allows passing of an operator (+, -) rather than calling tsum/tprod
-- tfold (+) 0 tree == tsum  tree
-- tfold (*) 1 tree == tprod tree
tfold :: (Int -> Int -> Int) -- ^ operator
      -> Int -- ^ initial value
      -> Tree -- ^ tree to be traversed
      -> Int
tfold (op) e Leaf = e
tfold (op) e (Branch left right split) = ((e `op` split) `op` (tfold op e left)) `op` (tfold op e right)

aleaf = tinsert (Leaf) 2
tbranch = tinsert (Branch (Leaf) (Leaf) 17) 13
alpha = tinsert (tinsert (tinsert tbranch 5) 18) 7

indent 0 = "*"
indent 1 = "    "
indent i = "    " ++ indent (i-1)

main = do putStrLn $ unlines (tshow 0 alpha)
--          putStrLn (tshow 0 aleaf)
--          putStrLn (tshow 0 tbranch)
          print (tfold (+) 0 alpha)
          print (tfold (*) 1 alpha)
