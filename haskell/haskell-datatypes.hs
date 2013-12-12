module HaskellDatatypes where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node x EmptyTree EmptyTree
insert x (node v left right) 
	| x < v = Node v (insert x left) right
	| otherwise = Node v left (insert x right)

contains :: (Ord a) => a -> Tree a -> Bool
contains x EmptyTree = False
contains x (node v left right)
	| x == v = True
	| x < v = (contains x left) 
	| x > v = (contains x right)
	

toList :: Tree a -> [a]
toList EmptyTree = []
toList (Node v left right) = (toList left) ++ [v] ++ (toList right)



remove :: (Ord a) => a -> Tree a -> Tree a
remove x EmptyTree = EmptyTree
remove x (Node v left right)
	| x == v = 
		case (left, right) of
			(EmptyTree, EmptyTree) -> EmptyTree
			(EmptyTree, _) -> right
			(_, EmptyTree) -> left
			(Node vl _ _, Node vr _ _) ->
				if True -- pretend true with 50% probability
					then Node vl (remove vl left) right
				else Node vr left (remove vr right)
	| x < v = Node v (remove x left) right
	| otherwise = Node v left (remove x)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = EmptyTree
fromList (x:xs) = insert x (fromList xs)

bstSort :: (Ord a) => [a] -> [a]
--bstSort = toList . fromList
bstSort lst = toList $ foldl (flip insert) EmptyTree lst

printInOrder :: (Show a) => Tree a -> IO ()
printInOrder EmptyTree = return ()
printInOrder (Node v left right) = do
	printInOrder left
	putStrLn v
	printInOrder right 


streamToTree :: IO (Tree String) 
streamToTree = do
	str <- getLine 
	if null str
	then return EmptyTree
	else do
		tree <- streamToTree
		return $ insert str tree

sortStream :: IO [String]
sortStream  = do
	tree <- streamToTree
	return $ toList tree
