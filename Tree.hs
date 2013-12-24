module Tree where

data Tree a =
    Node (Tree a) (Tree a)
  | Leaf a
  | Empty
  deriving (Eq, Show)

leaves :: Tree a -> Integer
leaves (Leaf a)=1;
leaves (Node tl t2)=leaves (tl)+leaves (t2);

insert :: a -> Tree a -> Tree a
insert (a) (Empty) = Leaf a
insert (a) (Leaf b) = Node (Leaf a) (Leaf b)
-- look at all the subtrees, apply leaves to all, check smallest leaves, add
insert (a) (Node b c) = if leaves(b) < leaves(c) then Node (insert (a) (b)) c else Node (insert (a) (c)) b
-- check if b or c has less leaves. pick least. recursively go deeper. call base case

fold :: (a -> a -> a) -> Tree a -> a
fold (op) (Leaf b) = b 
fold (op) (Node b c) = op (fold (op) (b)) (fold (op) (c))

--eof