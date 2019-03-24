module Sub.Trees.BTree where

data BTree a = Node a (BTree a) (BTree a)