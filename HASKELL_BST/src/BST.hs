module BST

  where


{- BST Declaration
the following terms will be used throughout the code:
  key - polymorphic implementation of the Node's Key
  item - polymorphic implementation of the Node's item
  Leaf - an empty node
  leftChild - the node to the left of the current node
  rightChild - the node to the left of the current node
-}
data BST key item = Node { keyVal::key, itemVal::item, leftChild::BST key item, rightChild::BST key item}
  | Leaf deriving (Ord, Eq)


