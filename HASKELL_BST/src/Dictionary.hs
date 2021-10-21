module Dictionary
  ( Dictionary, isLeaf, create, displayAsList, displayIndented, lookup, insert, delete, deleteIf )
  where


import Prelude hiding (lookup)
import BST

type Dictionary = BST



data ListItem key item = ListNode {indent::Int, keyval::key, itemval::item, leaf::Bool}

-- overwrite the default print with a prettier, indented version based upon our treeAsList function
instance (Show key, Show item, Eq key, Eq item) => Show (BST key item) where
  show tree = unlines (displayIndented tree)


bt :: BST Int String
bt = insert 37 "Margaret" (insert 26 "Gareth" (insert 29 "Russell" (insert 33 "Peter" (insert 11 "John" (insert 22 "Mary" create)))))


-- number of spaces to apply per indentation
indentMultiplier :: Int
indentMultiplier = 4



-- creates an empty BST
create :: BST key item
create = Leaf



-- helper function which creates indentation
createIndentation :: Int -> String
createIndentation indent = replicate (indent * indentMultiplier) ' '



-- helper function which strips the leading and trailing \" from a shown string
noQuotes :: (Show a) => a -> String
noQuotes input = filter (`notElem` "\"\'") (Prelude.show input)



-- displays the tree as a list without indentation
displayAsList :: (Show key, Show item, Eq key, Eq item) => BST key item -> [(key, item, Bool)]
displayAsList tree = map formatList (bstToList tree 0)



-- displays the tree with indentation (also used by the updated Show instance)
displayIndented :: (Show key, Show item, Eq key, Eq item) => BST key item -> [String]
displayIndented tree = map formatIndented (bstToList tree 0)


formatList :: (Int, key, item, Bool) -> (key, item, Bool)
formatList (indent, key, item, isALeaf) = (key, item, isALeaf)


formatIndented ::(Show key, Show item) => (Int, key, item, Bool) -> String
formatIndented (indent, key, item, True) = createIndentation indent ++ noQuotes key ++ " - " ++ noQuotes item ++ "(*)"
-- otherwise create content without the star and recurse left and right child nodes
formatIndented (indent, key, item, False) = createIndentation indent ++ noQuotes key ++ " - " ++ noQuotes item



-- converts a BST to a List and implements indentation
bstToList :: (Show key, Show item, Eq key, Eq item) => BST key item -> Int -> [(Int, key, item, Bool)]
bstToList Leaf _ = [] -- return an empty list if the tree is a Leaf
-- return content with a "(*) to denote a leaf if both child nodes are leaves"
bstToList (Node key item Leaf Leaf) indent = [(indent, key, item, True)] 
-- otherwise create content without the star and recurse left and right child nodes
bstToList (Node key item leftChild rightChild) indent = [(indent, key, item, False)] 
  ++ bstToList leftChild (indent + 1) ++ bstToList rightChild (indent + 1)



lookup ::(Ord key) => key -> BST key item -> Maybe item
lookup _ Leaf = Nothing -- return Nothing if the tree is a Leaf
lookup soughtKey (Node key item leftChild rightChild)
  | soughtKey < key = lookup soughtKey leftChild -- recurse left child node if less than
  | soughtKey > key = lookup soughtKey rightChild -- recurse right child node if more than
  | otherwise = Just item -- return the value



insert :: (Ord key) => key -> item -> BST key item -> BST key item
insert newKey newItem Leaf = Node newKey newItem Leaf Leaf -- create a new root if the original is a leaf
insert newKey newItem (Node key item leftChild rightChild)
  | newKey == key = Node key newItem leftChild rightChild -- overwrite the existing item if it's a duplicate
  | newKey > key  = Node key item leftChild (insert newKey newItem rightChild) -- if soughtKey greater than key, insert into the rightChild
  | otherwise     = Node key item (insert newKey newItem leftChild) rightChild -- else insert into the leftChild



delete :: (Ord key, Eq key, Eq item) => key -> BST key item -> BST key item
delete _ Leaf = Leaf  -- return a leaf if passed a leaf
delete soughtKey (Node key item leftChild rightChild)
  | soughtKey < key = Node key item (delete soughtKey leftChild) rightChild -- recurse leftChild if value < key
  | soughtKey > key = Node key item leftChild (delete soughtKey rightChild) -- recurse rightChild if value > key
--delete soughtKey (Node key item leftChild rightChild) soughtKey must match key at this point
delete _ (Node _ _ Leaf Leaf) = Leaf -- if both children are leaves return a leaf
delete _ (Node _ _ leftChild Leaf) = leftChild -- if leftChild isnt a leaf, but rightChild is, return leftChild
delete _ (Node _ _ Leaf rightChild) = rightChild -- if rightChild isnt a leaf, but leftChild is, return rightChild 
-- both child nodes are present so we must replace the current node with the smallest node on the rightChild
-- make a new root with minimumKey and a new right child with the minimum value removed
delete _ (Node key item leftChild rightChild) = Node minimumKey minimumItem leftChild (delete minimumKey rightChild)
    where minimumValues = findMinimumNode rightChild
          minimumKey = fst minimumValues
          minimumItem = snd minimumValues



-- helper function which finds and returns the minimum node in a tree
findMinimumNode :: (Ord key, Eq key, Eq item) => BST key item -> (key, item)
findMinimumNode (Node key item leftChild rightChild)
  | leftChild == Leaf = (key, item)
  | otherwise = findMinimumNode leftChild



-- apply a function to deletion within the tree
deleteIf ::(Ord key, Eq key, Eq item) => (key -> Bool) -> BST key item -> BST key item
deleteIf _ Leaf = Leaf -- return a leaf if passed a leaf
deleteIf remove (Node key item leftChild rightChild) =
    if remove key
      {- need to re-apply the deleteIf to catch instances where the root was deleted and 
      replaced with a node which would also match otherwise just apply to leftChild and rightChild -}
      then deleteIf remove (delete key (Node key item leftChild rightChild))  -- 
      else Node key item (deleteIf remove leftChild) (deleteIf remove rightChild)



-- checks to see if the passed tree is a leaf
isLeaf :: (Eq key, Eq item) => BST key item -> Bool
isLeaf tree
  | tree == Leaf = True  
  | otherwise = False