{-# LANGUAGE TypeSynonymInstances #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad
import Dictionary
import BST
import Data.List hiding(insert, delete)

-- Tasty Setup ------------------------------------------------------------------------------

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]


unitTests = testGroup "Unit tests" [createTests, isLeafTests, lookupTests, insertTests, displayAsListTests, displayIndentedTests, deleteTests, deleteIfTests]
propertyTests = testGroup "Property tests" [typeTests, sizeTests, orderTests]


-- UNIT TEST - SETUP ------------------------------------------------------------------------------
emptyBST :: Dictionary Int String
emptyBST = create

nonEmptyBST :: Dictionary Int String
nonEmptyBST = insert 33 "Peter" (insert 11 "John" (insert 22 "Mary" Dictionary.create))

nonEmptyBSTAsList :: [(Int, String, Bool)]
nonEmptyBSTAsList = [(22, "Mary", False), (11, "John", True), (33, "Peter", True)]

nonEmptyBSTIndented :: [String]
nonEmptyBSTIndented = ["22 - Mary", "    11 - John(*)", "    33 - Peter(*)"]

nonEmptyBSTNoLeftChild :: Dictionary Int String
nonEmptyBSTNoLeftChild = insert 33 "Peter" (insert 22 "Mary" create)

nonEmptyBSTNoRightChild :: Dictionary Int String
nonEmptyBSTNoRightChild = insert 11 "John" (insert 22 "Mary" create)

complexBST :: Dictionary Int String
complexBST = insert 37 "Margaret" (insert 26 "Gareth" (insert 29 "Russell" (insert 33 "Peter" (insert 11 "John" (insert 22 "Mary" Dictionary.create)))))

complexBSTDeletePeter :: Dictionary Int String
complexBSTDeletePeter = insert 26 "Gareth" (insert 29 "Russell" (insert 37 "Margaret" (insert 11 "John" (insert 22 "Mary" Dictionary.create))))

complexBSTDeleteEven :: Dictionary Int String
complexBSTDeleteEven = delete 26 (delete 22 complexBST)

complexBSTDeleteOdd :: Dictionary Int String
complexBSTDeleteOdd = insert 26 "Gareth" (insert 22 "Mary" create)

complexBSTAsList :: [(Int, String, Bool)]
complexBSTAsList = [(22, "Mary", False), (11, "John", True), (33, "Peter", False), (29, "Russell", False), (26, "Gareth", True), (37, "Margaret", True)]

complexBSTMultDeletes :: Dictionary Int String
complexBSTMultDeletes = insert 29 "Russell" (insert 37 "Margaret" (insert 11 "John" (insert 26 "Gareth" create)))

displayIndentedResult :: [String]
displayIndentedResult = ["22 - Mary", "    11 - John(*)", "    33 - Peter", "        29 - Russell", "            26 - Gareth(*)", "        37 - Margaret(*)"]

complexLT33 :: Dictionary Int String
complexLT33 = insert 37 "Margaret" (insert 33 "Peter" create)

onlyPeter :: Dictionary Int String
onlyPeter = insert 33 "Peter" create

onlyJohn :: Dictionary Int String
onlyJohn = insert 11 "John" create

polymorphicBST :: Dictionary Char Int
polymorphicBST = insert 'e' 1 (insert 'a' 44 (insert 'c' 66 create))

polymorphicBSTAsList :: [(Char, Int, Bool)]
polymorphicBSTAsList = [('c', 66, False), ('a', 44, True), ('e', 1, True)]

polymorphicBSTIndented :: [String]
polymorphicBSTIndented = ["c - 66", "    a - 44(*)", "    e - 1(*)"]

modThreeIsZero:: Int -> Bool
modThreeIsZero input =
    input `mod` 3 == 0


-- UNIT TESTS ------------------------------------------------------------------------------
createTests = testGroup "create Function Tests"
    [
        testCase "create_compare_with_leaf" $ assertEqual "create and compare to leaf" True (isLeaf emptyBST)
    ]

isLeafTests = testGroup "isLeaf Function Tests"
    [
        testCase "isLeaf_emptyBST" $ assertEqual "is an empty BST a leaf?" True (isLeaf emptyBST),
        testCase "isLeaf_nonEmptyBST" $ assertEqual "is a non-empty BST a leaf?" False (isLeaf nonEmptyBST)
    ]

lookupTests = testGroup "lookup Function Tests"
    [
        testCase "lookup_in_empty_BST" $ assertEqual "lookup value 22 in empty BST" Nothing (Dictionary.lookup 22 emptyBST),
        testCase "lookup_in_non_empty_BST" $ assertEqual "lookup value 22 in non-empty BST" (Just "Mary") (Dictionary.lookup 22 nonEmptyBST),
        testCase "lookup_value_less_than_root" $ assertEqual "lookup value 11 which is < root value" (Just "John") (Dictionary.lookup 11 nonEmptyBST),
        testCase "lookup_value_greater_than_root" $ assertEqual "lookup 33 which is > root value" (Just "Peter") (Dictionary.lookup 33 nonEmptyBST),
        testCase "lookup_value_in_complex_BST" $ assertEqual "lookup 33 which is > root value" (Just "Gareth") (Dictionary.lookup 26 complexBST),
        testCase "lookup_polymorphic" $ assertEqual "lookup 'a' which is > root value" (Just 44) (Dictionary.lookup 'a' polymorphicBST)
    ]

insertTests = testGroup "Insert Function Tests"
    [
        testCase "insert_empty" $ assertEqual "Insert 22 Mary in empty BST" (Just "Mary") (Dictionary.lookup 22 (insert 22 "Mary" emptyBST)),
        testCase "insert_non_empty" $ assertEqual "Insert 99 Harry in non-empty BST" (Just "Harry") (Dictionary.lookup 99 (insert 99 "Harry" nonEmptyBST)),
        testCase "insert_overwrite_existing" $ assertEqual "Overwrite an existing value" (Just "Louise") (Dictionary.lookup 22 (insert 22 "Louise" nonEmptyBST)),
        testCase "insert_into_complex" $ assertEqual "Insert into a complex BST" (Just "Graham") (Dictionary.lookup 99 (insert 99 "Graham" complexBST)),
        testCase "insert_polymorphic" $ assertEqual "Insert l 66 in parametric BST" (Just 66) (Dictionary.lookup 'l' (insert 'l' 66 polymorphicBST))
    ]

displayAsListTests = testGroup "displayAsList Function Tests"
    [
        testCase "display_empty" $ assertEqual "display an empty BST" [] (displayAsList emptyBST),
        testCase "display_non_empty" $ assertEqual "display a non-empty BST" nonEmptyBSTAsList (displayAsList nonEmptyBST),
        testCase "display_complex" $ assertEqual "display a complex BST" complexBSTAsList (displayAsList complexBST),
        testCase "display_parametric" $ assertEqual "display a polymorphic BST" polymorphicBSTAsList (displayAsList polymorphicBST)
    ]

displayIndentedTests = testGroup "displayIndented Function Tests"
    [
        testCase "display_indented_empty" $ assertEqual "display an empty BST" [] (displayIndented emptyBST),
        testCase "display_indented_non_empty" $ assertEqual "display a non-empty BST" nonEmptyBSTIndented (displayIndented nonEmptyBST),
        testCase "display_indented_complex" $ assertEqual "display a complex BST" displayIndentedResult (displayIndented complexBST),
        testCase "display_indented_parametric" $ assertEqual "display a polymorphic BST" polymorphicBSTIndented (displayIndented polymorphicBST)
    ]

deleteTests = testGroup "delete Function Tests"
    [
        testCase "delete_from_empty" $ assertEqual "delete from an empty BST" emptyBST (delete 44 emptyBST),
        testCase "delete_left_child_from_non_empty" $ assertEqual "delete left child from non-empty BST" nonEmptyBSTNoLeftChild (delete 11 nonEmptyBST),
        testCase "delete_right_child_from_non_empty" $ assertEqual "delete right child from non-empty BST" nonEmptyBSTNoRightChild (delete 33 nonEmptyBST),
        testCase "delete_root_no_right_child" $ assertEqual "delete root from non-empty BST with no rightChild" onlyJohn (delete 22 nonEmptyBSTNoRightChild),
        testCase "delete_root_no_left_child" $ assertEqual "delete root from non-empty BST with no leftChild" onlyPeter (delete 22 nonEmptyBSTNoLeftChild),
        testCase "delete_root_both_children" $ assertEqual "delete root where both child nodes exist" complexBSTDeletePeter (delete 33 complexBST),
        testCase "delete_multiple" $ assertEqual "delete multiple from complex BST" complexBSTMultDeletes (delete 22 (delete 33 complexBST))
    ]

deleteIfTests = testGroup "deleteIf Function Tests"
    [
        testCase "deleteIf_even_entries" $ assertEqual "delete even entries from a BST" complexBSTDeleteEven (deleteIf even complexBST),
        testCase "deleteIf_odd_entries" $ assertEqual "delete odd entries from a BST" complexBSTDeleteOdd (deleteIf odd complexBST),
        testCase "deleteIf_greater_than_22" $ assertEqual "delete entries > 22 from a BST" nonEmptyBSTNoRightChild (deleteIf (> 22) complexBST),
        testCase "deleteIf_less_than_22" $ assertEqual "delete entries < 33 from a BST" complexLT33 (deleteIf (< 33) complexBST),
        testCase "deleteIf_custom_mod_function" $ assertEqual "delete entries where custom function for mod 3 = 0 from a BST" complexBSTDeletePeter (deleteIf modThreeIsZero complexBST)
    ]



-- PROPERTY TESTS SETUP ------------------------------------------------------------------------------

instance (Arbitrary key, Arbitrary item) => Arbitrary (BST key item) where arbitrary = sized balancedTree

--instance (Arbitrary key, Arbitrary item) => Arbitrary (BST key item) where arbitrary = sized unbalancedTree


-- | generate a tree of given size
-- size ~ number of branch nodes
balancedTree :: (Arbitrary key, Arbitrary item) => Int -> Gen (BST key item)
balancedTree size
  | size>0 = do key <- arbitrary
                item <- arbitrary
                leftChild <- balancedTree (size`div`2)
                rightChild <- balancedTree (size`div`2)
                return (Node key item leftChild rightChild)
  | otherwise = return Leaf 


{-unbalancedTree :: (Arbitrary key, Arbitrary item) => Int -> Gen (BST key item)
unbalancedTree size
  | size>0 = frequency [(4, generateNode), (1, return Leaf)] -- frequency at which nodes and leaves happen
  | otherwise = return Leaf
  where

    generateNode = do 
        -- generate calues for a node 
        key <- arbitrary
        item <- arbitrary
        leftChild <- unbalancedTree (size`div`2)
        rightChild <- unbalancedTree (size`div`2)
        return (Node key item leftChild rightChild) -}

data Person = Person String Int deriving (Eq, Show)

instance Arbitrary Person where
  arbitrary = genPerson

genPerson :: Gen Person
genPerson =
   do name <- elements ["Russell", "Peter", "John", "Gareth", "Jane", "Mary", "Harold", "Edward", "Victoria", "Matilda", "Oliver", "Elizabeth", "Henry", "Stephen", "James", "Edward", "Anne", "Elizabeth", "William", "Charles"]
      age <- choose (21, 67)
      return (Person name age)

minimumKey :: (Ord key, Eq key, Eq item) => BST key item -> key
minimumKey (Node key item leftChild rightChild)
  | leftChild == Leaf = key
  | otherwise = minimumKey leftChild

maximumKey :: (Ord key, Eq key, Eq item) => BST key item -> key
maximumKey (Node key item leftChild rightChild)
  | rightChild == Leaf = key
  | otherwise = maximumKey rightChild

numNodes :: BST key item -> Int
numNodes Leaf = 0
numNodes (Node _ _ leftChild rightChild) = 1 + numNodes leftChild + numNodes rightChild

{- generated BST's dont work as BST's because they implement duplicates sometimes so can't check that

isBST :: Ord key => BST key item -> Bool
isBST Leaf = True
isBST (Node key item leftChild rightChild) = allTree (<= key) leftChild && allTree (>= key) rightChild && isBST leftChild && isBST rightChild
    where   allTree :: (key -> Bool) -> BST key item -> Bool
            allTree f (Node key item leftChild rightChild) = f key && allTree f leftChild && allTree f rightChild
            allTree f (Leaf) = True -}

{- generated BST's implement duplicates and arent ordered sometimes so can't check that either
getKeyList :: BST key item -> [key]
getKeyList Leaf = []
getKeyList (Node key _ leftChild rightChild) = getKeyList leftChild ++ [key] ++ getKeyList rightChild

checkTreeIsOrdered :: (Ord key) => [key] -> Bool
checkTreeIsOrdered [] = True
checkTreeIsOrdered [x] = True
checkTreeIsOrdered (x:y:xs) = x <= y && checkTreeIsOrdered (y:xs) -}

prop_insertLookupCharChar :: Char -> Char -> BST Char Char -> Bool
prop_insertLookupCharChar key item tree = Just(item) == (Dictionary.lookup key (insert key item tree))

prop_insertLookupIntInt :: Int -> Int -> BST Int Int -> Bool
prop_insertLookupIntInt key item tree = Just(item) == (Dictionary.lookup key (insert key item tree))

prop_insertLookupIntString :: Int -> String -> BST Int String -> Bool
prop_insertLookupIntString key item tree = Just(item) == (Dictionary.lookup key (insert key item tree))

prop_insertLookupIntPerson :: Int -> Person -> BST Int Person -> Bool
prop_insertLookupIntPerson key item tree = Just(item) == (Dictionary.lookup key (insert key item tree))


prop_biggerAfterInsert :: Int -> Int -> BST Int Int -> Bool 
prop_biggerAfterInsert key item tree = numNodes tree == numNodes finalTree || numNodes tree == (numNodes finalTree) -1
    where finalTree = insert key item tree

prop_smallerAfterDelete :: Int -> Person -> BST Int Person -> Bool 
prop_smallerAfterDelete key item tree = numNodes tree >= numNodes finalTree
    where finalTree = delete key (insert key item tree)


-- PROPERTY TESTS ------------------------------------------------------------------------------

typeTests = testGroup "Test with various types"
  [ QC.testProperty "Testing BST with: Int Int" (prop_insertLookupIntInt),
    QC.testProperty "Testing BST with: Char Char" (prop_insertLookupCharChar),
    QC.testProperty "Testing BST with: Int String" (prop_insertLookupIntString),
    QC.testProperty "Testing BST with: Int Person" (prop_insertLookupIntPerson)
  ]

sizeTests = testGroup "Size after insert or delete Tests"
  [ QC.testProperty "Test that the BST is bigger (or equal since we overwrite existing keys) after an insert" (prop_biggerAfterInsert),
    QC.testProperty "Test that the BST is bigger (or equal since we overwrite existing keys) after an insert" (prop_smallerAfterDelete)
  ]

orderTests = testGroup "Ordering Tests"
  [ --QC.testProperty "Test that the BST is bigger (or equal since we overwrite existing keys) after an insert" (prop_checkOrdered)
  ]
