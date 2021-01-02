module Solution where

import Data.List

----------------------------------------------------------------------
-- Recursing through lists
--
-- The following problems may be solved by writing recursive
-- functions which pattern match on an input list (or lists).
--
-- We will do more examples in class, but you can find one already 
-- in lec/jan28-maybe--pattern-matching/Inclass.hs,
-- namely anyNothingList.
----------------------------------------------------------------------

{- drop every other element from the input list, beginning
   with the first element.  So p1 [1,2,3,4,5,6] should return
   [2,4,6]. -}
p1 :: [a] -> [a]
p1 [] = []
p1 (a : []) = []
p1 (a : b : xs) = b : p1 xs

{- drop every other element from the input list, beginning
   with the second element.  So p1 [1,2,3,4,5,6] should return
   [1,3,5]. -}
p2 :: [a] -> [a]
p2 [] = []
p2 (a : []) = a : []
p2 (a : b : xs) = a : p2 xs

{- duplicate every element in the list.  So p3 "abc" should return
   "aabbcc". -}
p3 :: [a] -> [a]
p3 [] = []
p3 (x : xs) = x : x : p3 xs

-- tower of exponents: [1,2,3,4] should turn into 1 ^ 2 ^ 3 ^ 4
p4 :: Integral a => [a] -> a
p4 [] = 1
p4 (x : xs) = x ^ (p4 xs)

{- This function is supposed to create a new list consisting of
   alternating elements of the two input lists (so first from
   the first list, then from the second, then from the first
   again, etc.). So p1 [1,3,5] [2,4,6] should produce [1,2,3,4,5,6]. -}
p5 :: [a] -> [a] -> [a]
p5 [] ys = ys
p5 (x : xs) ys = x : p5 ys xs

----------------------------------------------------------------------
-- Operations on trees
--
-- These problems are to help get you used to programming with
-- user-declared datatypes.  Some of these require recursion, while
-- others do not.
----------------------------------------------------------------------

-- here is a datatype for binary trees with data at the nodes
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show , Eq)

-- inject a piece of data as a Node whose left and right subtrees
-- are leaves.  So p6 3 should return Node 3 Leaf Leaf.
p6 :: a -> Tree a
p6 x = Node x Leaf Leaf

-- count the number of elements (i.e., the number of uses of Node)
p7 :: Tree a -> Int
p7 Leaf = 0
p7 (Node _ l r) = 1 + p7 l + p7 r

{- given an element and a tree, build a new tree with that
   element at the root and the tree as both the left and right
   subtree.  So p8 3 (Node 1 Leaf Leaf) would return
   Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf). -}
p8 :: a -> Tree a -> Tree a
p8 x t = Node x t t

{- return the elements of the tree using an in-order traversal.  This
   means that for any Node, you should list the elements of the left
   subtree, then the data at that Node, and then the elements of the
   right subtree.

   See, for example,
   https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/ -}
p9 :: Tree a -> [a]
p9 Leaf = []
p9 (Node x l r) = p9 l ++ [x] ++ p9 r

-- do the same thing as for p9, but use a post-order traveral. 
p10 :: Tree a -> [a]
p10 Leaf = []
p10 (Node x l r) = p10 l ++ p10 r ++ [x]

{- apply the given function to all the data in the input tree,
   constructing a new tree with the same structure (but holding
   the outputs of the function). -}
p11 :: (a -> b) -> Tree a -> Tree b
p11 f Leaf = Leaf
p11 f (Node x l r) = Node (f x) (p11 f l) (p11 f r)

-- recursively flip left and right subtrees throughout the tree
p12 :: Tree a -> Tree a
p12 Leaf = Leaf
p12 (Node x l r) = Node x (p12 r) (p12 l)

-- return the root of the tree if it has one
p13 :: Tree a -> Maybe a
p13 (Node x _ _ ) = Just x
p13 _ = Nothing

{---------------------------------------------------------------------
 A more complex datatype

 The following code is taken from
 http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html

 JSON is a textual format commonly used for exchanging structured data between
 clients and servers in web applications.  There are just a few forms of data
 allowed.  

 A tiny explanation is given below with p14.

 See various online references for a tutorial if needed, like
     https://www.w3schools.com/js/js_json_intro.asp
----------------------------------------------------------------------}

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord)

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

instance Show JValue where
  show = renderJValue

{- write the JSON value

     { "id" : 51 ,
       "public" : false ,
       "JobPairs" : [ { "id" : 31 , "solverid" : 1 , "benchid" : 251 } ,
                      { "id" : 32 , "solverid" : 1 , "benchid" : 58  } ,
                      { "id" : 33 , "solverid" : 2 , "benchid" : 251  } ] }

   as a value of type JValue.

   Square brackets are for arrays, curly brackets are for objects.

   I found it handy to write a helper function for creating the JSON for
   a job pair like { "id" : 33 , "solverid" : 2 , "benchid" : 251  }.
   Then I can call this helper function three times, rather than write out
   three rather bulky JValue expressions directly.
   -}
makePair :: Double -> Double -> Double -> JValue
makePair id sid bid = JObject [ ("id" , JNumber id), ("sid" , JNumber sid) , ("bid" , JNumber bid)]

p14 :: JValue
p14 = JObject [ ("id" , JNumber 51), ("public", JBool False) ,
                ("JobPairs" , JArray [ makePair 31 1 251 , makePair 32 1 58 , makePair 33 2 251 ])]

-- return True if the input JValue is JNull, and False otherwise
p15 :: JValue -> Bool
p15 JNull = True
p15 _ = False

{- return the list of attribute-value pairs if the input JValue is a JObject. -}   
p16 :: JValue -> Maybe [(String , JValue)]
p16 (JObject avs) = Just avs
p16 _ = Nothing

{- replace JString "null" everywhere you find it with JNull (do not try to
   replace attributes of JObjects like in { "null" : 3 }) -}
p17 :: JValue -> JValue
p17 (JString "null") = JNull
p17 (JString x) = JString x
p17 (JBool f) = JBool f
p17 JNull = JNull
p17 (JObject avs) = JObject $ map (\ (s,v) -> (s,p17 v)) avs
p17 (JArray vs) = JArray $ map p17 vs

{- return a list of all the values v for which the attribute-value pair (s,v)
   occurs in the JValue, where s is the given input String. -}
p18 :: String -> JValue -> [JValue]
p18 s (JObject avs) = (map snd $ filter (\ (s',_) -> s == s') avs) ++ concat (map (\ (_,v) -> p18 s v) avs)
p18 s (JArray vs) = concat $ map (p18 s) vs
p18 s x = []

