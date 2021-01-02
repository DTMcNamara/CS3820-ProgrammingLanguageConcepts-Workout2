module PublicTests where

import Control.Applicative
import Solution

-- test x y n means to run test number n, to check that x equals y.
test :: (Show a , Eq a) => a -> a -> Int -> IO ()
test x y n =
  let name = "test for p" ++ show n in
  if x == y then
    putStrLn ("Passing " ++ name ++ ".")
  else
    putStrLn ("Failing " ++ name ++ ": expected " ++ show y ++ ", computed " ++ show x)

main :: IO ()
main =
  do
    putStrLn ("Executing " ++ show (length tests) ++ " tests.")
    sequence_ (zipWith id tests [1..])
  where
    tests = 
      [

        test (p1 [1]) [] ,
        test (p2 [1]) [1],
        test (p3 "a1") "aa11",
        test (p4 [0,0,1]) 1,
        test (p5 [1,3,4] [2]) [1,2,3,4],
        test (p6 0) (Node 0 Leaf Leaf),
        test (p7 (Node 3 Leaf (Node 5 (Node 6 Leaf Leaf) Leaf))) 3,
        test (p8 3 Leaf) (Node 3 Leaf Leaf),
        test (p9  (Node 1 (Leaf) (Node 5 Leaf Leaf))) [1,5],
        test (p10 (Node 1 (Leaf) (Node 5 Leaf Leaf))) [5,1],
        test (p11 not (Node True Leaf Leaf)) (Node False Leaf Leaf),
        test (p12 (Node 1 Leaf (Node 4 Leaf Leaf)))
             (Node 1 (Node 4 Leaf Leaf) Leaf),
        test (p13 (Node 1 Leaf Leaf)) (Just 1),
        test (show p14) "{\"id\": 51.0, \"public\": false, \"JobPairs\": [{\"id\": 31.0, \"sid\": 1.0, \"bid\": 251.0}, {\"id\": 32.0, \"sid\": 1.0, \"bid\": 58.0}, {\"id\": 33.0, \"sid\": 2.0, \"bid\": 251.0}]}",
        test (p15 (JNull)) True ,
        test (p16 (JBool True)) (Nothing),
        test (p17 (JArray [ JObject [ ("val" , JString "nullo") ] , JString "null", JBool True]))
             (JArray [ JObject [ ("val" , JString "nullo") ] , JNull, JBool True]),
        test (p18 "val" (JArray [ JObject [ ("val" , JNull) ], JObject [ ("val" , JNumber 3) ] , JObject [ ("nested" , JObject [ ("val" , JArray [ JBool True , JBool False])])], JNull, JBool True]))
             [ JNull, JNumber 3 , JArray [ JBool True , JBool False ]]

      ]


      
