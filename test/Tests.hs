module Tests where

import Test.HUnit

foo n = (1,2)
partA :: Integer -> IO (Integer, Integer)
partA n = return (5, 5)
partB :: Integer -> IO Bool
partB n = return True

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)