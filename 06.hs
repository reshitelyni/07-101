import Data.List as List
-- global defs
name = "Problem 06"

-- prototype
isPal :: (Eq a) => [a] -> Bool

-- common lib
test_suite_expect :: (Eq b) => String -> (a -> b) -> [a] -> [b] -> IO(String)
test_suite_assert :: (Eq b) => String -> (a -> b) -> [a] -> [b] -> IO(String)
test_print_all :: [IO(String)] -> IO()

test_all::(Eq b) => (a -> b) -> [a] -> [b] -> [Bool]
test_all f = zipWith (==) . (fmap f)
expect_all e suite = if (foldl (&&) True e) then name ++ " : " ++ suite ++ " \x1b[40;32mPassed\x1b[0m" else name ++ " : " ++ suite ++ " \x1b[40;31mWrong\x1b[0m"
assert_all::[Bool] -> String -> String
assert_all [] suite = name ++ " : " ++ suite ++ " \x1b[40;32mPassed\x1b[0m"
assert_all (False:xe) suite = name ++ " : " ++ suite ++ " \x1b[40;31mWrong\x1b[0m"
assert_all (True:xe) suite = assert_all xe suite

test_suite :: (Eq b) => String -> ([Bool] -> String -> String) -> (a -> b) -> [a] -> [b] -> IO(String)
test_suite suite judge f input expect = return $ judge (test_all f input expect) suite

test_suite_expect = (flip test_suite) expect_all
test_suite_assert = (flip test_suite) assert_all

test_print :: IO(String) -> IO()
test_print str = do
    name <- str
    putStrLn name

test_print_all_impl :: [IO(String)] -> Int -> Int -> IO()
test_print_all_impl [] _ _ = return ()
test_print_all_impl (s:xs) cur total = do
    name <- s
    putStrLn $ "[" ++ (show cur) ++ "/" ++ (show total) ++ "] " ++ name
    test_print_all_impl xs (cur+1) total

test_print_all ls = test_print_all_impl ls 1 $ length ls

-- tests
test1=test_suite_expect "Special List" isPal ["", "a", "aa"] [True, True, True]
test2=test_suite_expect "List's size over 2" isPal ["bdb", "4r5r4"] [True, True]
test3=test_suite_expect "Not Palindrome" isPal ["da", "fs", "affd", "3ere5"] $ take 4 $ repeat False
test_suites = [test1,test2,test3]

-- wrapper
main = do
    putStrLn $ name ++ " Start"
    test_print_all test_suites
    putStrLn $ name ++ " End"
    putStrLn "=================="

-- impl
isPal [] = True
isPal [_] = True
isPal (f:xs) = if let l = last xs in f == l then (isPal $ init xs) else False
