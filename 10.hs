import Data.List as List
-- global defs
name = "Problem 10"

-- prototype
encode :: (Eq a) => [a] -> [(Int, a)]

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
test2=test_suite_expect "Single Element" encode ["a"] [[(1, 'a')]]
test3=test_suite_assert "Seq-Compressed" encode ["aabbcc"] [[(2, 'a'), (2, 'b'), (2, 'c')]]
test4=test_suite_assert "Sep-Compressed" encode ["aabbaaadddd"] [[(2, 'a'), (2, 'b'), (3, 'a'), (4, 'd')]]
test5=test_suite_expect "Not/Partial-Compressed" encode ["ssffffgtt"] [[(2, 's'), (4, 'f'), (1, 'g'), (2, 't')]]
test_suites = [test2,test3,test4,test5]

-- wrapper
main = do
    putStrLn $ name ++ " Start"
    test_print_all test_suites
    putStrLn $ name ++ " End"
    putStrLn "=================="

-- impl
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack ls = foldr (\x acc -> if x /= (head $ head acc) then [x]:acc else (x:(head acc)):(tail acc)) [[last ls]] $ init ls

encode ls = zipWith (,) (map length $ pack ls) (map head $ pack ls)
