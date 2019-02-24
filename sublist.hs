module Sublists where

samplelist = [5,3,4,5,1,2,7,6,8]

largestIncSub' :: (Enum a, Eq a) => [a] -> [a] -> [a] -> [a]
largestIncSub' best cand [] = if length cand > length best   -- terminal case
                                then cand
                                else best
largestIncSub' best [] x = largestIncSub' best [head x] (tail x)
largestIncSub' best cand x = if length cand > length best
                                then largestIncSub' cand cand x
                                else if succ (last cand) == head x 
                                    then largestIncSub' best (cand ++ [head x]) (tail x)
                                    else largestIncSub' best [] x

largestIncSub :: (Enum a, Eq a) => [a] -> [a]
largestIncSub x = largestIncSub' [] [] x