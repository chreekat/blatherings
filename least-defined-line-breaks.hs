{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- just for documentationG
{-# LANGUAGE PackageImports #-}
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function (on, fix)
import Data.List
import Data.Map (Map, (!))
import Data.Semigroup
import Data.Set (Set)
import "pretty-simple" Debug.Pretty.Simple
import "pretty-simple" Text.Pretty.Simple
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import qualified Data.Set as S

{-

I use par. I like it. It's also really old with a funny UI and bugs/behavior I'd
like to fix. I was also intrigued by its documentation, which states that it was
borne from the understanding that making nice line breaks is a dynamic
programming exercise. I had no clue how to think of it as a dynamic programming
excercise.

So let's find out. Then we can rewrite par.

Help in this undertaking can be had at http://xxyxyz.org/line-breaking/ .

The first step is understanding what the problem even is. I admit I struggled
with this for a bit, in part because of my misremembering of how dynamic
programming works. (When viewed through that broken lens, the problem was
insurmountable.) Ignoring dynamic programming, the task of line breaking is to
pick a set of break points, each of which are between two words of the input.
When the input has n words, there are O(2^n) possible sets of break points: all
the possible ways of choosing which words end a line.

Choosing one set over another is done by measuring the raggedness of each line.
The sum of raggedness is the measure, and less is better. We could simply
subtract the length of a line from the target length to measure raggedness, but
then we'd have a situation where

    |these                                   | 35
    |two lines of totally unequal raggedness |  1
                                               --
                                               36

have a slightly smaller sum than

    |these two lines with                    | 20
    |more equal raggedness.                  | 18
                                               --
                                               38

So we'll use the square of the difference instead, giving 1226 and 724 for those
same examples. Much better. Oh, and lines that are *longer* than the target
length basically cost infinity.

There is one complication: At the top level, if the last line is shorter than
<target>, it has no (zero) cost. Otherwise, it causes the solution to have
maxBound cost.

Thus, we need to keep track of the cost of the last line for a solution, in
order to ignore it at the top level.

-}

data Cost = Cost Int | MaxCost
    deriving (Eq,Show,Ord)

-- | Add two costs
(.+.) :: Cost -> Cost -> Cost
Cost x  .+. Cost y  = Cost (x + y)
_       .+. _       = MaxCost

-- | Cost of a single line
cost :: Int -> String -> Cost
cost t s | length s > t = MaxCost
         | otherwise    = Cost ((t - length s) ^ 2)

-- | Cost of multiple lines
ordinarySumCost :: Int -> [String] -> Cost
ordinarySumCost t = foldr (.+.) (Cost 0) . map (cost t)

-- | Find the total cost of a solution, minding the tricky last line
specialSumCost :: Int -> [String] -> Cost
specialSumCost t = accumCost (Cost 0)
  where
    accumCost acc [] = acc
    accumCost acc [l] = case cost t l of
        MaxCost -> MaxCost
        _ -> acc
    accumCost acc (l1:l2:ls) = accumCost (acc .+. cost t l1) (l2:ls)

{-

Now we can brute force it.

-}

-- | Given input list of words, generate all possible line break solutions
allSolns :: [String] -> [[String]]
allSolns [] = [[]]
allSolns ws =
  let n = length ws
      is         = take n $ inits ws
      ts         = take n $ tails ws
      subSolns   = map allSolns is
      lastLines = map unwords ts
  in  concat $ zipWith (\ss l -> map (++ [l]) ss) subSolns lastLines

bruteForce target ws = minimumBy (compare `on` specialSumCost target) (allSolns ws)

{-

Hooray, we have an algorithm that is already having a hard time with an input of
15 1-letter words. Of course, my constant factor is probably also pretty bad,
but in the face of O(2^N) does it really matter? No.

Ok, enough of powersets. The first thing to do is recognize that this problem
has optimal substructure.

Assume we have an optimal solution; i.e., a set of break points. Then, removing
the last line break and all the words that follow would maintain the optimality.
We know this because if we could find a more optimal subsolution, we could then
add on the last line again and create a better total solution, contradicting our
original assumption.

The fact that one can remove the last step without affecting the subsolution is
how we recognize optimal substructure. With that in hand, we can use dynamic
programming.

This is where my misremembrance of dynamic programming can be cleared up. The
bad phrase that I had remembered was, "Assume you have found the optimal
solutions for the first N-1 subproblems. Then, find the optimal solution for the
last problem, and you're done."

The real way to do it is to repeat all of that process *for each possible 'last'
subproblem*, and then choose the best overall solution.

EDIT!

Turns out I was correct all along! The problem is that I am still not properly
seeing how to formulate the problem such that it has optimal substructure. My
"improvement" over my old memory was merely an attempt to make this formulation.

Here's another attempt at describing the problem with its proper substructure:

The minimum raggedness for a text with N words is the subsolution for the first
M-1 words plus the cost of the line comprised of words M..N, where M \elem
[1..N] is chosen to minimize the overall cost. That is, for word N, you must
search all M \elem [1..N] and choose the one that has the best score.

That's the last step. Since we have to do it for each word index w \elem [1..N],
the algorithmic complexity is O(N^2). (We can improve on this, later.)

I know, that's still a terrible description. I'll work on it.

-}

-- | A Break is the index of the last word of a line. I.e. a 0 would mean break
-- after the first word.
newtype Break = Break { unBreak :: Int }
    deriving (Eq, Ord, Num, Show)

data Soln = S { solnCost :: Cost, solnSoln :: [Break] }
    deriving Show

unshift :: [a] -> a -> [a]
unshift xs x = xs ++ [x]

minimumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)

-- | Implement slice on lists. Sorry.
slice :: Int -> Int -> [a] -> [a]
slice i n = take n . drop i

-- | Slice an inclusive segment from a list.
segment :: Int -> Int -> [a] -> [a]
segment s e = slice s (e - s + 1)

-- | Just return each solution. We won't try to get tricky with the last line
-- yet.
--optimalSubSolns :: Int -> [String] -> [Soln]
--optimalSubSolns len [] = []
optimalSubSolns len (N.fromList -> xs) =
    let n = N.length xs
        -- | slacks[i,j] is how much slack space exists for a line that starts
        -- at word i and ends at word j, inclusive
        slacks :: Map (Int, Int) Int
        slacks =
            M.fromList
                [ ((i,j), slack)
                | i <- [0..n-1]
                , j <- [i..n-1]
                , let slack = len - (length (unwords (segment i j (N.toList xs))))
                ]

        -- | Mapping slacks to costs.
        costs :: Map (Int, Int) Cost
        costs = fmap costfn slacks
            where costfn n | n < 0     = MaxCost
                           | otherwise = Cost (n ^ 2)

        -- | Solve given previous solutions
        -- solve :: [Soln] -> Int -> [Soln]
        solve nxt (Break j)
            | j == (-1) = [S (Cost 0) []]
            | otherwise =
                let -- | A miracle occurs
                    previousSolns = nxt (Break (j-1))
                    -- | These are the possible lines following the possible last
                    -- breaks
                    someLines = S.fromList [(i,j) | i <- [0..j]]
                    -- | These are the costs for those possible lines
                    someCosts = M.toList (M.restrictKeys costs someLines)
                    -- Pair up the costs and previous solns so we can pick one
                    pairs = zipWith combine someCosts previousSolns
                    -- Given a previous solution and the line that would come
                    -- after it, create a new solution that adds the line.
                    combine (_, c) (S c' bs) = S (c' .+. c) (bs ++ [Break j])

                    best = minimumOn solnCost pairs
                in -- pTraceShow
                   --  ("prevSolns", previousSolns, "someCosts", someCosts, "pairs", pairs, "best", best) $
                    previousSolns ++ [best]
    in fix solve (Break (n-1))

{-

Hooray!! Thanks to laziness, this is already a zillion times faster than the
brute force method. But it's still VERY slow. Time to start profiling!

-}


wrap :: [Break] -> [String] -> [String]
wrap [] ws = [unwords ws]
wrap (Break b:bs) ws =
    let (unwords -> line, rest) = splitAt b ws
        decr x (Break y) = Break (y - x)
    in line : wrap (map (decr b) bs) rest

main :: IO ()
main =
    let
        -- txt = words
        --     $ "My favorite text is usually comprised of many letters and words, given that this is how you comprise text."
        --     ++ " My favorite text is usually comprised of many letters and words, given that this is how you comprise text."
        --     ++ " My favorite text is usually comprised of many letters and words, given that this is how you comprise text."
        --     ++ " My favorite text is usually comprised of mmmmmmmany letters and words, given that this is how you comprise text."
        txt = replicate 800 "aaa"
    in putStrLn $ unlines $ flip wrap txt $ solnSoln $ last $ optimalSubSolns 30 $ txt
