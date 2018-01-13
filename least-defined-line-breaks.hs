import Data.List
import Data.Function (on, fix)
import Data.Function.Memoize

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
_       .+. MaxCost = MaxCost
MaxCost .+. _       = MaxCost

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

The last step to dynamic programming is to reuse previous results. If we search
forward from smaller results, the optimal substructure will ensure that we
continuously reach for the smaller results as we build up larger ones.

Let's build an algorithm that shows this. Rather than building all possible
solutions and finding their costs, we'll find solutions as we progress through
larger and larger subsolutions

-}

optimalSubSolns :: Int -> [String] -> [String]
optimalSubSolns _ []  = []
optimalSubSolns t wss = memoFix f wss
  where
    f _ [] = []
    f step ws =
        let n         = length ws
            is        = take n $ inits ws
            ts        = take n $ tails ws
            subSolns  = map step is
            lastLines = map unwords ts
            combine (ss, l) = let qwe = ss ++ [l] in (ordinarySumCost t qwe, qwe)
        in  snd
            . minimumBy (compare `on` fst)
            . map combine
            . zip subSolns
            $ lastLines
