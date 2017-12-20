import Data.List
import Data.Function (on)

{-

I use par. I like it. It's also really old with a funny UI and bugs/behavior I'd
like to fix. I was also intrigued by its documentation, which states that it was
borne from the understanding that making nice line breaks is a dynamic
programming exercise. I had no clue how to think of it as a dynamic programming
excercise.

So let's find out. Then we can rewrite par.

Help in this undertaking can be had at http://xxyxyz.org/line-breaking/.

The first step is understanding what the problem even is. I admit I struggled
with this for a bit, in part because of my misremembering of how dynamic
programming works. (When viewed through that broken lens, the problem was
insurmountable.) Ignoring dynamic programming for a bit, the task of line
breaking is to pick a set of break points, each of which are between two words
of the input. When the input has n words, there are 2^(n-1) possible sets of
break points: all the possible ways of choosing which words end an inner line.

Choosing one set over another is done by measuring the raggedness of each line.
The sum of raggedness is the measure: less raggedness is better. We could simply
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

So we'll use the square of the difference instead, giving 1226 and 724. Much
better. Oh, and lines that are *longer* than the target length basically cost
infinity.

Now we can brute force it.

-}

-- | Given input list of words, generate all possible line break solutions
allSolns :: [String] -> [[String]]
allSolns [] = [[]]
allSolns ws =
    let is = tail $ inits ws
        ts = tail $ tails ws
        firstLines = map unwords is
        subSolns = map allSolns ts
    in  concat $ zipWith (\f ss -> map ([f]++) ss) firstLines subSolns

-- | The cost of a total solution given a target length. It is standard to
-- ignore the cost of the last line.
cost :: Int -> [String] -> Int
cost t ls | any ((>t) . length) ls = maxBound
          | otherwise = maybe 0 (subCost t) (initMay ls)
    where initMay [] = Nothing
          initMay xs@(_:_) = Just (init xs)

-- | The cost of a subsolution. In this case we don't discount the cost of the
-- last line.
subCost :: Int -> [String] -> Int
subCost t ls | any ((>t) . length) ls = maxBound
             | otherwise = sum (map ((^2) . (t-) . length) ls)


bruteForce target ws = minimumBy (compare `on` cost target) (allSolns ws)

{-

Hooray, we have an algorithm that is already having a hard time with an input of
15 1-letter words. Of course, my constant factor is probably also pretty bad,
but in the face of O(2^N) does it really matter? No.

Ok, enough of powersets. The first thing to do is recognize that this problem
has optimal substructure. This is where my misremembrance of dynamic programming
comes to bear. The bad phrase that I had remembered was, "Assume you have found
the optimal solutions for the first N-1 subproblems. Then, find the optimal
subproblem for the last problem, and you're done."

The real way to do it is to repeat all of that *for each possible 'last'
subproblem*, and then choose the best overall solution.

Thus, the first step is to find the set of possible last subproblems. In our
case, that means finding the last break point, assuming we've found all the
others already. That last break could be at any one of the possible locations,
so we have n-1 possible last subproblems.

Well, let's back up. The *zeroth* step is to recognize optimal substructure.

Assume we have an optimal solution. Then, removing the last line break and all
the words that follow would maintain the optimality. We knwo this because if we
could find a more optimal subsolution, we could then add on the last line again
and create a better total solution, contradicting our original assumption.

Now we see why it's safe to assume that we have found the optimal subsolution to
the first n-1 subproblems. That bit of the total solution won't change. We can
work from that assumption to find the optimal solution for the remainder without
impacting the validity of the part we assumed.

We still have to move from *assuming* we've found the subsolution to *actually
finding it*, but we begin to see how that's possible by stripping away the last
step and recursing on the algorithm.

First, let's rewrite the brute force method to illustrate the recursion.
-}
