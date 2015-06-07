The long reply
==============

On Wednesday, June 3, 2015 at 12:02:17 PM UTC-7, Anthony Cowley wrote:
> 
> On Wed, Jun 3, 2015 at 1:43 PM, Richard Eisenberg <e...@cis.upenn.edu> wrote: 
> > I have to say I'm a big -1 on the proposed syntax -- it's awfully confusing 
> > to have the import list mean something entirely different before the `as` 
> > compared to after the `as`. I proposed a different syntax on the ticket 
> > (https://ghc.haskell.org/trac/ghc/ticket/10478#comment:3), which I paste 
> > here for ease of access. 
> 
> Calling this "awfully confusing" is dramatically overstating things. A 
> quick grep over all of the Stackage package set sources suggests that 
> the "import Foo as F (foo)" syntax is used in less than 0.3% of all 
> import statements. If the rule that the "explicit import list is 
> always imported unqualified" really is too confusing (even though it 
> is the current rule), you could guess wrong every time and only 
> misunderstand three out of a thousand imports you encounter in the 
> wild. 
> 

Feelings on the proposal
------------------------

The most surprising thing to me about the existing import syntax is that you can
import the same module twice. I like that this
proposal does away with the most common use for that. Terser syntax is also a plus,
albeit a lesser one.

I sympathize with Richard Eisenberg, however. While it is true that the rule, "[an] explicit import list is always imported unqualified," would always guide me to choose the right syntax, the fact that I would *need* a rule warrants a -1 from me. If both syntaxes had been available when I started learning Haskell, I might still be trying to keep them straight. It doesn't matter how many examples of one or the other exist in the wild — as I sit down to write a new import line, am I going to have to guess, or look up a rule? I would prefer to do neither.

The syntax of "ln" strikes me as a valid parallel.

Further, it makes the difference between `import Foo (f) as F` and `import Foo as F` very surprising. That's -1 all by itself. I would grimace every time I tried to teach this to a new Haskeller.

Alternate Proposal #1
------------

If someone held a gun to my head and forced me to improve import syntax without breaking existing code, I would suggest modifying the "import qualified" syntax instead:

    import qualified Foo.Quuz.Bar
        as B (Quux, lamb, dah)
        unqualified (Bar, Tweedle, robot)

Even that proposal has weird corner cases like `import qualified Foo.Quuz.Bar as B unqualified`, however. Either that line is allowed, which makes for nonsense, or it is disallowed, which feels arbitrary.

Alternate Proposal #2
-----------

I think trying to force psuedo-English on the syntax is the real problem. I can't think of any way to improve it while keeping compatibility. While it is probably just wishful thinking, I'd rather just see

```haskell
imports
    Data.Text.Format as T
                     as Format (Only, Shown)
                     unqualified (Format)
    Data.Text
        as T
        unqualified (Text)
    Control.Applicative
        unqualified
    Data.Set -- qualified by default!
    Data.Map
        unqualified (Map, foldrWithKey)
    Data.List
        as L
        unqualified

```

Notes:

1. Any number of qualifications per module (maybe a bad idea, but presented for discussion)
2. Same whitespace rules (and reduction to whitespace-independent braces and semicolons) as other syntaxes
3. `unqualified` line must come last
4. The equivalent of `import Data.List as L` is possible, but unweildy

The equivalent stanzas in today's syntax would be

```haskell
import qualified Data.Text.Format as T
import qualified Data.Text.Format as Format (Only, Shown)
import Data.Text.Format (Format)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative
import qualified Data.Set
import Data.Map (Map, foldrWithKey)
import Data.List as L
```

As a final note, my second proposal is *very* similar to my first, and doing away with all the whitespace changes would make them nearly identical. Except for two things:

1. Backwards compatibility is broken by removing the `qualified` keyword
2. The nonsense line `import Data.List as L unqualified` is made illegal by breaking individual import stanzas 
into individual statements
