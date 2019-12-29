-- | Take two lists and zip them together, that is, make a new list of ordered
-- pairs constructed from sequential items of the two lists, BUT:
-- if an element in the first list satisfies a given @test@, make a pair of that
-- element and a sustitute element (@sub@) instead of the corresponding
-- element from the second list.
zipFill [] _ test sub = [] 
    -- ^ stop when the first list is done
zipFill a [] test sub = zipFill a [sub] test sub
    -- ^ when the second list is done, use @sub@ as placeholder for remaining
    -- items in first list
zipFill (a:as) (b:bs) test sub = 
    if test a 
        then (a, b) : zipFill as bs test sub 
        else (a, sub) : zipFill as (b:bs) test sub
    -- ^ build a list of pairs of either the heads of both lists or the head
    -- of the first list and the @sub@ value



