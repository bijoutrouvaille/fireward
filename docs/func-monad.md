# A function monad tutorial. 
### Explanation of convertToNative

`convertToNative = flip maybe id `ap` flip lookup natives`
Because `<*>` is defined as `(<*>) = ap`, therefore,
a pointfree version of 
    convertToNative name = maybe name id (lookup name natives)
    maybe :: y -> (x -> y) -> Maybe x -> y
    lookup :: Eq p => p -> [(p, a)] -> Maybe a
    ap :: Monad m => m (a -> b) -> m a -> m b
    ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
    flip maybe id :: m (a -> b) === y -> Maybe x -> y === Maybe res -> (res or default name)
    flip lookup natives :: m a === p -> Maybe a

### A how is a function a monad?
A function is a monad, defined as: `Monad ((->) r)`
think `Monad (Func r)` if `data Func ret arg = Func ret arg`. 
Here, then, r stands for the return value.
What does the the bind operator do for functions?
Simple example: `let z = do { x <- f; return x }`
here `z` is a function that takes a param, binds it with `<-` to `x`
and then converts it into a new monad, which is `\_->x`. Therfore
`z` is equivalent to `f`. Precisely, `z x = const (f x) x`.
The do notation for the function monad takes each binding
and applies the same parameter to each. For a more complex example:
`let z = do { a <- f; b <- g; c <- h; return [a,b,c] }`
Here `z` is same as: `z x = [f x, g x, h x]`.

Now we can know what ap does: it takes a function 
with the return value of `(a->b)` and a function with the
return value of b and returns a function with the return
value of b. For example:
given: `f x a = show x++": "++show a; g x = show x;`
we have `ap f g === f `ap` g === fg :: a -> Int; fg p = (g p)`
or in longer terms:

     fg p = result where -- with p = 77
     x1 = f p -- :: a -> String = f a = "77: "++show a
     x2 = g p -- :: String = "77"
     result = x1 x2 -- :: String = "77: "++show "77" = "77: \"77\""

Hope this clarifies things
