# A function monad tutorial.

Warning: this is just a theoretical exercise, not at all necessary for understanding the core code in this repo.

### Explanation of `convertToNative`

In the code this function is used: 

    convertToNative = flip maybe id <*> flip lookup natives

and explanation was promised. In a simple, not point-free way, the function can be written as 

    convertToNative name = maybe name id (lookup name natives)

and it searches the dictionary `natives` for a `name` as the key and returns the value if there, and if not, returns itself.

The interesting part in this tutorial is the usage of operator `<*>`, defined as `(<*>) = ap`. `ap` has the type 

    ap :: Monad m => m (a -> b) -> m a -> m b
    
Let's annotate all the other constituents too:

    maybe :: y -> (x -> y) -> Maybe x -> y
    lookup :: Eq p => p -> [(p, a)] -> Maybe a
    
And, informally, the two expressions:

    flip maybe id :: m (a -> b) === y -> (Maybe y -> y) === default -> Maybe result -> (result or default)
    flip lookup natives :: m a === p -> Maybe a

You may soon see what `m (a->b)` and `m a` are above. But the glue here is `ap`, and it glues two functions together somehow. The function `ap`, which seems to stand for "apply", takes a mapping function `a->b` wrapped in a monad and a monadic value `a` that the function would map over, producing the same monad with the mapped value `b`. It is defined like so:

    ap m1 m2 = do { f <- m1; x <- m2; return (f x) }
    
For a more intuitive monad, `Maybe`, an example usage could look like this:

    Just (+4) `ap` Just 7 

Which would result in `11`. But...


### How is a function a monad?

A function is a monad, and is defined as: `Monad ((->) r)`, where `r` is the function's return value.
It helps to imagine it as a data type `data Func ret arg = Func ret arg` and think of it as `Monad (Func r)`. 

What does the the `bind` function do for functions then?
Let's take a simple example: `let z f = do { x <- f; return x }`.
Here `z` is a function that takes a monadic param `f`, binds it with `<-` to `x`,
and then wraps it into the same monad. So `z` is `\_->x` aka `const`: it extracts the "future" return value from `f`, leaving an unapplied slot open for `f`'s argument. Therfore `z` is equivalent to its argument `f`. Precisely, `z f x = const (f x) x`.

The do notation for the function monad takes each binding
and applies the same parameter to it. In a slightly more complex example,

    let z f g h = do { a <- f; b <- g; c <- h; return [a,b,c] }` 

`z` is same as: `z f g h x = [f x, g x, h x]`. 

Knowing this, we can monadically describe what `ap` does: it takes a function 
with the return value of type `(a->b)` and a function with the
return value of type `b` and returns another function with the return
value of type `b`. For example, given: `f x a = show x ++ ": " ++ show a` and  `g x = show x;`, `ap f g` would be the same as:

```haskell
     fg p = result where -- e.g. p = 77
       x1 = f p -- :: a -> String; f a = "77: " ++ show a
       x2 = g p -- :: String = "77"
       result = x1 x2 -- :: String = "77: "++show "77" = "77: \"77\""
     fg 77
```

Function `f` returns `a->b` and `g` returns `a`, `ap` applies the second to the first, and the result is same as (f x) (g x), but we didn't have to repeat the `x` twice, which is exactly what we did in the plain version of the `convertToNative` function. There, `f` returns the first argument, if the second one is `Nothing`, and `g` returns a value wrapped in `Just` or `Nothing`, if not found.

That's it.
