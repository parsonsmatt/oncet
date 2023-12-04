# oncet

This package provides a type `OnceT` which allows you to define recipes where actions are performed *lazily* - that is, when you actually demand the result of those actions.

For example, consider this program:

```haskell
produceInt :: OnceT IO Int
produceInt = do
    declare do
        putStrLn "Making an Int"
        pure 3

produceString :: OnceT IO String
produceString = do
    declare do
        putStrLn "Making a String"
        pure "hello"

produceTuple :: OnceT IO (Int, String)
produceTuple = do
    int <- produceInt
    string <- produceString
    pure (int, string)
```

This program describes a recipe for producing an `Int` and a `String`.
If we run this program, then we will perform the side-effects *lazily* - as we demand the structure.

```haskell
main = do
    (i, s) <- runOnceT produceTuple
    print i
    print s
```

`runOnceT` uses `NFData` to evaluate the result, and so we'll run the effects in the order determined there.

However, we may not need everything from the recipe.
We may only want a subset of the data.

```haskell
main = do
    s <- runOnceT do
            (_i, s) <- produceTuple
            pure s
    print s
```

We never actually *demand* the `Int` here, and as a result, we don't run the side effects that would be necessary to produce it.
