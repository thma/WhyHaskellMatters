# Proof of functor laws for Maybe


**Facts:**
This is the `Functor` instance declaration of the type `Maybe`:

```haskell
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing       -- (1)
    fmap f (Just a)      = Just (f a)    -- (2)
```

This is the declaration of the composition operator `(.)`:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g x = f (g x)                       -- (3)
```

This is the definition of the Identity function `id`:
```haskell
id :: a -> a
id x =  x                               -- (4)
```


Now we are asked to proof that `Maybe` fulfils the functor laws:
 
```haskell
1.: fmap id = id
2.: fmap (f . g) = (fmap f . fmap g)
```

## Proof of the first law
**Claim:** `fmap id m = id m`, for any `m` of type `Maybe a`.

**Proof.** On cases of `m`.

*Case 1:* `m = Nothing`.

```haskell
fmap id m = fmap id Nothing -- by expansion of m
          = Nothing         -- by applying equation (1)
          = id m            -- by definition m, by applying equation (4)
```

*Case 2:* `m = Just a`.

```haskell
fmap id m = fmap id (Just a) -- by expansion of m
          = Just (id a)      -- by applying equation (2)
          = Just a           -- by expansion of id (equation (4))
          = m                -- by definition of m
          = id m             -- by applying equation (4)
```
Therefore, `fmap id m = id m` in all cases.∎

## Proof of the second law

**Claim:** `fmap (f . g) m = (fmap f . fmap g) m`, for any `m` of type `Maybe a`.

**Proof.** On cases of `m`.

*Case 1:* `m = Nothing`.

```haskell
fmap (f . g) m      = fmap (f . g) Nothing    -- by expansion of m
                    = Nothing                 -- by applying equation (1)
(fmap f . fmap g) m = fmap f (fmap g Nothing) -- by expansion of (.) and m
                    = fmap f Nothing          -- by applying equation (1)
                    = Nothing                 -- by applying equation (1)
```

*Case 2:* `m = Just a`.

```haskell
fmap (f . g) m      = fmap (f . g) (Just a)    -- by expansion of m
                    = Just ((f . g) a)         -- by applying equation (2)
(fmap f . fmap g) m = fmap f (fmap g (Just a)) -- by expansion of (.), m
                    = fmap f (Just (g a))      -- by applying equation (2)
                    = Just (f (g a)            -- by applying equation (2)
                    = Just ((f . g) a)         -- by applying equation (3)
```
Therefore, `fmap (f . g) m = (fmap f . fmap g) m` in all cases. ∎
