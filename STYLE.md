# Haskell Style guide for Waskell

## Acknowledgements

As there’s no “canonical” style guide for Haskell, this code style has been
inspired from a few online resources, including the style guide for the
[Snap framework](http://snapframework.com/docs/style-guide"),
[this style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md),
[this other style guide](http://www.cs.caltech.edu/courses/cs11/material/haskell/misc/haskell_style_guide.html),
and where it was all mashed together, [google Ganeti's style](http://www.cs.caltech.edu/courses/cs11/material/haskell/misc/haskell_style_guide.html).  

## Guide

### Git style

#### Commits
Commits titles should start by a capital letter, no full stop and in present
tense under 68 characters. It can include a description (skip a line) that
explains in much details the changes made in that commit.

#### Rebase
Use `git rebase -i` if you feel like 2 or more of your commits should be merged
together (like having to push to check the `.md` files display properly).  
Try and avoid merging branches by using `git pull --rebase` instead of 
`git pull`.

### Documentation
Follow the [Haddock](https://www.haskell.org/haddock/) style for writing 
documentation. All functions called outside the module should have
documentation, tests should however be clear enough in their naming to avoid
writing documentation.  
The global **testing** should take place in the `src-test` folder, however 
we'll be using [Doctest](https://github.com/sol/doctest#readme) as a way to
enforce up to date documentation, make it clearer for everyone and as a quicker
way to fix small development objectives inside a module.  

### Files
Files must be `.hs` non-literate begining with haddock documentation:  
```haskell
{-| Short module summary.
Longer module description.

-}

{-

Copyright (C) ...

This program is free software ...

-}
```  

All module-level pragmas should be placed on top of the file, above the
description.  

### Imports
Imports should be placed in group, each sorted alphabetically in this order:  
  * Haskell standard library imports
  * External libraries imports
  * Internal libraries imports  


It is *allowed* to use qualified imports for standard library and internal
libraries.

### Indentation
  * Use only spaces, never tabs. Indentation level is 2 characters. For Emacs, 
this means setting the variable haskell-indent-offset to 2.
  * Line length should be at most 80 chars long including comments.
  * Use indentation-based structure, and not braces/semicolons. 

>
**Note** for `if-then-else` structures both in `do` and normal blocks should 
be:  
```haskell
if condition
  then expr1
  else expr2
```
>

  * Multi-line functions should be indented that way:  

```haskell
id x = 
  let y = succ x
  in  pred y
```

  * Single line functions can be written that way:  
`f x = x + 1`  

### Multi-line strings
Multi-line strings should be written with a backslash at the end of the closing
line and a second backslash at the beginning of the new line aligned with the
original opening quotes.  
```haskell
longString :: String
longString = "This is a very very very long string that\
             \ needs to be split in two lines"
```  

### Data declaration
When declaring either data types, or using list literals, etc., the columns
should be aligned, and for lists use a comma at the start of the line, not at
the end. You must wrap the first element (newline after `=`).  
```haskell
data OpCode =
      OpStartupInstance ...
    | OpShutdownInstance ...
    | ...

data Node =
  Node { name :: String
       , ip   :: String
       , ...
       }

myList = 
  [ value1
  , value2
  , value3
  ]
```

You can use inline notation for declaring your data elements, with a space
around braces and after the comas.  

```haskell
Tree t = { Leaf 'f', Tree { ... } }

Node n = { ip = "8.8.8.8" }
```

For lists declarations should match prelude's show function output: `[1, 2, 4]`.  

### Lambda functions
Surround binary operators with a space. Do not put a space after `\` in lambda
arguments.  
`foldl (\x y -> ...) [...]`  

### White space
Put new lines between top level declarations, but no new lines between type and 
implementation. 
```haskell
f :: Int 
  -> Int
f x = x + 1

g :: String
g = "Hello"
```

### Naming
Functions should be named in mixedCase style, and types in CamelCase. Function
arguments and local variables should be mixedCase. When using acronyms, ones
longer than 2 characters should be typed capitalised, not fully upper-cased
(e.g. Http, not HTTP).  

For variable names, use descriptive names; it is only allowed to use very short
names (e.g. a, b, i, j, etc.) when:  
  * the function is trivial, e.g.: `sum x y = x + y`
  * we talk about some very specific cases, e.g. iterators or accumulators:
`map (\v -> v + 1) lst`
  * using x:xs for list elements and lists, etc.
In general, short/one-letter names are allowed when we deal with polymorphic
values and should be consistent with the Haskell prelude.  

When reusing names with updated values it is allowed up to three `'` e.g.
`a'' = f a'` or in where clauses. Anything higher than this should be
refactored in a function. 

### Aligning

#### Line too long
Same as for function declaration, break aguments by line e.g.:  
```haskell
function
  (very long args 1)
  (and $
    toooooooooooooooooooooooooooooooooooooooooooo 
    long arg2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  )
```

#### Equals
Do **not** align according to the name but to the equal sign.  
```haskell
let hello    = greetings
     persons = getAllPersons
in ...
```

#### Case _ of
```haskell
case x of
  "hello"    -> putStr "greeting"
  ", world!" -> putStr "else"
```

#### Comments
As seen in the [comments](#comments) section, inline comments should be aligned
to the rightmost element.

#### Where clauses
Where clauses should be placed inline with current indentation e.g.:
```haskell
f x y =
  let x' = add x 1
      y' = add y 1
  in  x'' - y''
  where
    x'' = succ x'
    y'' = succ y'
```

### Exceptions
Do **not** use exceptions, use maybes!

### Point Free
E V E R Y T H I N G   P O I N T   F R E E  
```haskell
-- NO
let a x = f (g (h x))
-- MORE
let a = f . g . h
```

And function composition may follow the same concept:  
```haskell
-- bad
f (g (h x))
-- better
f $ g $ h x
-- G O D 
f . g . h $ x
```

### Extensions
It is recommended to keep the use of extensions to a minimum, so that the code
can be understood even if one is familiar with just Haskel98/Haskell2010. That
said, some extensions are very common and useful, so they are recommended:  
  * Bang patterns: useful when you want to enforce strict evaluation 
(and better than repeated use of seq)
  * CPP: a few modules need this in order to account for configure-time options;
don’t overuse it, since it breaks multi-line strings
  * Template Haskell: we use this for automatically deriving JSON instances and
other similar boiler-plate.  

Such extensions should be declared using the Language pragma:  
```haskell
{-# Language BangPatterns #-}

{-| This is a small module... -}
```

Pragmas that only refer to a function should be placed directly under
(useful for `inline` pragma).  
```haskell
succ a = a + 1
{-# inline succ #-}
```

### Quick Check
If you have big type that takes time to generate and several properties to test
on that, by default 500 of those big instances are generated for each property.
In many cases, it would be sufficient to only generate those 500 instances once
and test all properties on those. To do this, create a property that uses
conjoin to combine several properties into one. Use counterexample to add
expressive error messages. For example:  
```haskell
prop_myMegaProp :: myBigType -> Property
prop_myMegaProp b =
  conjoin
      [ counterexample
          ("Something failed horribly here: " ++ show b) (subProperty1 b)
      , counterexample
          ("Something else failed horribly here: " ++ show b)
          (subProperty2 b)
      , -- more properties here ...
      ]

subProperty1 :: myBigType -> Bool
subProperty1 b = ...

subProperty2 :: myBigType -> Property
subProperty2 b = ...`
```

### Comments
Always use proper sentences; start with a capital letter and use punctuation in
top level comments:  
```haskell
-- | A function that does something.
f :: ...
```

For inline comments, start with a capital letter but no ending punctuation.
Furthermore, align the comments together with a 2-space width from the end of 
the item being commented:  
```haskell
data Maybe a = Nothing  -- ^ Represents empty container
             | Just a   -- ^ Represents a single value
```

The comments should be clear enough so that one doesn’t need to look at the 
code to understand what the item does/is.  

Use `-- |` to write doc strings rather than bare comment with `--`.
