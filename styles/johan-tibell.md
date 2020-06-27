# Introduction

This file acts both as a presentation of the Floskell formatting
styles, as well as a set of regression tests.

You can see how a particular style will format Haskell source by
reading the matching Markdown file in the styles/ directory.

For regression testing, the canonical source, TEST.md in the root
directory, is parsed and each Haskell code block formatted according
to all predefined styles.  The formatted output is then compared with
the corresponding, already formatted code block in the <style\>.md file
in the styles/ subdirectory.

The regression test will also verify that repeated invocations of the
pretty printer will not modify an already formatted piece of code.

The following code block acts as a quick presentation for the
different formatting styles, by presenting a mixture of common Haskell
constructs.

``` haskell
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Style.Haskell.Example

Haskell Code Style Example.
-}
module Style.Haskell.Example
  (-- * Types
   Enum(..)
  ,Either(..)
  ,Point(..)
   -- * Functions
  ,hello) where

-- Module imports
import qualified Control.Monad.Trans.State
       (State,evalState,execState,get,modify,put,runState)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Prelude hiding (map)

-- Data declarations
data Enum
    = CaseA
    | CaseB
    | CaseC
    deriving (Eq,Enum,Show)

data Either a b
    = Left a
    | Right b
    deriving (Eq,Show)

data Point =
    Point
    { pointX :: Float
    , pointY :: Float
    , pointLabel :: String
    }
    deriving (Eq,Show)

-- Type classes
class Functor f => Applicative a where
    pure :: b -> a b
    ap :: a (b -> c) -> a b -> a c

class Fundep a b | a -> b where
    convert :: a -> b

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap x) = Wrap $ fmap f x

-- Values
origin :: Point
origin =
    Point
    { pointX = 0
    , pointY = 0
    , pointLabel = "Origin"
    }

lorem :: [String]
lorem =
    [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    , "Curabitur nec ante nec mauris ornare suscipit."
    , "In ac vulputate libero."
    , "Duis eget magna non purus imperdiet molestie nec quis mauris."
    , "Praesent blandit quam vel arcu pellentesque, id aliquet turpis faucibus."]

-- Functions
facs :: [Int]
facs = [1, 1] ++ zipWith (+) (tailfacs)

hello :: MonadIO m => m ()
hello = do
    name <- liftIO getLine
    liftIO . putStrLn $ greetings name
  where
    greetings n = "Hello " ++ n ++ "!"

letExpr :: Point -> String
letExp x =
    let y = 1
        z = 2
    in if x > 0
           then y
           else z

ifExpr :: Bool -> Bool
ifExpr b =
    if b == True
        then False
        else True

caseExpr :: [a] -> Maybe a
caseExpr xs = case xs of
    [] -> Nothing
    (x:_) -> Just x

guarded :: Int -> Int
guarded x
  | x == 0 = 1
  | x == 1 = 1
  | otherwise = guarded (x - 2) + guarded (x - 1)

someLongFunctionNameWithALotOfParameters
    :: (MonadIO m, MonadRandom m) => String -> (String -> String) -> m ()
someLongFunctionNameWithALotOfParameters = undefined
```

# Unit Tests

## ModuleHead and ExportSpecList

Without exports

``` haskell
module Main where
```

With exports

``` haskell
module Main (foo,bar,baz,main) where
```

With exports and comments

``` haskell
module Main
  (-- * Main Program
   main
   -- * Functions
  ,foo -- foo function
  ,bar -- bar function
  ,baz -- baz function
  ) where
```

With deprecation

``` haskell
module Main {-# DEPRECATED "no longer supported" #-} where
```

With warnings

``` haskell
module Main {-# WARNING "do not use" #-} where
```

## ImportDecl

``` haskell
import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString (ByteString,pack,unpack)
import qualified Data.ByteString as BS (pack,unpack)
import Control.Monad hiding (forM)
```

## Decl

### TypeDecl

``` haskell
type Name = String

type Pair a = (a, a)

type Fun a b = a -> b
```

### DataDecl and GDataDecl

``` haskell
data Void

data Unit = Unit

data Maybe a
    = Nothing
    | Just a

data Num a => SomeNum = SomeNum a

newtype RWS r w s = RWS (ReaderT r (WriterT w (StateT s Identity)))
    deriving (Functor,Applicative,Monad)

data Enum
    = One   -- Foo
    | Two   -- Bar
    | Three -- Baz

data Foo
    deriving ()

data Foo
    deriving Show

data Foo
    deriving (Show)

data Foo
    deriving (Eq,Ord)

data Expr :: * -> * where
    Const :: Int -> Expr Int
    Plus :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool
    deriving (Show)

data Term a where
    Lit :: { val :: Int
           } -> Term Int
    Succ :: { num :: Term Int
            } -> Term Int
    Pred :: { num :: Term Int
            } -> Term Int
    IsZero :: { arg :: Term Int
              } -> Term Bool
    Pair :: { arg1 :: Term a
            , arg2 :: Term b
            } -> Term (a, b)
    If :: { cnd :: Term Bool
          , tru :: Term a
          , fls :: Term a
          } -> Term a
```

### TypeFamDecl, TypeInsDecl, and ClosedTypeFamDecl

``` haskell
type family Mutable v

type family Mutable v = (r :: *)

type family Mutable v = r | r -> v

type instance Mutable Int = MIntVector

type family Store a where
    Store Bool = [Int]
    Store a = [a]

type family Store a = (r :: *) where
    Store a = [a]

type family Store a = r | r -> a where
    Store a = [a]
```

### DataFamDecl, DataInsDecl, and GDataInsDecl

``` haskell
data family List a

data instance List () = NilList Int

data instance List Char
    = CharNil
    | CharCons Char (List Char)
    deriving (Eq,Ord,Show)

data instance List Int :: * where
    IntNil :: List Int
    IntCons :: Int -> List Int
    deriving (Eq,Ord,Show)

data instance List Int :: * where
    IntNil :: List Int
    IntCons :: { val :: Int
               } -> List Int
    deriving (Eq,Ord,Show)

newtype Penalty = Penalty Int
    deriving (Eq,Ord)
    deriving stock (Read,Show)
    deriving newtype (Num)
    deriving anyclass (FromJSON,ToJSON)
    deriving (Semigroup,Monoid) via M.Sum Int
```

### ClassDecl and InstDecl

``` haskell
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Applicative m => Monad m where
    fail :: m a
    return :: a -> m a
    (>>=) :: a -> (a -> m b) -> m b

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a

class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

instance ToJSON ()

instance Bounded Bool where
    minBound = False

    maxBound = True

instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance Data () where
    type Base = ()

    newtype Wrapped =
        Wrapped
        { unWrap :: ()
        }

    data Expr :: * -> * where
        Const :: Int -> Expr Int
        Plus :: Expr Int -> Expr Int -> Expr Int
        Eq :: Expr Int -> Expr Int -> Expr Bool
```

### DerivDecl

``` haskell
deriving instance Eq a => Eq (Sum a)

deriving instance {-# OVERLAP #-} Eq a => Eq (Sum a)

deriving stock instance {-# OVERLAPS #-} Eq a => Eq (Sum a)

deriving anyclass instance {-# OVERLAPPING #-} Eq a => Eq (Sum a)

deriving newtype instance {-# OVERLAPPABLE #-} Eq a => Eq (Sum a)
```

### InfixDecl

``` haskell
infix 4 ==, /=, <, <=, >, >=

infixr 0 $

infixl !!
```

### DefaultDecl

``` haskell
default ()

default (Integer,Double)
```

### SpliceDecl

``` haskell
$foo

$(bar baz)
```

### TypeSig

``` haskell
id :: a -> a
sort :: Ord a => [a] -> [a]
long :: (IsString a, Monad m)
     => ByteString
     -> ByteString
     -> ByteString
     -> ByteString
     -> ByteString
     -> a
     -> m ()
mktime :: Int -- hours
       -> Int -- minutes
       -> Int -- seconds
       -> Time
transform :: forall a. St -> State St a -> EitherT ServantErr IO a
```

### PatSyn and PatSynSig

``` haskell
{-# LANGUAGE PatternSynonyms #-}

pattern MyJust :: a -> Maybe a
pattern MyJust a = Just a

pattern MyPoint :: Int -> Int -> (Int, Int)
pattern MyPoint {x,y} = (x,y)

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall s <- ErrorCallWithLocation s _
  where
    ErrorCall s = ErrorCallWithLocation s ""

pattern IsTrue :: Show a => a
pattern IsTrue <- ((== "True") . show -> True)

pattern ExNumPat :: () => Show b => b -> T
pattern ExNumPat x = MkT x

pattern Foo, Bar :: Show a => a
```

### FunBind and PatBind

``` haskell
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RecordWildCards #-}

pi = 3.14

id x = x

not False = True
not _ = False

head (x:_) = x

maybe x _ Nothing = x
maybe _ f (Some x) = f x

fst (x,_) = x

fst' (# x,_ #) = x

fstPrism (# x | | #) = Just x
fstPrism (# | _ | #) = Nothing
fstPrism (# | | _ #) = Nothing

empty [] = True
empty _ = False

unSum (Sum {getSum = s}) = s

mag2 Point {x,y} = sqr x + sqr y
mag2 Point {..} = sqr x + sqr y

strict !x = x

irrefutable ~x = x

(//) a b = undefined

a // b = undefined

main = do
    greet "World"
  where
    greet who = putStrLn $ "Hello, " ++ who ++ "!"
```

### ForImp and ForExp

``` haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall sin :: Double -> Double

foreign import ccall "sin" sin :: Double -> Double

foreign import ccall "sin" sin :: Double -> Double

foreign import ccall unsafe exit :: Double -> Double

foreign export ccall callback :: Int -> Int
```

### Pragmas

``` haskell
{-# RULES #-}

{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f . g) xs #-}

{-# RULES "map/append" [2] forall f xs ys. map f (xs ++ ys) =
          map f xs ++ map f ys #-}

{-# DEPRECATED #-}
{-# DEPRECATED foo "use bar instead" #-}
{-# DEPRECATED foo, bar, baz "no longer supported" #-}
{-# WARNING #-}
{-# WARNING foo "use bar instead" #-}
{-# WARNING foo, bar, baz "no longer supported" #-}
{-# INLINE foo #-}
{-# INLINE [3] foo #-}
{-# INLINE [~3] foo #-}
{-# NOINLINE foo #-}
{-# INLINE CONLIKE foo #-}
{-# INLINE CONLIKE [3] foo #-}
{-# SPECIALISE foo :: Int -> Int #-}
{-# SPECIALISE [3] foo :: Int -> Int, Float -> Float #-}
{-# SPECIALISE INLINE foo :: Int -> Int #-}
{-# SPECIALISE NOINLINE foo :: Int -> Int #-}
{-# SPECIALISE instance Foo Int #-}
{-# SPECIALISE instance forall a. (Ord a) => Foo a #-}
{-# ANN foo (Just "Foo") #-}
{-# ANN type Foo (Just "Foo") #-}
{-# ANN module (Just "Foo") #-}
{-# MINIMAL foo | bar, (baz | quux) #-}
```

## Exp

### Var, Con, Lit, Tuple, UnboxedSum, List, and ExpTypeSig

``` haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

foo = foo

foo = Nothing

foo = 123

foo = 'x'

foo = ""

foo = "Lorem Ipsum Dolor Amet Sit"

foo = ()

foo = (1, 2)

foo =
    ( 1 -- the one
    , 2)

foo = (1, )

foo = (, 2)

foo = (, 2, )

foo = (# #)

foo = (# 1, 2 #)

foo =
    (# 1 -- the one
     , 2 #)

foo = (# 1 #)

foo = (# | 1 | | #)

foo =
    (# | 1 -- the one
    | | #)

foo = []

foo = [1]

foo = [1, 2]

foo =
    [ 1 -- the one
    , 2]

foo = 1 :: Int
```

### App, InfixApp, NegApp, LeftSection, RightSection

``` haskell
foo = foldl fn init list

foo =
    foldl
        fn -- reducer
        init -- initial value
        list

foo = 1 + 2

foo = fn `map` list

foo = -3

foo = (+ arg)

foo = (`op` arg)

foo = (arg +)

foo = (arg `op`)
```

### EnumFrom, EnumFromTo, EnumFromThen, EnumFromThenTo, ParArrayFromTo, ParArrayFromThenTo

``` haskell
foo = [1 ..]

foo = [1 .. 10]

foo = [1, 2 ..]

foo = [1, 2 .. 10]

foo = [:1 .. 10:]

foo = [:1, 2 .. 10:]
```

### ListComp, ParComp, and ParArrayComp

``` haskell
{-# LANGUAGE TransformListComp #-}

foo = [(x, y) | x <- xs, y <- ys]

foo =
    [(x, y) -- cartesian product
    | x <- xs -- first list
    , y <- ys -- second list
    ]

foo = [(x, y) | x <- xs | y <- ys]

foo =
    [(x, y) -- zip
    | x <- xs -- first list
    | y <- ys -- second list
    ]

foo = [:(x, y) | x <- xs | y <- ys:]

foo =
    [:(x, y) -- zip
    | x <- xs -- first list
    | y <- ys -- second list
    :]

foo =
    [(x, y) | x <- xs
            , y <- ys
            , then reverse
            , then sortWith by (x + y)
            , then group using permutations
            , then group by (x + y) using groupWith]
```

### RecConstr and RecUpdate

``` haskell
{-# LANGUAGE RecordWildCards #-}

foo =
    Point
    { x = 1
    , y = 2
    }

foo =
    Point
    { x = 1 -- the one
    , y
    , ..
    }

foo =
    bar
    { x = 1
    }

foo =
    bar
    { x = 1 -- the one
    , y
    , ..
    }
```

### Let, If, MultiIf, and Case

``` haskell
{-# LANGUAGE MultiWayIf #-}

foo =
    let x = x
    in x

foo =
    let x = x -- bottom
    in
       -- bottom
       x

foo =
    if null xs
        then None
        else Some $ head xs

foo =
    if null xs -- condition
        then None -- it's empty
        else Some $ head xs -- it's not

foo =
    if
      | null xs -> None
      | otherwise -> Some $ head xs

foo =
    if
      | null xs ->
          -- it's empty
          None
      | otherwise ->
          -- it's not
          Some $ head x

foo = case x of
    True -> False
    False -> True

foo = case xs of
    [] ->
        -- it's empty
        None
    x:_ ->
        -- it's not
        Some x

foo = case xs of
    _
      | null xs -> None
    _ -> Some $ head x
```

### Do and MDo

``` haskell
{-# LANGUAGE RecursiveDo #-}

foo = do
    return ()

foo = do
    return ()

foo = do
    this <- that
    let this' = tail this
    if this -- condition
        then that
        else those

foo = mdo
    return ()
```

### Lambda, LCase

``` haskell
{-# LANGUAGE LambdaCase #-}

foo = \x -> x

foo = \ ~x -> x

foo = \ !x -> x

foo d = \case
    Nothing -> d
    Some x -> x
```

### BracketExp, SpliceExp, QuasiQuote, VarQuote, and TypQuote

``` haskell
{-# LANGUAGE TemplateHaskell #-}

mkDecl :: Q Decl
mkDecl = [d|id x = x|]

mkType :: Q Type
mkType = [t|(a, b) -> a|]

mkPat :: Q Pat
mkPat = [p|(a,b)|]

mkExp :: Q Exp
mkExp = [|a|]

fst :: $(mkType)
fst $(mkPat) = $(mkExp)

html = [html|<p>Lorem Ipsum Dolor Amet Sit</p>|]

foo = mkSomething 'id 'Nothing ''Maybe
```

# Regression Tests

## Do

Before comments and onside indent do not mix well.

``` haskell
foo = do
    -- comment
    some expression
```

## Types

Long types allow linebreaks.

``` haskell
newtype MyMonadT a b m =
    MyMonad
    { runMyMonad
          :: StateT
              ([(a, a -> b)])
              (ReaderT a (ExceptT [IM.IntMap b]) (WriterT [IS.IntSet x] m))
    }
```

## Patterns

Long function pattern matches allow linebreaks.

``` haskell
doThing
    (Constructor field1 field2 field3)
    (Constructor field1 field2 field3)
    (Constructor field1 field2 field3) = undefined
```

## Onside

Indent within onside started on non-empty line should still not stack.

``` haskell
foo =
    if cond
        then do
            this
        else do
            that
```

Before comments at the start of onside do not trigger onside.

``` haskell
foo = do
    -- comment
    some expression
```

Matche arms have individual onside.

``` haskell
foo True =
    some -- comment
        expression
foo False =
    some -- comment
        other
        expression
```

Where binds are considered outside of onside.

``` haskell
foo =
    some -- comment
        expression
  where
    expression = other
```

Align overrides onside.

``` haskell
foo =
    some
        expr
        [ 1 -- comment
        , 2]
```

If-then-else must always indent in do blocks.

``` haskell
foo = do
    if condition -- comment
        then this
        else that
```

Lists must not suppress onside.

``` haskell
foo = case x of
    [y -- comment
        ,z] -> bar

foo = do
    [ x -- comment
        , y]
```

## Comments

Don't be too eager in assigning comments to the following AST node.

``` haskell
data Foo =
    Foo
    { fooBar :: Text
      -- ^A comment, long enough to end up on its own line, or at least I hope so.
    }
    deriving (Eq)
```

Keep comments together and aligned.

``` haskell
-- block
-- one
data Foo
    = Foo  -- some
           -- comments
    | Quux -- more
           -- comments
-- block
-- two
```

... even when haskell-src-exts has weird column span info.

``` haskell
module Main where

-- comment
instance Foo Bar where
    foo = undefined

bar = undefined
```

Only comments.

``` haskell
-- some comment
```

Make sure no comments are dropped from operators or argument.

``` haskell
foo =
    some -- comment 1
    -- comment 2
    %~ -- comment 3
    argument -- comment 4
```

Comments after `where` stay there.

``` haskell
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m (Stream step state) = Stream step1 Nothing
  where
    {-# INLINE_LATE step1 #-}
    step1 _ _ = undefined
```

## Indentation and Line Prefixes

Preserving indentation and line prefixes so that Floskell can be run
on individual declarations and quoted haskell code.

``` haskell
    data Enum
        = One   -- Foo
        | Two   -- Bar
        | Three -- Baz
```

``` haskell
>
>    data Enum
>        = One   -- Foo
>        | Two   -- Bar
>        | Three -- Baz
>
```

# CITI

## Data

``` haskell
data SimpleR
  = SimpleR { srA :: a, srB :: (a -> a)
    , srC :: (forall a. a -> (a, a)), srD :: (forall a b. C a, C2 b => a -> (a, b) -> (a, a))}
  deriving (Show )
```

``` haskell
data Term a :: ((* -> *) -> *) where
  If
    :: { cnd :: Term Bool, tru :: Term a, fls :: Term a } -> Term (a -> (a, a))
  deriving
  ( Show, Eq, Ord)
```

``` haskell
data Expr :: * -> * where
  Const :: Int -> Expr Int
  Plus :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  deriving (Show)
```

## TypeSignatures

``` haskell

functionDefinition :: forall a b c. (C a, B a ~ C b) => C a -> (forall z k. (B k, B k ~ B z) => (forall d. C d => C d -> r) -> C a) -> (r -> a)
functionDefinition = undefined
```

## FunctionBodies

``` haskell
{-# LANGUAGE TypeApplications #-}

functionDefinition = do
                   let res = undefined @z
                       res2 = undefined

                   a <- a :: b
                   pure $ f @z a
```

``` haskell
{-# LANGUAGE TypeApplications #-}

functionDefinition = let
                    res = undefined @z
                    res2 = undefined
                     in do
                      a <- a :: b
                      pure $ f @z a
```

## FunctionDeclarations

``` haskell

functionDefinition avalue anothervalue = undefined
```

``` haskell

functionDefinition (DeSum d v a) anothervalue = undefined
```

``` haskell

functionDefinition R { asd = v } = undefined
```

``` haskell

functionDefinition (DeSum d (R { asd = v }) a) = undefined
```

``` haskell

functionDefinition R { asd = v, aLongerField = aLongerField } = undefined
```

``` haskell

functionDefinition (R { asd = v, aLongerField = aLongerField }) = undefined
```

``` haskell
{-# LANGUAGE ViewPatterns #-}

functionDefinition (unDeSum -> anothervalue) = undefined
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

functionDefinition R { .. } = undefined
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

functionDefinition (R { .. }) = undefined
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

functionDefinition R { asd = v, aLongerField = aLongerField, .. } = undefined
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

functionDefinition (R { asd = v, aLongerField = aLongerField, .. }) = undefined
```

``` haskell
{-# LANGUAGE NamedFieldPuns #-}

functionDefinition R { asd , aLongerField } = undefined
```

``` haskell
{-# LANGUAGE NamedFieldPuns #-}

functionDefinition (R { asd , aLongerField }) = undefined
```

``` haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

functionDefinition R { asd, aLongerField , .. } = undefined
```

``` haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

functionDefinition (R { asd , aLongerField , .. }) = undefined
```

## Constructs

### Case

``` haskell

functionDefinition x
  = case x of y | y > 7 -> undefined
              [y, z] | y > 7 -> undefined
              (y:z) | y > 7 -> undefined
```

### LambdaCase

``` haskell
{-# LANGUAGE LambdaCase #-}

functionDefinition
  = \case
  y | y > 7 -> undefined
  [y, z] | y > 7 -> undefined
  (y:z) | y > 7 -> undefined
```

### MultiWayIf

``` haskell
{-# LANGUAGE MultiWayIf #-}

functionDefinition = if | y > 1 -> undefined
                        | otherwise -> undefined
```

### FunctionAlternatives

``` haskell
{-# LANGUAGE MultiWayIf #-}

functionDefinition (A y) = if
                              | y > 1 -> undefined
                              | otherwise -> undefined
functionDefinition (B y) = case y of
                              y      | y > 7 -> undefined
                              [y, z] | y > 7 -> undefined
                              (y:z)  | y > 7 -> undefined
functionDefinition (C _ _ y) = let d = y + 1 in d + 2
```

``` haskell
{-# LANGUAGE LambdaCase #-}

-- LAME not exported steal them
offsetToInt64
  :: PartitionOffset
  -> Int64
offsetToInt64
  = \case
  PartitionOffsetEnd       -> -1
  PartitionOffset off      -> off
{-# INLINE offsetToInt64 #-}

int64ToOffset
  :: Int64
  -> PartitionOffset
int64ToOffset o
  | o >= 0     = PartitionOffset o
  | otherwise  = PartitionOffsetInvalid
{-# INLINE int64ToOffset #-}
```

### Lists

``` haskell
foo = do
  pure [ x
    , y ]
```

``` haskell
foo = do
  pure [ x -- comment
    , y ]
```

``` haskell
foo = do
  pure [ x {- comment -}
    , y ]
```

``` haskell
foo = do
  pure [ x {- comment -} , y ]
```

``` haskell
foo = let d = [ x -- comment
                , y ]
      in d
```

``` haskell
foo = let d = [ x
                , y ]
      in d
```

``` haskell
foo = let d = [ x
                , y , z]
      in d
```

### ListComprehensions

``` haskell
foo ys = [ (y, y) | y <- ys
      , y > 1 , y `div` 10 == 1
        ]
```

``` haskell
foo = let d ys = [ (y, y) | y <- ys
                , y > 1 , y `div` 10 == 1
                ] in d
```

### ClassDecl and InstDecl

``` haskell

class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = genericToJSON defaultOptions
```

``` haskell

instance Data () where
  type Base = ()
  newtype Wrapped = Wrapped { unWrap :: () }
  data Expr :: * -> * where
    Const :: Int -> Expr Int
    Plus :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool
```

### RecConstr and RecUpdate

``` haskell

foo = Point { x = 1, y = 2 }
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

foo = Point { x = 1 -- the one
            , y
            , ..
            }
```

``` haskell

foo = bar { x = 1 }
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

foo = bar { x = 1 -- the one
          , y
          , ..
          }
```

``` haskell
{-# LANGUAGE RecordWildCards #-}

foo = bar { x = 1 -- the one
          , va = 34
          , y
          , ..
          }
```

### LANGUAGE Pragma

``` haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
```

``` haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
```

``` haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
```

``` haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
```

### Tuple, UnboxedSum, List

``` haskell

foo = ()
foo = (1, 2)
foo = (1 -- the one
 , 2)
```

``` haskell
{-# LANGUAGE TupleSections #-}

foo = (1,)
foo = (,2)
foo = (,2,)
```

``` haskell
{-# LANGUAGE UnboxedTuples #-}

foo = (# #)
foo = (# 1, 2 #)
foo = (# 1 -- the one
 , 2 #)
```


``` haskell
{-# LANGUAGE UnboxedSums #-}

foo = (# 1 #)
foo = (# | 1 | | #)
foo = (# | 1 -- the one
      | | #)
```

``` haskell

foo = []
foo = [1]
foo = [1,2]
foo = [1 -- the one
 , 2]
```

### Syntax

#### Exponents
``` haskell

fn = 1234e+6
fn = 1234e-6
fn = 1234e6
```

#### BinaryLiterals
``` haskell
{-# LANGUAGE BinaryLiterals #-}

fn = 0b011110
```

#### HexFloatLiterals
HSE Does not support these at all . The last 2 cases are pattently ambiguous and broken

``` haskell
{-# LANGUAGE HexFloatLiterals #-}

-- fn = 0x1.1
-- fn = 0x0.1p-4
-- fn = 0x0.1p12
-- fn = 0xF.FF
-- fn = 0xF.FFp-12
```

``` haskell
{-# LANGUAGE HexFloatLiterals #-}
fn = f 0xF . FFp12
fn = f 0xF.FFp12
```

#### NumericUnderscores

``` haskell
{-# LANGUAGE NumericUnderscores #-}

fn = do 3_000
```

``` haskell
{-# LANGUAGE NumericUnderscores #-}

fn = 3_000
fn = 2_123_123_1234
```

``` haskell
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

fn = let t = 3_000 in t

fn = let t = 0x3ff_00_00
         b = 0b01_0000_0000
         e = 6.022_140_857e+23
      in (t, b , e)
```

## Full Module

``` haskell
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}


module KafkaConsumer
  ( KafkaCfg (..)
  , KafkaConsumerEvent (..)
  , logKafka
  , logKafka'
  , kafkaTopicConsumer
  , redisKey
  , recordOffsetsToRedis
  , restoreOffsetsFromRedis
  , removeInvalidOffsetsForRecording
  , redisRebalanceCallback'
  , int64ToOffset
  , getKafkaConsumerCfgFromEnv
  ) where

import           Control.Arrow
  ( (&&&)
  )
import           Control.Exception
  ( SomeException
  )
import           Control.Exception.Lifted
  ( bracket
  , try
  )
import           Control.Monad
  ( forM
  , forM_
  , void
  )
import           Control.Monad.Reader
  ( MonadReader
  )
import           Control.Monad.Trans.Control
import           Data.Aeson
  ( FromJSON
  , eitherDecode
  )
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC
import qualified Data.ByteString.Lazy        as BL
import           Data.Char
  ( toLower
  )
import           Data.Foldable
  ( toList
  )
import           Data.Int
  ( Int64
  )
import qualified Data.Map.Strict             as M
import           Data.Maybe
  ( catMaybes
  , fromMaybe
  )
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Time.Clock.POSIX       as POSIX
import           GHC.Exts
  ( fromList
  )
import           Kafka.Consumer
  ( BatchSize (..)
  , BrokerAddress (..)
  , ConsumerGroupId (..)
  , ConsumerProperties (..)
  , ConsumerRecord (..)
  , KafkaConsumer
  , KafkaError
  , KafkaLogLevel (..)
  , Offset (..)
  , OffsetCommit (..)
  , OffsetReset (..)
  , PartitionId (..)
  , PartitionOffset (..)
  , RebalanceEvent (..)
  , Subscription
  , Timeout (..)
  , TopicName (..)
  , TopicPartition (..)
  , brokersList
  , debugOptions
  , extraProps
  , groupId
  , logCallback
  , logLevel
  , newConsumer
  , noAutoCommit
  , offsetCommitCallback
  , offsetReset
  , rebalanceCallback
  , setCallback
  , topics
  )
import           PyF
import           Text.Read
  ( readMaybe
  )

import           PureLib.Types

data KafkaCfg
  = KafkaCfg
  { kcBrokers               :: ![T.Text]
  , kcTimeoutMillis         :: !Timeout
  , kcBatchSize             :: !BatchSize
  , kcOffsetReset           :: !OffsetReset
  , kcMaxPollIntervalMillis :: !(Maybe Integer)
  }
  deriving (Eq, Show)

kafkaPrefix
  :: String
kafkaPrefix
  = "KafkaConsumer:    "

logKafka
  :: String
  -> IO ()
logKafka msg
  = do
  ts <- POSIX.getPOSIXTime
  putStrLn [fmt|{ts:s}: {kafkaPrefix}{msg}|]

logKafka'
  :: ( SupportsLogging m
     , SupportsTime m
     , Monad m
     )
  => T.Text
  -> m ()
logKafka' msg
  = do
  ts <- getPOSIXTime
  logIt [fmt|{ts:s}: {kafkaPrefix}{msg}|]


-- Global consumer properties
consumerProps
  :: forall m s
   . ( SupportsKafka m
     , SupportsLogging m
     , SupportsRedis m
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => KafkaCfg
  -> (KafkaConsumer -> s)
  -> (forall a. m a -> s -> IO a)
  -> T.Text
  -> ConsumerProperties
consumerProps KafkaCfg {..} mkState runner group
  = brokersList [BrokerAddress k | k <- kcBrokers]
  <> groupId (ConsumerGroupId group)
  <> noAutoCommit
  <> setCallback (rebalanceCallback (defer hoistGAppMToIO mkState runner (redisRebalanceCallback group)))
  <> setCallback (offsetCommitCallback printingOffsetCallback)
  <> setCallback (logCallback printingLogCallback)
  <> debugOptions []
  <> logLevel KafkaLogDebug
  <> extraProps extraPropsCfg
  where
    extraPropsCfg
      = fromList
      $ catMaybes
        [ ("max.poll.interval.ms",) . T.pack . show <$> kcMaxPollIntervalMillis ]

-- Subscription to topics
consumerSub
  :: KafkaCfg
  -> T.Text
  -> Subscription
consumerSub _kcConfig topic
  = topics [TopicName topic]
  <> offsetReset Earliest


data KafkaConsumerEvent
  = KCEDecodeSucceeded
  | KCEHandled
  | KCEDecodeFailed
  deriving
  ( Show
  , Eq
  )

-------------------------------------------------------------------
processMessages
  :: ( SupportsKafka m
     , SupportsRedis m
     , SupportsLogging m
     , SupportsTime m
     , MonadReader s m
     , FromJSON e
     )
  => KafkaCfg
  -> T.Text
  -> (ConsumerRecord (Maybe BS.ByteString) (Maybe BS.ByteString) -> e -> m ())
  -> (KafkaConsumerEvent -> T.Text -> T.Text -> m ())
  -> m (Either KafkaError ())
processMessages KafkaCfg {..} group onMessageCB onEvent
  = do
  logKafka' "Polling for Messages"
  msgs <- pollMessageBatch kcTimeoutMillis kcBatchSize

  logKafka' [fmt|Messages: {msgs:s}|]
  forM_ msgs $ \msg1 -> do
    logKafka' [fmt|Message: {msg1:s}|]

    forM_ msg1 $ \msg ->

      forM_ (crValue msg) $ \msgValue ->
        case eitherDecode $ BL.fromStrict msgValue of
          Right decoded -> do
            incKafkaMetric KCEDecodeSucceeded msg
            onMessageCB msg decoded
            incKafkaMetric KCEHandled msg
          Left err -> do
            logKafka' [fmt|Failed to decode msg: {err:s}|]
            incKafkaMetric KCEDecodeFailed msg

    err <- commitAllOffsets OffsetCommit
    logKafka' [fmt|Offsets: {maybe "Committed." (T.pack . show) err}|]

  recordOffsetsToRedis
    group
    $ removeInvalidOffsetsForRecording
    $ fmap (\((t, p), o) -> TopicPartition t p (int64ToOffset o))
    $ M.toList
    $ M.fromListWith
        max
          [ ((crTopic m, crPartition m), (+1) $ unOffset $ crOffset m)
          | Right m <- msgs
          ]

  return $ Right ()

  where
    incKafkaMetric e msg
      = onEvent
      e
      (unTopicName . crTopic $ msg)
      (T.pack . show . unPartitionId . crPartition $ msg)


-- LAME not exported steal them
offsetToInt64
  :: PartitionOffset
  -> Int64
offsetToInt64
  = \case
  PartitionOffsetBeginning -> -2
  PartitionOffsetEnd       -> -1
  PartitionOffset off      -> off
  PartitionOffsetStored    -> -1000
  PartitionOffsetInvalid   -> -1001
{-# INLINE offsetToInt64 #-}

int64ToOffset
  :: Int64
  -> PartitionOffset
int64ToOffset o
  | o == -2    = PartitionOffsetBeginning
  | o == -1    = PartitionOffsetEnd
  | o == -1000 = PartitionOffsetStored
  | o >= 0     = PartitionOffset o
  | otherwise  = PartitionOffsetInvalid
{-# INLINE int64ToOffset #-}

printingLogCallback
  :: Int
  -> String
  -> String
  -> IO ()
printingLogCallback i s s1
  = logKafka
  $ "Logit: "
  <> show i
  <> " "
  <> s
  <> " "
  <> s1

redisKey
  :: T.Text
  -> TopicName
  -> PartitionId
  -> BSC.ByteString
redisKey groupName topicName partitionId
  = "kafka_consumer.group_positions."
  <> TE.encodeUtf8 (unTopicName topicName)
  <> "."
  <> TE.encodeUtf8 groupName
  <> "."
  <> BSC.pack (show $ unPartitionId partitionId)
  <> ".position"

newtype RecordableTopicPartition
  = RecordableTopicPartition
  { unRecordableTopicPartition :: TopicPartition
  }
  deriving
  Show

mkRecordableTopicPartition
  :: TopicPartition
  -> Maybe RecordableTopicPartition
mkRecordableTopicPartition tp@TopicPartition {tpOffset = PartitionOffset _}
  = Just $ RecordableTopicPartition tp
mkRecordableTopicPartition _
  = Nothing

recordOffsetsToRedis
  :: ( SupportsRedis m
     , SupportsLogging m
     , SupportsTime m
     , Foldable f
     , Monad m
     )
  => T.Text
  -> f RecordableTopicPartition
  -> m ()
recordOffsetsToRedis _ (null -> True)
  = pure ()
recordOffsetsToRedis group positions
  = do
  logKafka' [fmt|Positions to record to redis {(toList positions):s}|]
  forM_ positions $ \(RecordableTopicPartition TopicPartition {..}) ->
    setRedis (redisKey group tpTopicName tpPartition) (BSC.pack $ show $ offsetToInt64 tpOffset)

restoreOffsetsFromRedis
  :: ( SupportsRedis m
     , SupportsKafka m
     , SupportsLogging m
     , SupportsTime m
     , Monad m
     )
  => T.Text
  -> [(TopicName, PartitionId)]
  -> m ()
restoreOffsetsFromRedis _ (null -> True)
  = pure ()
restoreOffsetsFromRedis group ps
  = do
  res <- forM ps $ \(topicName, partitionId) ->
    fmap (topicName, partitionId,) <$> getRedis (redisKey group topicName partitionId)

  logKafka' [fmt|Raw results from redis {res:s}|]

  let
    seeks
      =
      [ TopicPartition t p (int64ToOffset $ read $ BSC.unpack o)
      | Right (t, p, Just o) <- res
      ]

  logKafka' [fmt|Filtered results from redis for seeking {seeks:s}|]

  void $ seek (Timeout 3_000) seeks

redisRebalanceCallback
  :: forall m
   . ( SupportsRedis m
     , SupportsKafka m
     , SupportsLogging m
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => T.Text
  -> RebalanceEvent
  -> m ()
redisRebalanceCallback group e
  = either
  (logKafka' . ("[Rebalance] caught an error rebalancing " <>) . T.pack . show)
  (const $ pure ())
    =<< (try @m @SomeException $ redisRebalanceCallback' group e)

redisRebalanceCallback'
  :: forall m
   . ( SupportsRedis m
     , SupportsKafka m
     , SupportsLogging m
     , SupportsTime m
     , Monad m
     )
  => T.Text
  -> RebalanceEvent
  -> m ()
redisRebalanceCallback' group
  = \case
  RebalanceBeforeAssign ps ->
    logKafka' [fmt|[Rebalance] About to assign partitions: {ps:s}|]
  RebalanceAssign ps -> do
    restoreOffsetsFromRedis group ps
    logKafka' [fmt|[Rebalance] Assign partitions: {ps:s}|]
  RebalanceBeforeRevoke ps ->
    logKafka' [fmt|[Rebalance] About to revoke partitions: {ps:s}|]
  RebalanceRevoke ps -> do
    positions <- position ps

    forM_ positions $ \positions' ->
      recordOffsetsToRedis
        group
        $ removeInvalidOffsetsForRecording positions'

    logKafka' [fmt|[Rebalance] Revoke partitions: {ps:s}|]

printingOffsetCallback
  :: KafkaConsumer
  -> KafkaError
  -> [TopicPartition]
  -> IO ()
printingOffsetCallback _ e ps
  = do
  logKafka ("Offsets callback: " <> show e)
  mapM_ (logKafka . ("Offsets callback: " <>) . show . (tpTopicName &&& tpPartition &&& tpOffset)) ps

getKafkaConsumerCfgFromEnv
  :: forall m
   . ( SupportsEnv m
     , Monad m
     )
  => m KafkaCfg
getKafkaConsumerCfgFromEnv
  = do
  let
    defBatch
      = BatchSize 500
    defTimeout
      = Timeout 100
    defOffsetReset
      = Earliest

    envHelper
      :: a
      -> (String -> a)
      -> String
      -> m a
    envHelper def_ conv_ env_
      = maybe
      def_
      conv_
      <$> lookupEnv env_

  kcBrokers <- envHelper
    []
    (T.splitOn "," . T.pack)
    "IR_EVENTS_KAFKA_SERVERS"

  kcBatchSize <- envHelper
    defBatch
    (maybe defBatch BatchSize . readMaybe)
    "IR_CONSUMER_MAX_POLL_RECORDS"

  kcTimeoutMillis <- envHelper
    defTimeout
    (maybe defTimeout Timeout . readMaybe)
    "IR_CONSUMER_POLL_TIMEOUT_MS"

  kcMaxPollIntervalMillis <- envHelper
    Nothing
    readMaybe
    "IR_CONSUMER_MAX_POLL_INTERVAL_MS"

  kcOffsetReset <- envHelper
    defOffsetReset
    (fromMaybe defOffsetReset . (\v ->
      case fmap toLower v of
        "earliest" -> Just Earliest
        "latest"   -> Just Latest
        _          -> Nothing
    ))
    "IR_CONSUMER_OFFSET_RESET"

  pure KafkaCfg {..}

-- Running an example
kafkaTopicConsumer
  :: forall m s e
   . ( Monad m
     , SupportsAsync m
     , SupportsGracefulTermination m
     , SupportsKafka m
     , SupportsLogging m
     , SupportsRedis m
     , SupportsTime m
     , MonadBaseControl IO m
     , MonadReader s m
     , FromJSON e
     )
  => (KafkaConsumer -> s)
  -> (forall a. m a -> s -> IO a)
  -> (ConsumerRecord (Maybe BS.ByteString) (Maybe BS.ByteString) -> e -> m ())
  -> (KafkaConsumerEvent -> T.Text -> T.Text -> m ())
  -> T.Text
  -> T.Text
  -> IO ()
kafkaTopicConsumer mkState runner onMessage onEvent topic group
  = do
  logKafka "Start consumer"

  kcConfig <- getKafkaConsumerCfgFromEnv

  let
    props
      = consumerProps kcConfig mkState runner group

  res <- bracket
    (mkConsumer kcConfig props)
    clConsumer
    (runHandler kcConfig)

  logKafka [fmt|End consumer: {res:s}|]
  where
    mkConsumer kcConfig props
      = do
      logKafka "Starting a new consumer"
      c <- newConsumer props (consumerSub kcConfig topic)
      logKafka "Started a new consumer"
      pure c


    clConsumer (Left err)
      = do
      logKafka [fmt|Finalizing a consumer that failed to start: {err:s}|]
      pure $ Left err
    clConsumer (Right kc)
      = do
      logKafka "Preparing to finalize a consumer"
      r <- hoistGAppMToIO mkState runner kc (finalizeConsumer topic group)
      logKafka "Finalized a consumer"
      pure r

    runHandler
      :: KafkaCfg
      -> Either KafkaError KafkaConsumer
      -> IO (Either KafkaError ())
    runHandler _kcConfig (Left err)
      = do
      logKafka "No consumer to run. Bail"
      return (Left err)
    runHandler kcConfig (Right kc)
      = do
      logKafka "Hoist and process messages"
      r <- hoistGAppMToIO mkState runner kc go
      logKafka "Hoist and process messages finished gracefully"
      pure r
      where
        go = processMessages kcConfig group onMessage onEvent >> checkForDeath >>= \case
          True -> pure (Right ())
          False -> yield >> go

finalizeConsumer
  :: ( SupportsKafka m
     , SupportsRedis m
     , SupportsLogging m
     , SupportsTime m
     , MonadReader s m
     )
  => T.Text
  -> T.Text
  -> m (Either KafkaError ())
finalizeConsumer topic group
  = do
  let
    pfx
      :: T.Text
    pfx
      = "Finalize:"

  logKafka' [fmt|{pfx} Committing all offsets on finalize|]
  err <- commitAllOffsets OffsetCommit
  logKafka' [fmt|{pfx} {maybe "Finally Committed Offsets" (T.pack . show) err}|]

  logKafka' [fmt|{pfx} Fetching assignment from kafka|]
  topicToPartition <- assignment
  logKafka' [fmt|{pfx} Fetched assignments {topicToPartition:s}|]

  forM_ topicToPartition $ \topicToPartition' -> do
    let
      topicName
        = TopicName topic
      ourIds
        = fmap (topicName,) <$> M.lookup topicName topicToPartition'

    forM_ ourIds $ \ourIds' -> do
      logKafka' [fmt|{pfx} Getting positions from kafka|]
      -- Hypothesis: this doesnt give us true positions for things we havent seen...
      positions <- position ourIds'
      logKafka' [fmt|{pfx} Fetched positions {positions:s}|]

      forM_ positions $ \positions' -> do
        logKafka' [fmt|{pfx} Recording positions to redis {positions':s}"|]
        recordOffsetsToRedis
          group
          (removeInvalidOffsetsForRecording positions')

  maybe (Right ()) Left <$> closeConsumer

-- as for Hypothesis: remove weird offsets, only record true offsets
removeInvalidOffsetsForRecording
  :: [TopicPartition]
  -> [RecordableTopicPartition]
removeInvalidOffsetsForRecording positions
  = catMaybes
  [ mkRecordableTopicPartition t
  | t <- positions
  ]

hoistGAppMToIO
  :: forall m s
   . (KafkaConsumer -> s)
  -> (forall a. m a -> s -> IO a)
  -> KafkaConsumer
  -> (forall a. m a -> IO a)
hoistGAppMToIO mkState runner kc
  = go
  where
  go
    :: m a
    -> IO a
  go act
    = runner act $ mkState kc

defer
  :: ((defer -> s) -> (forall a. m a -> s -> n a) -> defer -> act -> act2)
  -> (defer -> s)
  -> (forall a. m a -> s -> n a)
  -> (arg -> act)
  -> defer
  -> arg
  -> act2
defer hoist mkState runner
  = go
  where
    go act kc y
      = hoist mkState runner kc (act y)
```
