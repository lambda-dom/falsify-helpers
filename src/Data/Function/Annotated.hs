{-# LANGUAGE NoFieldSelectors #-}

{- |

Module: Data.Function.Named

A small dsl to construct showable functions.
-}

module Data.Function.Annotated (
    -- * Predicates.
    isIdentifier,
    isOperator,

    -- * The @Operator@ type.
    Operator,

    -- ** Prisms.
    operator,

    -- * The @Identifier@ type.
    Identifier,

    -- ** Prisms.
    identifier,

    -- * The @Function@ type.
    Function (..),

    -- ** @t'Function'@ combinators.
    annotateE,
    letE,
    idE,
    composeE,
    firstE,
    secondE,
    terminal,
    diagonalE,
    productE,
    binaryE,
    uncurryE,
    curryE,
    swapE,
    associatorE,
    cofirstE,
    cosecondE,
    initialE,
    codiagonalE,
    coproductE,
    coswapE,
    coassociatorE,
    mapE,
    zipE,
    pureE,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import qualified Control.Category as Category (Category (..))
import Data.Bifunctor (bimap)
import Data.Char (isLetter, isAlphaNum)
import Data.Function ((&))
import Data.Tuple (swap)
import Data.Void (Void, absurd)

-- Libraries.
import Data.Text (Text, pattern (:<), pattern Empty, unpack, pack)
import qualified Data.Text as Text (all)
import Optics.Core (Prism', (^.), (.~), prism')

-- Package.
import Data.Either ((:+:), coswap, coassociator)


{- | Surround a 'Text' with parens. -}
withParens :: Text -> Text
withParens xs = pack "(" <> xs <> pack ")" 


{- | 'True' if 'Text' is a valid identifier. -}
isIdentifier :: Text -> Bool
isIdentifier Empty     = False
isIdentifier (c :< cs) = isLetter c && Text.all (\ x -> isAlphaNum x || '_' == x || '-' == x) cs

{- | 'True' if 'Text' is a valid operator. -}
isOperator :: Text -> Bool
isOperator Empty = False
isOperator xs    = Text.all (`elem` "<>|$%/=?+*.:~^") xs


{- | The @Operator@ type. -}
newtype Operator = Operator Text
    deriving stock Eq
    deriving newtype Show

{- | Prism for t'Identifier'. -}
operator :: Prism' Text Operator
operator = prism' make match
    where
        make :: Operator -> Text
        make (Operator xs) = xs

        match :: Text -> Maybe Operator
        match xs = if isOperator xs then Just . Operator $ xs else Nothing


{- | The @Identifier@ type. -}
newtype Identifier = Identifier Text
    deriving stock Eq
    deriving newtype Show

{- | Prism for t'Identifier'. -}
identifier :: Prism' Text Identifier
identifier = prism' make match
    where
        make :: Identifier -> Text
        make (Identifier xs) = xs

        match :: Text -> Maybe Identifier
        match xs = if isIdentifier xs then Just . Identifier $ xs else Nothing


{- | A show-able function. -}
data Function a b = Function {
    name     :: !Text,
    function :: (a -> b)
} deriving stock (Generic)


-- Instances.
instance Show (Function a b) where
    show :: Function a b -> String
    show = unpack . (^. #name)

instance Category.Category Function where
    id :: Function a a
    id = idE

    (.) :: Function b c -> Function a b -> Function a c
    (.) = composeE


{- | Annotate a function with a name. -}
annotateE :: Text -> (a -> b) -> Function a b
annotateE = Function

{- | Assign an identifier to a named function. -}
letE :: Identifier -> Function a b -> Function a b
letE (Identifier xs) f = f & #name .~ xs

{- | The identity. -}
idE :: Function a a
idE = annotateE (pack "id") id

{- | Composition of two named functions. -}
composeE :: Function b c -> Function a b -> Function a c
composeE g f =
    annotateE
        (g ^. #name <> pack " . " <> f ^. #name)
        ((g ^. #function) . (f ^. #function))

{- | The projection into the first component. -}
firstE :: Function (a, b) a
firstE = annotateE (pack "first") fst

{- | The projection into the first component. -}
secondE :: Function (a, b) b
secondE = annotateE (pack "second") snd

{- | The unique function into the product unit. -}
terminal :: Function a ()
terminal = annotateE (pack "terminal") (const ())

{- | The product diagonal. -}
diagonalE :: Function a (a, a)
diagonalE = annotateE (pack "diagonal") (\ x -> (x, x))

{- | The product bifunctor. -}
productE :: Function a c -> Function b d -> Function (a, b) (c, d)
productE f g =
    annotateE
        (f ^. #name <> pack " :*: " <> g ^. #name)
        (bimap (f ^. #function) (g ^. #function))

{- | A binary operator in uncurried form. -}
binaryE :: Operator -> (a -> a -> b) -> Function (a, a) b
binaryE (Operator op) h =
    annotateE
        ((pack "uncurry ") <> (withParens op))
        (uncurry h)

{- | Uncurry a named function. -}
uncurryE :: Function a (b -> c) -> Function (a, b) c
uncurryE f = annotateE (pack "uncurry " <> f ^. #name) (uncurry $ f ^. #function)

{- | Curry a named function. -}
curryE :: Function (a, b) c -> Function a (b -> c)
curryE f = annotateE (pack "curry " <> f ^. #name) (curry $ f ^. #function)

{- | The symmetry of the product. -}
swapE :: Function (a, b) (b, a)
swapE = annotateE (pack "swap") swap

{- | The monoidal structure of the product. -}
associatorE :: Function (a, (b, c)) ((a, b), c)
associatorE = annotateE (pack "associator") (\ (x, (y, z)) -> ((x, y), z))

{- | The first injection into the coproduct. -}
cofirstE :: Function a (a :+: b)
cofirstE = annotateE (pack "cofirst") Left

{- | The second injection into the coproduct. -}
cosecondE :: Function b (a :+: b)
cosecondE = annotateE (pack "cosecond") Right

{- | The unique function from the 'Void' type. Ftaghn. -}
initialE :: Function Void a
initialE = annotateE (pack "initial") absurd

{- | The coproduct codiagonal. -}
codiagonalE :: Function (a :+: a) a
codiagonalE = annotateE (pack "codiagonal") (either id id)

{- | The coproduct bifunctor. -}
coproductE :: Function a c -> Function b d -> Function (a :+: b) (c :+: d)
coproductE f g =
    annotateE
        (f ^. #name <> pack " :+: " <> g ^. #name)
        (bimap (f ^. #function) (g ^. #function))

{- | The symmetry of the coproduct. -}
coswapE :: Function (a :+: b) (b :+: a)
coswapE = annotateE (pack "coswap") coswap

{- | The monoidal structure of the coproduct. -}
coassociatorE :: Function (a :+: (b :+: c)) ((a :+: b) :+: c)
coassociatorE = annotateE (pack "coassociator") coassociator

{- | The 'Functor' map. -}
mapE :: Functor f => Function a b -> Function (f a) (f b)
mapE f =
    annotateE
        (pack "fmap " <> withParens (f ^. #name))
        (fmap (f ^. #function))

{- | The 'Applicative' zipping map. -}
zipE :: Applicative f => Function (f a, f b) (f (a, b))
zipE =
    annotateE
        (pack "zip")
        (\ (p, q) -> (fmap (,) p) <*> q)

{- | The 'Applicative' pure map. -}
pureE :: Applicative f => Function a (f a)
pureE = annotateE (pack "pure") pure
