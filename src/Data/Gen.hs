{- | Some generators used in testing. -}

module Data.Gen (
    -- * Generators for primitive types.
    integral,
    enumFull,

    -- ** Specializations: Char generators.
    char,
    safeChar,
    ascii,

    -- ** Specializations: primitive integral types.
    word8,
    word16,
    word32,
    int8,
    int16,
    int32,

    -- * @ByteString@ generators.
    byteString,

    -- * @Text@ generators.
    text,
    asciiText,

    -- * Tuple generators.
    pair,
    triple,
    quadruple,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Char (chr)
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, pack)
import qualified Data.Text as Text (Text, pack)
import qualified Data.Text.Internal as Internal (safe)

-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange, list, int)
import Test.Falsify.Range (uniform, inclusive, enum)
import Data.Int (Int8, Int16, Int32)


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Generator for the full range of a bounded enumeration. -}
enumFull :: (Enum a, Bounded a) => Gen a
enumFull = inRange $ enum (minBound, maxBound)


{- | Generator for 'Char'. -}
char :: Gen Char
char = enumFull

{- | Generator for 'Char'.

Performs replacement on invalid scalar values using the 'Data.Text.Internal.safe' function from
'Data.Text.Internal'. This is required when using the text package as many functions perform such a
replacement.
-}
safeChar :: Gen Char
safeChar = Internal.safe <$> char

{- | Generator for an ascii 'Char' except the null character. -}
ascii :: Gen Char
ascii = fmap chr . int $ inclusive (1, 127)


{- | Specialization of 'integral' to 'Word8'. -}
word8 :: Gen Word8
word8 = integral

{- | Specialization of 'integral' for 'Word16' generator. -}
word16 :: Gen Word16
word16 = integral

{- | Specialization of 'integral' for 'Word32' generator. -}
word32 :: Gen Word32
word32 = integral

{- | Specialization of 'integral' for 'Int8' generator. -}
int8 :: Gen Int8
int8 = integral

{- | Specialization of 'integral' for 'Int16' generator. -}
int16 :: Gen Int16
int16 = integral

{- | Specialization of 'integral' for 'Int32' generator. -}
int32 :: Gen Int32
int32 = integral


{- | Generate a (strict) 'Data.ByteString.ByteString' up to length @n@ shrinking towards 0. -}
byteString :: Word -> Gen Bytes.ByteString
byteString n = Bytes.pack <$> list (inclusive (0, n)) word8


{- | Generate a (strict) 'Data.Text.Text' up to length @n@ shrinking towards 0. -}
text :: Word -> Gen Text.Text
text n = Text.pack <$> list (inclusive (0, n)) char

{- | Generate a (strict) 'Data.Text.Text' up to length @n@ of ascii characters minus null. -}
asciiText :: Word -> Gen Text.Text
asciiText n = Text.pack <$> list (inclusive (0, n)) ascii


{- | Generator for pairs. -}
pair :: Gen a -> Gen b -> Gen (a, b)
pair g h = (,) <$> g <*> h

{- | Generator for triples. -}
triple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
triple f g h = (,,) <$> f <*> g <*> h

{- | Generator for quadruples, or 4-tuples. -}
quadruple :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d)
quadruple f g h i = (,,,) <$> f <*> g <*> h <*> i

