{- | Properties used in testing. -}

module Test.Property (
    -- * Properties.
    prop_extensional_equality,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Libraries.
import Test.Falsify (Gen, Property, (.$), gen, assert)

-- Package.
import Data.Function.Annotated (Function)
import Test.Predicate (isExtensionallyEqual)


{- | Test extensional equality of functions. -}
prop_extensional_equality
    :: (Show a, Eq b)
    => Function a b
    -> Function a b
    -> Gen a
    -> Property ()
prop_extensional_equality p q g = do
        x <- gen g
        assert $ isExtensionallyEqual p q .$ (fromString "x", x)
