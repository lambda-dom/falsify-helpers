{- | Monoid laws as falsifiable properties. -}

module Test.Laws.Monoid (
    -- * Monoid laws.
    prop_semigroup_laws,
    prop_monoid_laws,
) where

-- Imports.
-- Libraries.
import Data.Text (pack)
import Test.Falsify (Gen, Property)

-- Package.
import Data.Function.Annotated (NamedFn, annotateE, idE)
import Test.Gen (triple)
import Test.Property (prop_extensional_equality)


{- | Test 'Semigroup' laws. -}
prop_semigroup_laws
    :: forall a . (Semigroup a, Eq a, Show a)
    => Gen a
    -> Property ()
prop_semigroup_laws genA = prop_extensional_equality left right (triple genA genA genA) 
    where
        left :: NamedFn (a, a, a) a
        left = annotateE
            (pack "x <> (y <> z)")
            (\ (x, y, z) -> x <> (y <> z))

        right :: NamedFn (a, a, a) a
        right = annotateE
            (pack "(x <> y) <> z")
            (\ (x, y, z) -> (x <> y) <> z)

{- | Test 'Monoid' laws. -}
prop_monoid_laws
    :: forall a . (Monoid a, Eq a, Show a)
    => Gen a
    -> Property ()
prop_monoid_laws genA
    =  prop_semigroup_laws genA
    *> prop_extensional_equality left idE genA
    *> prop_extensional_equality right idE genA
    where
        left :: NamedFn a a
        left = annotateE (pack "x <> mempty") (<> mempty)

        right :: NamedFn a a
        right = annotateE (pack "mempty <> x") (mempty <>)
