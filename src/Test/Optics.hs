{- | Optics laws as properties. -}

module Test.Optics (
    -- * From optics to named functions.
    fromLens,

    -- * Optics laws.
    test_isomorphism_laws,
    test_prism_laws,
) where

-- Imports.
-- Libraries.
import Data.Text (Text, pack)
import Optics.Core (Iso', Lens', Prism', view, review, preview, set)

-- Testing.
import Test.Falsify (Gen, Property)

-- Package.
import Data.Function.Annotated (Function, annotateE, composeE, idE, mapE)
import Test.Property (prop_extensional_equality)


{- | Turn an Isomorphism into a pair of named functions. -}
fromIso :: Text -> Iso' a b -> (Function a b, Function b a)
fromIso xs i = (f, g)
    where
        f = annotateE (pack "view " <> xs) (view i)
        g = annotateE (pack "review " <> xs) (review i)

{- | Turn a Lens into a pair of named functions. -}
fromLens :: Text -> Lens' s a -> (Function s a, Function a (s -> s))
fromLens xs l = (f, g)
    where
        f = annotateE (pack "view " <> xs) (view l)
        g = annotateE (pack "set " <> xs) (set l)

{- | Turn a Prism into a pair of named functions. -}
fromPrism :: Text -> Prism' s a -> (Function s (Maybe a), Function a s) 
fromPrism xs p = (f, g)
    where
        f = annotateE (pack "preview " <> xs) (preview p)
        g = annotateE (pack "review " <> xs) (review p)


{- | Test the Isomorphism laws. -}
test_isomorphism_laws
    :: (Eq a, Eq b, Show a, Show b)
    => Text
    -> Iso' a b
    -> Gen a
    -> Gen b
    -> Property ()
test_isomorphism_laws xs i genA genB = let (f, g) = fromIso xs i in
        prop_extensional_equality (composeE g f) idE genA *>
        prop_extensional_equality (composeE f g) idE genB

{- | Test the Prism laws. -}
test_prism_laws
    :: forall a s . (Eq a, Eq s, Show a, Show s)
    => Text
    -> Prism' s a
    -> Gen s
    -> Gen a
    -> Property ()
test_prism_laws xs p genS genA = let (q, r) = fromPrism xs p in
        prop_extensional_equality (composeE q r) just genA *>
        prop_extensional_equality (composeE (mapE r) q) matches genS
    where
        just :: Function a (Maybe a)
        just = annotateE (pack "Just") Just

        matches :: Function s (Maybe s)
        matches =
            annotateE
                (pack "matching " <> xs)
                (\ s -> maybe Nothing (const . Just $ s) (preview p s))
