{-# LANGUAGE DataKinds #-}

{- | Some predicates used in testing. -}

module Test.Predicate (
    -- * Predicates.
    isExtensionallyEqual,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Libraries.
import Optics.Core ((^.))
import Test.Falsify (Predicate)
import Test.Falsify.Predicate (satisfies)

-- Package.
import Data.Function.Annotated (NamedFn (..))


{- | Predicate for functional, extensional equality of functions. -}
isExtensionallyEqual
    :: Eq b
    => NamedFn a b
    -> NamedFn a b
    -> Predicate '[a]
isExtensionallyEqual f g = satisfies (
        (fromString $ (show $ f ^. #name) ++ " == " ++ (show $ g ^. #name)),
        \ x -> (f ^. #function) x == (g ^. #function) x
    )
