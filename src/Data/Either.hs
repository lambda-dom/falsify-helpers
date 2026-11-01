{- |
Module: Data.Either

Some utilities for the `Either` coproduct bifunctor.
-}

module Data.Either (
    -- * Type operators.
    (:+:),

    -- * Monoidal structure of the coproduct.
    coswap,
    coassociator,

    -- * Functor combinators.
    cozip,
) where


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | The symmetry of the coproduct. -}
coswap :: a :+: b -> b :+: a
coswap = either Right Left

{- | The associator of the coproduct. -}
coassociator :: a :+: (b :+: c) -> (a :+: b) :+: c
coassociator = either (Left . Left) (either (Left . Right) Right)


{- | Dual of 'zip' for 'Either'. -}
cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
