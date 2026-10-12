{- |
Module: Data.Either

Some utilities for the `Either` coproduct bifunctor.
-}

module Data.Either (
    -- * Type operators.
    (:+:),

    -- * Functor combinators.
    cozip,
) where


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | Dual of 'zip' for 'Either'. -}
{-# INLINE cozip #-}
cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
