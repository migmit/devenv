{-# LANGUAGE DeriveFunctor #-}
module FailureOr(FailureOr, guardIO, orErr, recoverFromFailure, runFailureOr) where
import Control.Applicative(Alternative(empty, (<|>)))
import Control.Monad (MonadPlus, ap, liftM2)
import Control.Exception (throwIO)

data FailureOr a = FailureOr String | NoFailure a deriving Functor
instance Applicative FailureOr where
    pure = return
    (<*>) = ap
instance Monad FailureOr where
    fail = FailureOr
    return = NoFailure
    FailureOr s >>= _ = FailureOr s
    NoFailure a >>= f = f a
instance Monoid a => Monoid (FailureOr a) where
    mempty = return mempty
    mappend = liftM2 mappend
instance Alternative FailureOr where
    empty = fail ""
    FailureOr _ <|> f = f
    f@(NoFailure _) <|> _ = f
instance MonadPlus FailureOr
recoverFromFailure :: a -> FailureOr a -> a
recoverFromFailure d (FailureOr _) = d
recoverFromFailure _ (NoFailure a) = a
runFailureOr :: FailureOr a -> IO a
runFailureOr (FailureOr s) = throwIO $ userError s
runFailureOr (NoFailure a) = return a
orErr :: String -> Maybe a -> FailureOr a
orErr a Nothing = fail a
orErr _ (Just b) = return b

guardIO :: Bool -> String -> IO ()
guardIO False s = throwIO $ userError s
guardIO True _ = return ()