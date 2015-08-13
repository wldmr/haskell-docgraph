module DocGraph.Util where

-- | Monadic If
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate ifPart elsePart = do
    isTrue <- predicate
    if isTrue then ifPart else elsePart

