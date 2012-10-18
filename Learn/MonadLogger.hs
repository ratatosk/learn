module Learn.MonadLogger where

class Monad m => MonadLogger m where
  yell :: String -> m ()

instance MonadLogger IO where
  yell = putStrLn