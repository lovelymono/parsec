module Text.Parsec where

data Result i a =
    Fail i
  | Done i a
  deriving (Show)

data Parser i a = Parser { run :: i -> Result i a }

instance Functor (Parser i) where
  -- fmap :: (a -> b) -> Parser i a -> Parser i b
  fmap f p = Parser $ \i -> case run p i of
    Done i' v -> Done i' (f v)
    Fail i' -> Fail i'

instance Applicative (Parser i) where
  -- pure :: a -> Parser i a
  pure v = Parser $ \i -> Done i v

  -- (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
  p <*> q = Parser $ \i -> case run p i of
    Done i' v -> case run q i' of
      Done i'' w -> Done i'' (v w)
      Fail i' -> Fail i'
    Fail i -> Fail i

instance Monad (Parser i) where
  -- return :: a -> Parser i a
  return = pure

  -- (>>=) :: Parser i a -> (a -> Parser i b) -> Parser i b
  p >>= f = Parser $ \i -> case run p i of
    Done i' v -> run (f v) i'
    Fail i -> Fail i
