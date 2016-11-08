{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TAPL.Meow (
  exec
, exec_
, meow
, Meow
) where

import Control.Monad.Writer hiding (Writer)

newtype Meow a = Meow (WriterT [String] IO a)
  deriving (Functor, Applicative, Monad)

exec :: Meow a -> IO (a, [String])
exec (Meow m) = runWriterT m

exec_ :: Meow a -> IO [String]
exec_ (Meow m) = execWriterT m

meow :: String -> Meow ()
meow x = Meow $ tell [x]
