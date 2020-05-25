{-# LANGUAGE BlockArguments #-}

module Homework.Pipes.Producer where

import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.Trans.State
import Data.Function
import Pipes
import qualified Pipes.Prelude as P
import System.IO (isEOF)
import Text.Read (readMaybe)

stdinLn :: Producer' String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof do
    str <- lift getLine
    yield str
    stdinLn

stdinLn' :: Producer' String IO Int
stdinLn' = fmap (const 1) do
  eof <- lift isEOF
  unless eof do
    str <- lift getLine
    yield str

-- forall y' y. Proxy () String y' y IO ()
stdoutLn :: Consumer' String IO ()
stdoutLn = do
  str <- await
  lift do putStrLn str
  stdoutLn

main :: IO ()
main = flip evalStateT (0, 0) do runEffect loop

loop :: Effect (StateT (Int, Int) IO) ()
loop = do
  eof <- liftIO isEOF
  unless eof do
    str <- liftIO getLine
    case (readMaybe str :: Maybe Int) of
      Just x -> lift (modify do (+ x) *** (+ 1))
      _ -> pure mempty
    (n', d') <- lift get
    (liftIO . \_ -> print do fromIntegral n' / fromIntegral d' :: Double) str
    loop

loop' :: StateT (Int, Int) IO ()
loop' = do
  eof <- liftIO isEOF
  unless eof do
    str <- liftIO getLine
    (n, d) <- get
    case (readMaybe str :: Maybe Int) of
      Just x -> put (x + n, d + 1)
      _ -> pure mempty
    (n', d') <- get
    (liftIO . \_ -> print do fromIntegral n' / fromIntegral d' :: Double) str
    loop'
