{-# LANGUAGE BlockArguments #-}

module Homework.Pipes.Consumer where

import Control.Exception (throwIO, try)
import Control.Monad (unless)
import qualified GHC.IO.Exception as G
import Pipes

stdoutLn :: Consumer String IO ()
stdoutLn = do
  str <- await
  -- lift . putStrLn $ str
  x <- (lift . try . putStrLn) str
  case x of
    Left e@G.IOError {G.ioe_type = t} ->
      (lift . unless (t == G.ResourceVanished) . throwIO) e
    Right () ->
      stdoutLn

main :: IO ()
main = runEffect do lift getLine >~ stdoutLn
