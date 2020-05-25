{-# LANGUAGE BlockArguments #-}

module Homework.Pipes.Pipes where

import Control.Applicative ((<$))
import Control.Arrow ((***))
import Control.Monad (forever, replicateM_, unless)
import Control.Monad.Trans.State
import Pipes
import qualified Pipes.Prelude as P
import System.IO
import Text.Read (readMaybe)
import Prelude hiding (take)

-- bar :: Proxy () String () String IO String
-- bar :: Pipe String String IO String
-- bar = evalStateT bar' (0, 0)
--   where
--     bar' :: StateT (Int, Int) (Pipe String String IO) String
--     bar' = do
--       x <- lift await
--       case (readMaybe x :: Maybe Int) of
--         Just n -> do
--           modify ((+ n) *** (+ 1))
--           (x, y) <- get
--           liftIO do print (x `div` y)
--         Nothing -> liftIO (putStrLn "Invalid input!")
--       bar'

runningAverage :: Pipe String Int IO r
runningAverage =
  evalStateT
    ( forever do
        input <- lift await
        case (readMaybe input :: Maybe Int) of
          Just n -> do
            modify ((+ n) *** (+ 1))
            (x, y) <- get
            lift (yield (x `div` y))
          Nothing -> liftIO (putStrLn "Invalid input!!!")
    )
    (0, 0)

main :: IO ()
main = runEffect do
  P.stdinLn
    >-> runningAverage
    >-> P.take 4
    >-> P.show
    >-> P.stdoutLn
