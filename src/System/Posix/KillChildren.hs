-- This is a hack because you can't make runProcess handle ^C correctly right
-- now.  A Set of ProcessHandle's would be cleaner but there is no
-- Ord ProcessHandle even though it seems like there could be.  :(

module System.Posix.KillChildren (
  KillChildrenSt, initKillChildren, killInsChild, killDelChild) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap (IntMap)
import Data.Maybe
import System.Exit
import System.Posix
import System.Process
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap

type KillChildrenSt = TVar (IntMap ProcessHandle)

initKillChildren :: IO KillChildrenSt
initKillChildren = do
  mainThreadId <- myThreadId
  kSt <- atomically (newTVar IntMap.empty)
  installHandler sigINT (Catch $ sigINTHandler mainThreadId kSt) Nothing
  return kSt

killInsChild :: KillChildrenSt -> ProcessHandle -> IO Int
killInsChild kSt h = atomically $ do
  m <- readTVar kSt
  let
    i = fromMaybe 0 $ (+ 1) . fst . fst <$> IntMap.maxViewWithKey m
  writeTVar kSt $ IntMap.insert i h m
  return i

killDelChild :: KillChildrenSt -> Int -> IO ()
killDelChild kSt i = atomically $
  writeTVar kSt . IntMap.delete i =<< readTVar kSt

sigINTHandler :: ThreadId -> KillChildrenSt -> IO ()
sigINTHandler mainThreadId kSt = do
  Fold.mapM_ terminateProcess =<< atomically (readTVar kSt)
  throwTo mainThreadId (ExitFailure 1)
