module Main where

import System.Environment (getArgs)
import Paths_yjftp        (getDataFileName, version)
import Data.Version       (showVersion)
import Network.Yjftp      (runYjftp, defaultCommandList)
import HsConfigure        (runUsersEx)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "--version" ] -> putStrLn $ "yjftp " ++ showVersion version
    _               -> do
      src <- getDataFileName "yjftp.hs"
      runUsersEx "yjftp" (Just version) (Just src) (runYjftp defaultCommandList)
