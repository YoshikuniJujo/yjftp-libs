module Main where

import System.Environment (getArgs)
import Paths_yjftp_libs   (version)
import Data.Version       (showVersion)
import Network.Yjftp      (runYjftp)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "--version" ] -> putStrLn $ "yjftp-ni " ++ showVersion version
    _               -> runYjftp
