module Network.Console (
  CompIO
, Comp
, mkComp
, runConsole
, readConsole
, compFilename
, addHist

, compCatch
, compCatch2
, compHFlush
, compPrint
, compPutStr
, compPutStrLn
, compHPutStrLn

, lastWord
) where

import System.Console.Haskeline
import Prelude hiding (catch)
import Control.Monad.Trans
import System.IO
import Data.List (isPrefixOf, isSuffixOf)
import Control.Exception hiding (catch)
import Data.Char (isSpace)

data Comp = Comp (CompletionFunc IO)

type CompIO = InputT IO

runConsole :: Comp -> CompIO a -> IO a
runConsole (Comp cmp) act = runInputT (setComplete cmp defaultSettings) act

--compCatch :: Exception e => CompIO a -> (e -> CompIO a) -> CompIO a
compCatch :: CompIO a -> (IOException -> CompIO a) -> CompIO a
compCatch = catch

compCatch2 :: CompIO a -> (ErrorCall -> CompIO a) -> CompIO a
compCatch2 = catch

compHFlush :: Handle -> CompIO ()
compHFlush = lift . hFlush

mkComp :: (String -> IO [ String ]) -> Comp
mkComp f = Comp $ \(rts,_) -> do
  ss <-f $ reverse rts
  return (dropWhile (not.isSpace) rts, map (\s -> Completion s s $ notIsDir s) ss)
  where notIsDir fp = not $ isSuffixOf "/" fp
{-
mkComp f = Comp $ completeWord Nothing " " $
  \str -> fmap (map (\s -> Completion s s True)) $ f str
  -}

compPutStrLn :: String -> CompIO ()
compPutStrLn = outputStrLn

compHPutStrLn :: Handle -> String -> CompIO ()
compHPutStrLn = (.) lift . hPutStrLn

compPutStr :: String -> CompIO ()
compPutStr = outputStr

readConsole :: String -> CompIO (Maybe String)
readConsole = getInputLine

compFilename :: String -> IO [ String ]
compFilename inp = do
  fs <- fmap (map replacement) $ listFiles inp
  return $ filter (isPrefixOf inp) fs

compPrint :: Show a => a -> CompIO ()
compPrint = lift . print

lastWord :: String -> String
lastWord "" = ""
lastWord ln = if isSpace $ last ln then "" else last $ words ln

addHist :: String -> CompIO ()
addHist _ = return ()
