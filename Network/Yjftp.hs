module Network.Yjftp (
  runYjftp
-- , module Network.CommandList
) where

-- import Network.CommandList
import Network.FTP.Client   (FTPConnection, enableFTPDebugging, easyConnectFTP, login, loginAnon,
                             quit, cwd, uploadbinary, downloadbinary)
import System.IO            (hFlush, stdin, stdout,
                             hGetBuffering, hSetBuffering, BufferMode(NoBuffering))
import System.Directory     (setCurrentDirectory)
import System.Exit          (exitFailure)
import System.Environment   (getArgs)
import System.Posix.IO      (stdOutput)
import System.Posix.Terminal(getTerminalAttributes, setTerminalAttributes, withoutMode,
                             TerminalState(Immediately), TerminalMode(EnableEcho))
import Control.OldException    (catch, Exception)
import Control.Monad        (when, unless)
import Control.Applicative  ((<$>))
import Prelude hiding       (catch)
-- import Data.Char            (isSpace, isAscii)
-- import Data.List            (isPrefixOf)
-- import Network.Console(addHist, readConsole, runConsole, CompIO, compPutStr, compHFlush, compPrint, compHPutStrLn, Comp, mkComp, lastWord, compCatch, compCatch2)
-- import Control.Monad.Trans

runYjftp :: IO ()
runYjftp = do
  (act, src, srvr, usr, dr, pswd) <- processArgs
  h <- connectNlogin srvr usr pswd
  whenMaybe dr $ (>> return ()) . cwd h 
  case (act, src) of
    (Just Put, Just s) -> do unless (dirname s == "") $ setCurrentDirectory $ dirname s
                             uploadbinary h (basename s) >> return ()
    (Just Get, Just s) -> downloadbinary h s >> return ()
    _ -> error "bad arguments"
{-
    (Nothing, _) -> runConsole (myComp h cl) $ do
      doWhile_ $ do
        compHFlush stdout
        cmdln <- readConsole "> "
	maybe (return ()) addHist cmdln
        case myWords <$> cmdln of
	  Nothing         -> return False
          Just []         -> return True
	  Just ["?"]      -> mapM (\cmd -> compPutStr $ fst cmd ++ ":\t" ++ getHelp (snd cmd) ++ "\n") cl >> return True
          Just (cmd:args) -> flip compCatch2 ((>> return True) . compPrint) $ compCatch (executeCommand cl h cmd args) ((>> return True) . compPrint)
    _ -> error "bad argument (put/get)"
-}
  _ <- quit h
  return ()

connectNlogin :: Maybe String -> Maybe String -> Maybe String -> IO FTPConnection
connectNlogin mAddr mUsr pswd =
  case (mAddr, mUsr) of
       (Just addr, Just usr) -> do h <- easyConnectFTP addr
                                   _ <- maybe
				     (getPasswordNLogin h usr)
                                     (flip (login h usr) Nothing . Just) pswd
                                   return h
       (Just addr, Nothing)  -> do h <- easyConnectFTP addr
                                   _ <- loginAnon h
                                   return h
       (Nothing, Nothing)    -> do putStr "FTP SERVER ADDRESS: "
                                   hFlush stdout
                                   h <- getLine >>= easyConnectFTP
                                   putStr "USER NAME         : "
                                   hFlush stdout
                                   usr <- getLine
				   _ <- getPasswordNLogin h usr
                                   return h
       _                     -> error "bad pattern of address and user"
  where passwdError = "login error: password may not be correct\n"
        getPasswordNLogin h usr = tryNTimes 3 (const $ putStrLn passwdError) $ do
	                            psswd <- getPassword
			            login h usr (Just psswd) Nothing

tryNTimes :: Int -> (Exception -> IO a) -> IO b -> IO b
tryNTimes 0 _ _      = exitFailure
tryNTimes n errM act
  = if (n < 0) then error "tryNTimes: bad! minus times trial?"
               else catch act (\err -> errM err >> tryNTimes (n-1) errM act)

{-
executeCommand :: CommandList -> FTPConnection -> String -> [String] -> CompIO Bool
executeCommand cl h cmd args
  = maybe (compHPutStrLn stderr ("No such command: " ++ cmd) >> return True)
          (\c -> liftIO $ (getAction c) h args) (lookup cmd cl)
-}

processArgs ::
  IO (Maybe CLAction, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String)
processArgs = do
      args <- getArgs
      when (elem "-v" args || elem "--verbose" args)  enableFTPDebugging
      return $ let pswd  = snd <$> takeOptNArg "-p" args
                   args_ = dropOptNArg "-p" $ filter ((/="-v") &&& (/="--verbose")) args in
             case args_ of
                  [] -> (Nothing, Nothing, Nothing, Nothing, Nothing, pswd)
                  [srvr] -> (Nothing, Nothing, Just $ getSrvr srvr, Nothing, getDir $ head args_, pswd)
                  ["put", src, srvr, usr] -> (Just Put, Just src, Just $ getSrvr srvr, Just usr, getDir srvr, pswd)
                  ["get", srvr, usr] -> (Just Get, Just (basename srvr), Just $ getSrvr srvr,
                                         Just usr, getDir srvr >>= return . dirname, pswd)
                  ["get", srvr]      -> (Just Get, Just $ basename srvr, Just $ getSrvr srvr,
                                         Nothing , maybe Nothing (Just . dirname) $ getDir srvr,pswd)
                  [_,_] -> (Nothing, Nothing, Just $ getSrvr $ args_ !! 0, Just $ args_ !! 1, getDir $ head args_, pswd)
		  _     -> error "bad args"
  where getSrvr :: String -> String
        getSrvr = takeWhile (/='/')
        getDir :: String -> Maybe String
        getDir srvrDir = case dropWhile (/='/') srvrDir of
                              ""   -> Nothing
                              dr   -> Just dr

getPassword :: IO String
getPassword = do
  putStr "PASSWORD          : "
  hFlush stdout
  psswd <- getLineP
  return psswd

{-
doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do b <- act
                  if b then doWhile_ act
                       else return ()
-}

doWhile :: a -> (a -> IO (a, Bool)) -> IO a
doWhile i act = do (r,p) <- act i
                   if p then doWhile r act
                        else return r

getLineP :: IO String
getLineP = do
  bi <- hGetBuffering stdin
  bo <- hGetBuffering stdout
  ta <- getTerminalAttributes stdOutput
  setTerminalAttributes stdOutput (withoutMode ta EnableEcho) Immediately
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  str <- doWhile "" $ \s -> do
    c <- getChar
    if c /= '\n'
       then if c /= '\DEL'
               then do putChar '*'
                       return (c:s, True)
               else do if null s then return ("", True)
                                 else do putChar '\b' >> putChar ' ' >> putChar '\b'
                                         return (tail s, True)
       else do putChar '\n'
               return (s, False)
  setTerminalAttributes stdOutput ta Immediately
  hSetBuffering stdin bi
  hSetBuffering stdout bo
  return $ reverse str

basename :: FilePath -> FilePath
basename = reverse . takeWhile (/='/') . reverse

dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (/='/') . reverse

data CLAction = Put | Get deriving Show

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(p1 &&& p2) x = p1 x && p2 x

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing  _   = return ()
whenMaybe (Just x) act = act x

takeOptNArg :: String -> [String] -> Maybe (String, String)
takeOptNArg _ []  = Nothing
takeOptNArg _ [_] = Nothing
takeOptNArg opt (a:as)
  | opt == a  = Just (a, head as)
  | otherwise = takeOptNArg opt as

dropOptNArg :: String -> [String] -> [String]
dropOptNArg _ []  = []
dropOptNArg _ [x] = [x]
dropOptNArg opt (a:as)
  | opt == a  = tail as
  | otherwise = a : dropOptNArg opt as

{-
myWords :: String -> [String]
myWords ""                 = []
myWords ('!':cs)           = "!" : myWords cs
myWords str@(c:cs)
  | isWordHead c           = takeWhile isNotSpaceAscii str : myWords (dropWhile isNotSpaceAscii str)
  | isSpace c              = myWords cs
  | otherwise              = error "myWords: maybe your input is not askii"
  where isNotSpaceAscii c_ = isAscii c_ && not (isSpace c_)
	isWordHead c_      = isNotSpaceAscii c_ && (c_ /= '!')

myComp :: FTPConnection -> CommandList -> Comp
myComp h cl = mkComp $ myCompFunc h cl

myCompFunc :: FTPConnection -> CommandList -> String -> IO [String]
myCompFunc h cl strGen = do
  let bf  = strGen
      str = lastWord strGen
--  putStr "\nDEBUG1: "; print bf
  case bf of
    ""    -> return $ filter (isPrefixOf str) $ map fst cl
    _ | and (map (not . isSpace) bf) && head bf /= '!'
          -> return $ filter (isPrefixOf str) $ map fst cl
      | otherwise
          -> case getComp <$> lookup (head $ words bf) cl of
	          Nothing -> return []
		  Just f  -> f h str
-}
