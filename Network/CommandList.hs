module Network.CommandList (
  CommandList
, defaultCommandList
, getAction
, getHelp
, getComp

, quitFTP
, systemFTP
, pwdFTP
, listFTP
, directoryFTP
, changeDirectoryLocal
, changeDirectoryFTP
, getFileFTP
, putFileFTP
, showFileFTP
, removeFileFTP
, makeDirectoryFTP
, removeDirectoryFTP
, moveFileFTP
, copyFileFTP
, editBy
, readBy
, getEnv

, const2
, compRemoteFile
, compLs
, compFilename
) where

import Network.FTP.Client   (FTPConnection, getbinary, putbinary, uploadbinary, downloadbinary,
                             nlst, dir, pwd, cwd, rename, delete, mkdir, rmdir)
import System.IO            (Handle, hClose, hPutStr)
import System.Directory     (setCurrentDirectory, removeFile, getTemporaryDirectory)
import System.Environment   (getEnv)
import System.Cmd           (system)
import System.IO.Error      (isUserError)
import System.Posix.Temp    (mkstemp)
import Data.Maybe           (fromJust)
import Data.List            (isPrefixOf)
import Data.Char            (isSpace)
import Control.OldException    (catchJust, ioErrors, ioError, bracketOnError)

import Network.Console(compFilename)
import Network.Types

defaultCommandList :: CommandList
defaultCommandList = [
   ("q",     (quitFTP, "exit from yjftp", const2 $ return []))
 , ("quit",  (quitFTP, "exit from yjftp", const2 $ return []))
 , ("exit",  (quitFTP, "exit from yjftp", const2 $ return []))
 , ("bye",   (quitFTP, "exit from yjftp", const2 $ return []))
 , ("!",     (systemFTP, "run system command", const2 $ return []))
 , ("pwd",   (pwdFTP, "print where directory you are at", const2 $ return []))
 , ("ls",    (\h args   -> case args of
                             ("-l":args_) -> directoryFTP h args_
                             _            -> listFTP h args ,
			     "list directory contents\n\toption -l list detail of contents",
			     compLs))
 , ("cd",    (changeDirectoryFTP, "change directory in remote", compRemoteFile))
 , ("lcd",   (changeDirectoryLocal, "change directory in local", const compFilename))
 , ("put",   (putFileFTP, "upload file", const compFilename))
 , ("get",   (getFileFTP, "download file", compRemoteFile))
 , ("cat",   (showFileFTP, "show remote file", compRemoteFile))
 , ("rm",    (removeFileFTP, "delete remote file", compRemoteFile))
 , ("mkdir", (makeDirectoryFTP, "make directory in remote", const2 $ return []))
 , ("rmdir", (removeDirectoryFTP, "delete directory in remote", compRemoteFile))
 , ("mv",    (moveFileFTP, "move/change name file in remote", compRemoteFile))
 , ("cp",    (copyFileFTP, "copy file in remote", compRemoteFile))
 , ("edit",  (editBy $ \fn -> do edt <- catch (getEnv "EDITOR") (const $ return "vi")
                                 return $ edt ++ " " ++ fn , "edit by $EDITOR", compRemoteFile))
 , ("show",  (readBy $ \fn -> do pgr <- catch (getEnv "PAGER") (const $ return "less")
                                 return $ pgr ++ " " ++ fn , "show by $PAGER", compRemoteFile))
 ]

const2 :: a -> b -> c -> a
const2 = const . const

quitFTP :: Action
quitFTP = const2 $ return False

systemFTP :: Action
systemFTP = const $ (>> return True) . system . unwords

pwdFTP :: Action
pwdFTP h [] = pwd h >>= putStrLn . fromJust . fst >> return True
pwdFTP _ _  = error "pwdFTP: args incorrect"

listFTP :: Action
listFTP h []          = nlst h Nothing     >>= putStrLn . unwords >> return True
listFTP h [path]      = nlst h (Just path) >>= putStrLn . unwords >> return True
listFTP _ _           = error "listFTP: args incorrect"

directoryFTP :: Action
directoryFTP h []     = dir h Nothing      >>= putStrLn . unlines >> return True
directoryFTP h [path] = dir h (Just path)  >>= putStrLn . unlines >> return True
directoryFTP _ _      = error "directoryFTP: args incorrect"

changeDirectoryFTP :: Action
changeDirectoryFTP h [dirPath] = cwd h dirPath >> return True
changeDirectoryFTP _ _         = error "changeDirectoryFTP: args incorrect"

changeDirectoryLocal :: Action
changeDirectoryLocal _ [dirPath] = setCurrentDirectory dirPath >> return True
changeDirectoryLocal _ _         = error "changeDirectoryLocal: args incorrect"

putFileFTP :: Action
putFileFTP h [file] = uploadbinary h file >> return True
putFileFTP _ _      = error "putFileFTP: args incorrect"

getFileFTP :: Action
getFileFTP h [file] = downloadbinary h file >> return True
getFileFTP _ _      = error "getFileFTP: args incorrect"

showFileFTP :: Action
showFileFTP h [file] = getbinary h file >>= putStr . fst >> return True
showFileFTP _ _      = error "showFileFTP: args incorrect"

removeFileFTP :: Action
removeFileFTP h [file] = delete h file >> return True
removeFileFTP _ _      = error "removeFileFTP: args incorrect"

makeDirectoryFTP :: Action
makeDirectoryFTP h [dr] = mkdir h dr >> return True
makeDirectoryFTP _ _    = error "makeDirectoryFTP: args incorrect"

removeDirectoryFTP :: Action
removeDirectoryFTP h [dr] = rmdir h dr >> return True
removeDirectoryFTP _ _    = error "removeDirectoryFTP: args incorrect"

moveFileFTP :: Action
moveFileFTP h [src, dst] = rename h src dst >> return True
moveFileFTP _ _          = error "moveFileFTP: args incorrect"

copyFileFTP :: Action
copyFileFTP h [src, dst] = getbinary h src >>= flush >>= putbinary h dst . fst >> return True
  where flush s@(c,_) = putStr (take (length c - length c) "dummy") >> return s
copyFileFTP _ _ = error "Usage: cp src dist"

editBy :: (String -> IO String) -> Action
editBy mkEdtr h [fp] =
  bracketOnError (mkTempFile $ "ftpvi_" ++ basename fp)
                 (\(fn, fd) -> hClose fd >> tmpFileSave fn) $ \(fn, fd) -> do
    catchJust ioErrors (getbinary h fp >>= hPutStr fd . fst)
                       (\err -> if isUserError err then return () else ioError err)
    hClose fd
    edt <- mkEdtr fn
    system edt
    readFile fn >>= putbinary h fp
    removeFile fn
    return True
  where tmpFileSave n = putStrLn $ "temp file save as " ++ "'" ++ n ++ "'"
editBy _ _ _ = error "editBy: bad args"

readBy :: (String -> IO String) -> Action
readBy vwr h [fp] =
  bracketOnError (mkTempFile $ "ftpview_" ++ basename fp)
                 (\(fn, fd) -> hClose fd >> removeFile fn) $ \(fn, fd) -> do
    getbinary h fp >>= hPutStr fd . fst
    hClose fd
    pgr <- vwr fn
    system pgr
    removeFile fn
    return True
readBy _ _ _ = error "readBy: bad arguments"

basename :: FilePath -> FilePath
basename = reverse . takeWhile (/='/') . reverse

mkTempFile :: String -> IO (String, Handle)
mkTempFile fn = do
  tmpDir <- getTemporaryDirectory
  mkstemp $ tmpDir ++ "/" ++ fn ++ "-XXXXXX"

compLs :: FTPConnection -> String -> IO [String]
compLs h strGen
  | elem '/' str = do
      let d = reverse $ dropWhile (/='/') $ reverse str
      fns <- myNlst h $ Just d
      return $ addExcess $ filter (isPrefixOf str) $ "-l" : map (d++) fns
  | otherwise = do
      fns <- myNlst h Nothing
      return $ addExcess $ filter (isPrefixOf str) $ "-l" : fns
  where str = strGen -- lastWord strGen

compRemoteFile :: FTPConnection -> String -> IO [String]
compRemoteFile h strGen
  | elem '/' str = do
      let d = reverse $ dropWhile (/='/') $ reverse str
      fns <- myNlst h $ Just d
      return $ addExcess $ filter (isPrefixOf str) $ map (d++) fns
  | otherwise = do
      fns <- myNlst h Nothing
      let ret = addExcess $ filter (isPrefixOf str) fns
--      putStr "\nDEBUG2_: "; print strGen
--      putStr "DEBUG3: "; print ret
      return ret
  where str = strGen {- if isSpace $ last strGen
                 then ""
		 else lastWord strGen -}

addExcess :: [String] -> [String]
addExcess [""] = [""]
addExcess [fp]
  | last fp == '/' = [fp, fp++" "]
  | otherwise      = [fp]
addExcess fps = fps

myNlst :: FTPConnection -> Maybe String -> IO [String]
myNlst h str = do
    cnt <- dir h str
    return $ map mkRet cnt
  where
  mkRet ('d':rest) = last (words rest) ++ "/"
  mkRet ('-':rest) = last (words rest)
  mkRet ('l':rest) = last (words rest)
  mkRet ('c':rest) = last (words rest)
  mkRet _          = error "myNlst error"
