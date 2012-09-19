module Main where

import Network.Yjftp

myCommandList :: CommandList
myCommandList = [
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

main :: IO ()
main = runYjftp myCommandList
