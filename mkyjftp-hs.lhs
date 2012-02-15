#!/usr/bin/env runhaskell

> import Text.RegexPR
> import Maybe (fromJust)
> import System.Directory (getModificationTime, doesFileExist)
> import Control.Monad    (when)

> moduleCL = "Network/CommandList.hs"
> usrsSrc  = "yjftp.hs"

> main = whenM (needUpdate moduleCL usrsSrc) $ do
>   myCL <- getMyCommandList
>   writeFile usrsSrc $ header ++ myCL ++ footer

> getMyCommandList :: IO String
> getMyCommandList
>  = readFile moduleCL >>=
>      return . ("myCommandList :: CommandList\nmyCommandList = " ++) . (++"\n")
>             . fromJust . lookup 1 . snd . fromJust
>             . matchRegexPR "defaultCommandList\\s+\\=\\s+(\\[(.|\\s)*?^ \\])"

> header :: String
> header = "module Main where\n\nimport Network.Yjftp\n\n"

> footer :: String
> footer = "\nmain :: IO ()\nmain = runYjftp myCommandList\n"

> needUpdate :: FilePath -> FilePath -> IO Bool
> needUpdate src dst = do
>   ifM (doesFileExist dst) ( do
>     st <- getModificationTime src
>     dt <- getModificationTime dst
>     return $ st > dt )
>     ( return True )

> ifM :: Monad m => m Bool -> m a -> m a -> m a
> ifM p t e = do b <- p
>                if b then t else e

> whenM :: Monad m => m Bool -> m () -> m ()
> whenM p t = do b <- p
>                when b t
