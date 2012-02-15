module Network.Types (
  CommandList
, Command
, Action
, getAction
, getHelp
, getComp
) where

import Network.FTP.Client

type Action      = FTPConnection -> [ String ] -> IO Bool
type Command     = (Action, String, FTPConnection -> String -> IO [String])
type CommandList = [(String, Command)]

getAction :: Command -> Action
getAction (act, _,  _) = act
getHelp   :: Command -> String
getHelp   (_, help, _) = help
getComp   :: Command -> FTPConnection -> String -> IO [ String ]
getComp   (_, _, comp) = comp
