{-# LANGUAGE DatatypeContexts, LambdaCase #-}

module RunProcessSimple where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process (ProcessStatus)
import System.Posix.IO
import System.Posix.Types (Fd)

-- command, args
type SysCommand = (String, [String])

-- the result of any running command
data CommandResult = CommandResult
    { cmdOutput     :: IO String
    , getExitStatus :: IO ProcessStatus
    }

-- The type for handling global lists of FDs to always close in the children.
type CloseFDs = MVar [Fd]

-- Class representing anything that is a runnable command
class CommandLike a where
    -- Given the command and a String representing input, invokes the command.
    -- Returns a String representing the output of the command.
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        createPipe >>= \(stdin_read, stdin_write) ->  -- Handle stdin
        createPipe >>= \(stdout_read, stdout_write) -> -- Handle stdout

        -- Add the parent FDs to this list because we always need to close them
        -- in the children.
        addCloseFDs closefds [stdin_write, stdout_read] >>

        -- Grab the closed FDs list and fork the child.
        withMVar closefds (\fds -> fordProcess (child fds stdin_read, stdout_write)) >>= \childPID ->

        -- Not, on the parent, close the child-side FDs.
        closeFd stdin_read >>
        closeFd stdout_write >>

        -- Write the input to the command.
        fdToHandle stdin_write >>= \stdin_handle ->
        forkIO $
            hPutStr stdin_handle input >>
            hClose stdin_handle

        -- Prepare to receive output from the command.
        fdToHandle stdout_read >>= \stdout_handle ->

        return $ CommandResult { cmdOutput     = hGetContents stdout_handle
                               , getExitStatus = waitfunc
                               }
      where
          -- Set up the function to call when ready to wait for the child to exit.
        waitfunc =
            getProcessStatus True False childPID >>=
                \case
                    Just ps -> removeCloseFDs closefds [stdin_write, stdout_read] >>
                               return ps
                    Nothing -> fail "Error: Nothing from getProcessStatus"
        -- What happens in the child process.
        child closefds stdin_read stdout_write =
            -- Copy our pipes over the regular stdin/stdout FDs
            dupTo stdin_read stdInput >>
            dupTo stdout_write stdOutput >>

            -- Close the original pipe FDs.
            closeFd stdin_read >>
            closeFd stdout_write >>

            -- Close all the open FDs we inherited from the parent
            mapM_ (\fd -> catch (closeFd fd) (const $ return ())) closefds

            -- Start the program
            executeFile cmd True args Nothing

-- Add FDs to the list of FDs that must be closed post-fork in a child.
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds = modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- Remove FDs from the list
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds to_remove =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist to_remove)
  where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first offuccence of any given fd
    removeFd [] _ = []
    removeFd (x:xs) fd
        | fd == x = xs
        | otherwise = x : removefd xs fd

data (CommandLike src, CommandLike dst) =>
    PipeCommand src dst = PipeCommand src dst

(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

instance (CommandLike a, CommandLike b) => CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dst) closefds input =
        invoke src closefds input >>= \res1 ->
        cmdOutput res1            >>= \output1 ->
        invoke dst closefds output1 >>= \res2 ->
        return $ CommandResult (cmdOutput res2) (getEC res1 res2)

getEC :: CommandResult -> CommandResult -> IO ProcessStatus
getEC src dst =
    getExitStatus src >>= \src_ec ->
    getExitStatus dst >>= \dst_ec ->
    case src_ec of
        Exited ExitSuccess -> return dst_ec
        x                  -> return x

runIO :: CommandLike a => a -> IO ()
runIO cmd =
    newMVar [] >>= \closefds ->
    invoke cmd closefds [] >>= \res ->
    cmdOutput res >>= putStr >>

    getExitStatus res >>=
        \case
            Exited ExitSuccess -> return ()
            x -> fail $ "Exited: " ++ show x
