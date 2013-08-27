{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

import Control.Applicative ((<$>), pure)
import Control.Exception (catch, catchJust, IOException)
import Control.Monad (filterM, guard, liftM, unless)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory
    ( createDirectoryIfMissing, getCurrentDirectory, getDirectoryContents
    , doesFileExist, removeDirectoryRecursive, renameFile, removeFile
    , setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath
    (hasExtension, replaceBaseName, splitFileName, takeBaseName, (</>))
import System.IO (IOMode(..), hFileSize, hGetLine, hPutStrLn, stderr, withFile)
import System.IO.Error (IOErrorType(..), ioeGetErrorType, isDoesNotExistError)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import Text.Printf (printf)

import qualified Data.Digest.Pure.MD5 as MD5

traceShow' :: Show a => a -> a
traceShow' arg = traceShow arg arg

metaDir :: String
metaDir = ".redo"

main :: IO ()
main =
    getCurrentDirectory >>= \top_dir ->
    getArgs             >>=
    mapM_ (\arg ->
        let (dir, file) = splitFileName arg
        in setCurrentDirectory dir >>
           redo file dir >>
           setCurrentDirectory top_dir) >>
    getProgName                 >>= \prog_name ->
    lookupEnv "REDO_TARGET"     >>= \redo_target ->
    case (prog_name, redo_target) of
        ("redo-ifchange", Just redoTarget') -> getArgs >>= mapM_ (writeMD5 redoTarget')
        ("redo-ifchange", Nothing)          -> error "Missing REDO_TARGET environment variable."
        _ -> pure ()

redo :: String -> FilePath -> IO ()
redo target dir =
    upToDate target >>= \is_up_to_date ->
    unless is_up_to_date (
        doPath target >>=
        maybe missingDo redo')
  where
    redo' :: FilePath -> IO ()
    redo' path =
        hPutStrLn stderr ("redo " ++ (show $ dir </> target)) >>
        catchJust (guard . isDoesNotExistError)
                      (removeDirectoryRecursive deps_dir)
                      (const $ return ())          >>
            createDirectoryIfMissing True deps_dir >>
            writeMD5 target path                   >>
            getEnvironment                         >>= \old_env ->
            let
                newEnv = toList $
                             adjust (++ ":.") "PATH" $
                                 insert "REDO_TARGET" target $ fromList old_env
            in
                createProcess ((shell $ cmd path) { env = Just newEnv }) >>= \(_, _, _, handle) ->
                waitForProcess handle >>=
                \case
                    ExitSuccess      ->
                        fileSize tmp >>= \size ->
                        if size > 0
                            then renameFile tmp target
                            else removeFile tmp
                    ExitFailure code ->
                        hPutStrLn stderr ("Redo script exited with non-zero exit code " ++ show code) >>
                        removeFile tmp >>
                        exitFailure

    tmp :: FilePath
    tmp = target ++ "---redoing"

    deps_dir :: FilePath
    deps_dir = metaDir </> target

    missingDo =
        doesFileExist target >>= \target_exists ->
        unless target_exists (
            error $ printf "No .do file found for target '%s'" target)

    cmd :: FilePath -> String
    cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe <$> filterM doesFileExist candidates
   where candidates = (target ++ ".do")
                    : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate target = catch
   (do
      exists <- doesFileExist target
      if exists
         then do
            md5s <- getDirectoryContents (metaDir </> target)
            and `liftM` mapM depUpToDate md5s
         else return False
   )
   (\(_ :: IOException) -> return False)

   where depUpToDate :: String -> IO Bool
         depUpToDate oldMD5 = catch
            (do
               dep <- withFile (metaDir </> target </> oldMD5) ReadMode hGetLine
               newMD5 <- fileMD5 dep
               doScript <- doPath dep
               case doScript of
                  Nothing -> return $ oldMD5 == newMD5
                  Just _ -> do
                     upToDate' <- upToDate dep
                     return $ (oldMD5 == newMD5) && upToDate')
            (\e -> return $ ioeGetErrorType e == InappropriateType)

fileMD5 :: FilePath -> IO String
fileMD5 path = (show . MD5.md5) `liftM` BL.readFile path


writeMD5 :: FilePath -> FilePath -> IO ()
writeMD5 redoTarget dep =
    fileMD5 dep >>= \md5 ->
    writeFile (metaDir </> redoTarget </> md5) dep

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
