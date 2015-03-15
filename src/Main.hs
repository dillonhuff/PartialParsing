module Main(main) where

import Control.Monad (forM, liftM, mapM)
import Data.List as L
import System.Directory
import System.FilePath ((</>))
import System.IO

import CodeError

projectDir = "/Users/dillon/linux/linux/"

main :: IO ()
main = do
  source <- allSourceFilesInDir projectDir
  errors <- liftM concat $ mapM checkForErrors source
  putStrLn $ show $ errors

checkForErrors :: FilePath -> IO [CodeError]
checkForErrors path = do
  fileH <- openFile path ReadMode
  fileContents <- hGetContents fileH
  let errors = findErrors fileContents in
    do
      hClose fileH
      return errors

allSourceFilesInDir :: FilePath -> IO [FilePath]
allSourceFilesInDir dir = do
  fileNames <- getRecursiveContents dir
  let sourceFileNames = L.filter hasAcceptableSuffix fileNames in
    return sourceFileNames

acceptableSuffixes = [".cpp", ".c", ".h", ".hpp"]

hasAcceptableSuffix :: FilePath -> Bool
hasAcceptableSuffix path = L.elem suffix acceptableSuffixes
  where
    suffix = dropWhile (\x -> x /= '.') path

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", "..", ".git"]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

{-  (b :/ dt) <- readDirectory projectDir
  putStrLn "DONE READING"
  let flatDir = flattenDir dt
      sourceFiles = L.filter hasAcceptableSuffix flatDir
      strs = map name sourceFiles
      res = F.concat strs in
    putStrLn $ show $ res


hasAcceptableSuffix :: DirTree String -> Bool
hasAcceptableSuffix (Failed _ _) = False
hasAcceptableSuffix (Dir _ _) = False
hasAcceptableSuffix (File name _) = L.elem suffix acceptableSuffixes
  where
    suffix = dropWhile (\x -> x /= '.') name

replaceFailWithDummy :: FileName -> IOException -> DirTree String
replaceFailWithDummy fileName e = File fileName $ "Failed file read: " ++ show e
-}
