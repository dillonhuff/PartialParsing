module Main(main) where

import Data.Foldable as F
import System.Directory.Tree

projectDir = "/Users/dillon/dxter/src"

main :: IO ()
main = do
  (b :/ dt) <- readDirectory projectDir
  let strs = fmap (take 10) dt
      res = F.concat strs in
    putStrLn $ show $ res
