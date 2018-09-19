
module Main where

import Data.List
import System.Environment
import System.FilePath
import System.Directory
import Version
import ParseBundle

-- Main -------------------------------------------------------------------

getBundles :: IO [Bundle]
getBundles = do
  home <- getEnv "HOME"
  let root = home </> ".version"
  files <- map (root </>) . filter (not . isPrefixOf ".") <$> listDirectory (home </> ".version")
  let getBundle file = do
        mb <- parseFile [("HOME", home)] file
        case mb of
          Nothing -> [] <$ putStrLn ("Warning: failed to parse " ++ file)
          Just b  -> return [b]
  concat <$> mapM getBundle files

getBundle :: (?bundles :: [Bundle]) => Bool -> Tag -> IO Bundle
getBundle verbose x =
  case find ((x ==) . bundleName) ?bundles of
    Just b  -> validateBundle verbose b
    Nothing -> fail' $ "No bundle for " ++ x ++ "."

info :: (?bundles :: [Bundle]) => Bool -> Tag -> IO ()
info verbose b = bundleInfo verbose =<< getBundle verbose b

select' :: (?bundles :: [Bundle]) => Bool -> Force -> Tag -> Tag -> IO ()
select' verbose f b v = flip (doInstall f) v =<< getBundle verbose b

completeBundles :: (?bundles :: [Bundle]) => IO ()
completeBundles = mapM_ (putStrLn . bundleName) ?bundles

completeBundle :: (?bundles :: [Bundle]) => Tag -> IO ()
completeBundle x =
  case find ((x ==) . bundleName) ?bundles of
    Nothing -> return ()
    Just b  -> mapM_ (putStrLn . implName) $ bundleImpls b

main = do
  args <- getArgs
  let force | elem "--force" args = YesForce
            | otherwise           = NoForce
      verbose  = elem "--verbose" args
      complete = elem "--complete" args
  bundles <- getBundles
  let ?bundles = bundles
  case args \\ ["--force", "--verbose", "--complete"] of
    [b] | complete -> completeBundle b
    []  | complete -> completeBundles
    []             -> mapM_ (info verbose . bundleName) bundles
    [b]            -> info verbose b
    [b, v]         -> select' verbose force b v

