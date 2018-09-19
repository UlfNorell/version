{-# LANGUAGE TupleSections #-}
module Version where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Exception hiding (assert)
import System.FilePath
import System.Directory
import System.Posix as Posix hiding (createDirectory, removeDirectory)
import System.Exit
import Data.List
import Data.Maybe
import Data.Function

-- TODO -------------------------------------------------------------------

-- Use lenses

-- Types ------------------------------------------------------------------

newtype Pkg = Pkg { pkgFiles :: [FilePath] }

data Install = Inst { installPath :: FilePath
                    , installPkg  :: Pkg }

type Tag = String

data Impl = Impl { implName :: Tag
                 , implFiles :: [(FilePath, [FilePath])]
                 , implStatus :: Status }

data Status = Broken | Valid
  deriving (Eq, Show)

data Bundle = Bundle { bundleName   :: Tag
                     , bundleTarget :: Install
                     , bundleImpls  :: [Impl] }

instance Show Pkg where
  show (Pkg fs) = intercalate "," fs

instance Show Install where
  show (Inst p pkg) = p </> "{" ++ show pkg ++ "}"

instance Show Impl where
  show (Impl n xs _) =
    init $ unlines $ n : [ "  " ++ x ++ " -> " ++ concat (take 1 y) | (x, y) <- padFst xs ]

padFst :: [(String, a)] -> [(String, a)]
padFst xs = [ (pad w x, y) | (x, y) <- xs ]
  where
    w = maximum $ 0 : map (length . fst) xs

pad :: Int -> String -> String
pad n s | len < n   = s ++ replicate (n - len) ' '
        | otherwise = s
  where len = length s

instance Show Bundle where
  show (Bundle x i is) =
    unlines' [ x ++ " in " ++ installPath i
             , indent 2 $ unlines' $ map show is ]

comma :: [String] -> String
comma []       = ""
comma [x]      = x
comma [x, y]   = x ++ " and " ++ y
comma (x : xs) = x ++ ", " ++ comma xs

unlines' :: [String] -> String
unlines' = intercalate "\n"

indent :: Int -> String -> String
indent n = unlines' . map (replicate n ' ' ++) . lines

impl :: Tag -> [(FilePath, [FilePath])] -> Impl
impl t fs = Impl t fs (error "unknown status")

tag :: Tag -> Impl -> Impl
tag x i = i { implName = x }

-- File I/O ---------------------------------------------------------------

data FileInfo = RegularFile
              | Directory
              | SymbolicLink FilePath
              | Unknown
              | NonExisting
  deriving (Eq)

instance Show FileInfo where
  show RegularFile      = "a file"
  show Directory        = "a directory"
  show (SymbolicLink t) = "a symbolic link to " ++ t
  show NonExisting      = "non-existing"
  show Unknown          = "an unknown entity"

getFileInfo :: FilePath -> IO FileInfo
getFileInfo file = do
  status <- (Just <$> getSymbolicLinkStatus file) `catchIO_` return Nothing
  let res = case status of
              Nothing -> return NonExisting
              Just status
                | Posix.isSymbolicLink status -> SymbolicLink <$> readSymbolicLink file
                | isRegularFile status  -> return RegularFile
                | isDirectory status    -> return Directory
                | otherwise             -> return Unknown
  res

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

catchIO_ :: IO a -> IO a -> IO a
catchIO_ m h = catchIO m (const h)

removeFile' :: FilePath -> IO ()
removeFile' file = removeFile file `catchIO_` return ()

fail' :: String -> IO a
fail' err = putStrLn err >> exitWith (ExitFailure 1)

-- Validation -------------------------------------------------------------

exists :: FilePath -> IO Bool
exists file = do
  info <- getFileInfo file
  return $ case info of
    RegularFile    -> True
    Directory      -> True
    SymbolicLink _ -> True
    NonExisting    -> False
    Unknown        -> False

validateImpl :: Bool -> Pkg -> Impl -> IO Impl
validateImpl verbose p i = do
  let match = sort (pkgFiles p) == sort (map fst $ implFiles i)
  unless match $ putStrLn $
    "File mismatch between " ++ show p ++ " and\n" ++ show i
  files' <- mapM (\ (tgt, srcs) -> (tgt,) <$> filterM exists srcs) (implFiles i)
  let bad = filter (null . snd) files'
  unless (null bad || not verbose) $ putStrLn $
    "Missing targets:\n  " ++ intercalate "\n  " (map fst bad)
  let status | match && null bad = Valid
             | otherwise         = Broken
  return $ i { implFiles  = files'
             , implStatus = status }

assert :: Bool -> String -> IO ()
assert False err = fail' err
assert True  _   = return ()

assertM :: IO Bool -> String -> IO ()
assertM ok err = (`assert` err) =<< ok

validateBundle :: Bool -> Bundle -> IO Bundle
validateBundle verbose b = do
  let ipath = installPath $ bundleTarget b
  assertM (doesDirectoryExist ipath) $
    "Target directory " ++ ipath ++ " does not exist"
  is <- mapM (validateImpl verbose $ installPkg $ bundleTarget b) (bundleImpls b)
  mapM_ (checkOverlap is) (pkgFiles $ installPkg $ bundleTarget b)
  return b { bundleImpls = is }

checkOverlap :: [Impl] -> FilePath -> IO ()
checkOverlap is file = do
  ts <- mapM (canonicalizePath . head . fromJust . lookup file . implFiles)
          $ filter ((== Valid) . implStatus) is
  let bad = ts |> zip (map implName is)
               |> sortBy (compare `on` snd)
               |> groupBy ((==) `on` snd)
               |> filter ((> 1) . length)
               |> map (map fst &&& snd . head)
  case bad of
    [] -> return ()
    _  -> fail' $ unlines' $
            "Overlapping targets for:" :
            [ "  " ++ file ++ " in " ++ comma is ++ " (" ++ t ++ ")"
              | (is, t) <- bad ]
  where

infixl 0 |>
(|>) = flip ($)

-- Installation -----------------------------------------------------------

data InstalledVersion
  = Installed Tag
  | BadInstall [(FilePath, FileVersion)]
  deriving (Eq)

data FileVersion
  = FromImpl Tag
  | UnknownImpl FilePath
  | NotALink
  | Missing
  deriving (Eq)

instance Show FileVersion where
  show (FromImpl x)    = x
  show (UnknownImpl t) = "unknown target " ++ t
  show NotALink        = "not a symbolic link"
  show Missing         = "missing"

instance Show InstalledVersion where
  show (Installed x) = x
  show (BadInstall bad) =
    unlines' $
      "Bad install:" :
      [ "  " ++ f ++ " : " ++ show v | (f, v) <- padFst bad ]

fromImpl :: FilePath -> [Tag] -> FileVersion
fromImpl f []  = UnknownImpl f
fromImpl f [x] = FromImpl x
fromImpl f xs  = error $ "fromImpl called with multiple tags: " ++ show xs

hasMapping :: FilePath -> FilePath -> Impl -> IO Bool
hasMapping src tgt i
  | implStatus i == Broken = return False
  | otherwise              =
    case lookup src $ implFiles i of
      Nothing      -> return False
      Just (t : _) -> (==) <$> canonicalizePath t <*> canonicalizePath tgt
      Just []      -> return False

fileVersion :: Bundle -> FilePath -> IO FileVersion
fileVersion b file = do
  let path = installPath (bundleTarget b)
  assert (elem file $ pkgFiles $ installPkg $ bundleTarget b) $ file ++ " is not a member of " ++ show (bundleTarget b)
  info <- getFileInfo $ path </> file
  case info of
    RegularFile -> return NotALink
    Directory   -> return NotALink
    Unknown     -> return NotALink
    NonExisting -> return Missing
    SymbolicLink t -> do
      let absTgt = dropFileName (path </> file) </> t
      fromImpl absTgt . map implName
        <$> filterM (hasMapping file $ absTgt) (bundleImpls b)

checkInstalled :: Bundle -> IO InstalledVersion
checkInstalled b = do
  let files = pkgFiles $ installPkg $ bundleTarget b
  fvs <- mapM (fileVersion b) files
  return $ case nub fvs of
    [FromImpl i] -> Installed i
    _            -> BadInstall $ zip files fvs

data Force = NoForce | YesForce
  deriving (Eq)

doInstall :: Force -> Bundle -> Tag -> IO ()
doInstall force b tag = do
  assert (elem tag $ map implName $ bundleImpls b) $ tag ++ " is not a member of bundle " ++ show b
  let Just i = find ((== tag) . implName) (bundleImpls b)
  inst <- checkInstalled b
  case inst of
    _ | implStatus i == Broken ->
      putStrLn "Cannot install broken version."
    BadInstall bad | force == NoForce, any (notLink . snd) bad -> do
      putStrLn $ "Cannot install " ++ tag ++ " in " ++ (installPath $ bundleTarget b) ++
                 "\nwithout using force on " ++ comma (map fst $ filter (notLink . snd) bad) ++ "."
    _ -> do
      let full = (installPath (bundleTarget b) </>)
          install file = do
            removeFile' (full file)
            let Just (t : _) = lookup file $ implFiles i
            createSymbolicLink t (full file)
      mapM_ install $ pkgFiles $ installPkg $ bundleTarget b
      putStrLn $ "Installed " ++ bundleName b ++ " version " ++ tag ++ "."
  where
    notLink NotALink = True
    notLink _        = False

bundleInfo :: Bool -> Bundle -> IO ()
bundleInfo verbose b = do
  i <- checkInstalled b
  case i of
    BadInstall{} -> print i
    Installed{}  -> return ()
  let isInst imp = Installed (implName imp) == i
  putStrLn $ unlines' $ (bundleName b ++ " in " ++ installPath (bundleTarget b)) :
    [ "  " ++ implName i ++ concat (
      [ " (installed)" | isInst i ] ++
      [ " (broken)"    | implStatus i == Broken ] ++
      [ "\n" ++ indent 2 (unlines' $ tail $ lines $ show i) | verbose ])
    | i <- bundleImpls b ]

