
module ParseBundle where

import Control.Applicative hiding (many)
import Data.List
import Data.Maybe
import Data.Char
import System.FilePath
import Text.ParserCombinators.ReadP
import Version

parse :: ReadP a -> String -> Maybe a
parse p s =
  case [ x | (x, "") <- readP_to_S p s ] of
    [x] -> Just x
    []  -> Nothing
    xs  -> Nothing

type Env = [(String, String)]

parseFile :: Env -> FilePath -> IO (Maybe Bundle)
parseFile env file = parse (bundleP env (takeFileName file)) <$> readFile file

bundleP :: Env -> Tag -> ReadP Bundle
bundleP env name = toBundle env name <$> packageP <*> implsP

type Package = (FilePath, [FilePath], [FilePath])

toBundle :: Env -> Tag -> Package -> [(Tag, Maybe FilePath)] -> Bundle
toBundle systemEnv name (prefix, files, source) versions = Bundle name inst impls
  where
    inst = Inst { installPath = prefix
                , installPkg  = Pkg files }

    mkMapping tag extra file = (file, map (subst env) source)
      where env = [("TAG", tag), ("TARGET", file)] ++
                  [("SOURCE", path) | Just path <- [extra]] ++
                  systemEnv

    impls = [ impl tag $ map (mkMapping tag path) files
            | (tag, path) <- versions ]

-- subst [("VAR", "VALUE")] "...${VAR}..." = ...VALUE..."
subst :: [(String, String)] -> String -> String
subst env = concatMap sub . tokenize
  where
    sub (Left var)  = fromMaybe ("${" ++ var ++ "}") (lookup var env)
    sub (Right str) = str

    tokenize "" = []
    tokenize ('$' : '{' : s) =
      case break (== '}') s of
        (var, '}' : s') -> Left var : tokenize s'
        _               -> error "Unterminated '{'"
    tokenize ('$' : s) = Right "$" : tokenize s
    tokenize s = Right s0 : tokenize s1
      where (s0, s1) = break (== '$') s

nl :: ReadP ()
nl = () <$ char '\n'

toEOL :: ReadP String
toEOL = munch (== ' ') *> munch1 (/= '\n') <* nl

commaList :: ReadP [String]
commaList = sepBy1 (skipSpaces *> munch1 (`notElem` [',', '\n'])) (char ',')

packageP :: ReadP Package
packageP = (,,) <$ skipSpaces <* string "[package]" <* nl <*
  string "prefix:" <*> toEOL <*
  string "files:" <*> commaList <* nl <*
  string "source:" <*> (option () nl *> many1 toEOL)

implsP :: ReadP [(Tag, Maybe FilePath)]
implsP = skipSpaces *> string "[versions]" *> nl *> many versionP

versionP :: ReadP (Tag, Maybe FilePath)
versionP = (,) <$> munch1 (not . isSpace) <* munch (==' ') <*> (Nothing <$ nl <|> Just <$> toEOL)

