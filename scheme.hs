module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readDec, readHex, readInt)
import Data.Char (toLower, toUpper)
import Data.List (findIndex)

data LispNumber = LInteger Integer
                | LRational Rational
                | LRealShort Float
                | LRealSingle Float
                | LRealDouble Double
                | LRealLong Double
                | LExactComplex Rational Rational
                | LInexactComplex Double Double
                  deriving Show

data LispVal = Symbol String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNumber
             | String String
             | Bool Bool
             | Character Char
             | Vector [LispVal]
               deriving Show

data Exactness = Exact | Inexact | Unknown deriving (Eq, Show)
data Precision = Short | Single | Double | Long deriving (Eq, Show)
type Reader = (String -> [(Integer, String)], Integer)

newExactness :: Exactness -> Bool -> Exactness
newExactness Inexact _     = Inexact
newExactness Exact   _     = Exact
newExactness _       False = Inexact
newExactness e       True  = e

(&) :: Exactness -> Exactness -> Exactness
Unknown & e = e
a & b       = a

symbolChar :: Parser Char
symbolChar = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do char '#'
               b <- oneOf "tTfF"
               let boolean = toLower b == 't'
               return . Bool $ boolean

parseChar :: Parser LispVal
parseChar = do char '#'
               char '\\'
               name <- many (noneOf " ")
               let character = case map toLower name of
                                 ""        -> ' '
                                 "space"   -> ' '
                                 "newline" -> '\n'
                                 _         -> head name
               return . Character $ character

parseVector :: Parser LispVal
parseVector = do char '#'
                 char '('
                 v <- liftM Vector $ sepBy parseExpr spaces
                 char ')'
                 return v

parsePrefixExactness :: Parser Exactness
parsePrefixExactness = do char '#'
                          e <- oneOf "eEiI"
                          let exactness = if toLower e == 'e'
                                          then Exact
                                          else Inexact
                          return exactness

parsePrefixBase :: Parser Reader
parsePrefixBase = do char '#'
                     b <- oneOf $ prefixes ++ map toUpper prefixes
                     let base = case toLower b of
                                          'b' -> (readBin, 2)
                                          'o' -> (readOct, 8)
                                          'd' -> (readDec, 10)
                                          'x' -> (readHex, 16)
                     return base
    where prefixes = "bodx"
          readBin  = readInt 2 (`elem` "01") (read . return)

parsePrefix :: Parser (Exactness, Reader)
parsePrefix = try exactBase
              <|> try baseExact
              <|> return (Unknown, (readDec, 10))
    where exactBase = do e <- parsePrefixExactness
                         b <- try parsePrefixBase <|> return (readDec, 10)
                         return (e, b)
          baseExact = do b <- parsePrefixBase
                         e <- try parsePrefixExactness <|> return Unknown
                         return (e, b)

hexDigits = "0123456789aAbBcCdDeEfF"

parseSign :: Parser Integer
parseSign = do s <- oneOf "+-"
               if s == '+'
               then return 1
               else return (-1)

parseBareUInteger :: Exactness -> Reader -> Parser (Integer, Exactness)
parseBareUInteger exactness reader = do before <- liftM (fst . head . fst reader) $ many1 digit -- XXX
                                        hashes <- many (char '#')
                                        let base = snd reader
                                        let n = before * base ^ length hashes
                                        return (n, newExactness exactness $ null hashes)

parseBareRational :: Exactness -> Reader -> Parser (Rational, Exactness)
parseBareRational exactness reader = do (n, e) <- parseBareUInteger exactness reader
                                        char '/'
                                        (d, e') <- parseBareUInteger exactness reader
                                        let rat = toRational n / toRational d
                                        return (rat, exactness & e & e')

parseRational :: Exactness -> Reader -> Parser LispVal
parseRational exactness reader = do s <- try parseSign <|> return 1
                                    (d, e) <- parseBareRational exactness reader
                                    let val = if e == Inexact
                                              then Number . LRealDouble $ fromIntegral s * fromRational d
                                              else Number . LRational $ fromIntegral s * d
                                    return val

parseUInteger :: Exactness -> Reader -> Parser LispVal
parseUInteger exactness reader = do s <- try parseSign <|> return 1
                                    (n, e) <- parseBareUInteger exactness reader
                                    let val = if exactness & e == Inexact
                                              then Number . LRealDouble . fromIntegral $ s * n
                                              else Number . LRational . fromIntegral $ s * n
                                    return val


parseBareSufix :: Exactness -> Reader -> Parser (Precision, Integer, Exactness)
parseBareSufix exactness reader = do exp <- oneOf exponentMarkers
                                     s <- try parseSign <|> return 1
                                     d <- liftM (fst . head . fst reader) $ many1 digit
                                     let exponent = case toLower exp of
                                                      's' -> Short
                                                      'f' -> Single
                                                      'l' -> Long
                                                      _   -> Double
                                     return (exponent, s * d, exactness & Inexact)
  where exponentMarkers = "eEsSfFdDlL"

parseSufix :: Exactness -> Reader -> Parser (Precision, Integer, Exactness)
parseSufix exactness reader = try (parseBareSufix exactness reader)
                              <|> return (Double, 0, exactness & Unknown)

getDecimal :: Precision -> Double -> LispNumber
getDecimal Short  = LRealShort . fromRational . toRational
getDecimal Single = LRealSingle . fromRational . toRational
getDecimal Double = LRealDouble . fromRational . toRational
getDecimal Long   = LRealLong . fromRational . toRational

resolveDecimal :: String -> Precision -> Integer -> Exactness -> Reader -> LispNumber
resolveDecimal string prec power exactness reader =  if exactness == Exact
                                                     then LRational . toRational $ number
                                                     else getDecimal prec number
    where mPointPos = findIndex (== '.') . reverse $ string
          pointPos = fromIntegral $ case mPointPos of
                                      Just r -> r
                                      _      -> 0
          exp = fromIntegral $ snd reader ^ (power-pointPos)
          string' = filter (/='.') string
          number = exp * fromIntegral (fst . head . fst reader $ string')

parseDecimal1 :: Exactness -> Reader -> Parser LispVal
parseDecimal1 exactness reader = do before <- many1 digit
                                    hs <- many1 $ char '#'
                                    char '.'
                                    many $ char '#'
                                    (prec, power, e) <- parseSufix exactness reader
                                    let n = before ++ replicate (length hs) '0'
                                    return . Number $ resolveDecimal n prec power e reader

parseDecimal2 :: Exactness -> Reader -> Parser LispVal
parseDecimal2 exactness reader = do before <- many1 digit
                                    char '.'
                                    after <- many digit
                                    many $ char '#'
                                    (prec, power, e) <- parseSufix exactness reader
                                    let n = before ++ "." ++ after
                                    return . Number $ resolveDecimal n prec power e reader

parseDecimal3 :: Exactness -> Reader -> Parser LispVal
parseDecimal3 exactness reader = do char '.'
                                    after <- many1 digit
                                    many $ char '#'
                                    (prec, power, e) <- parseSufix exactness reader
                                    let n = '.' : after
                                    return . Number $ resolveDecimal n prec power e reader

parseDecimal4 :: Exactness -> Reader -> Parser LispVal
parseDecimal4 exactness reader = do n <- many1 digit
                                    (prec, power, e) <- parseSufix exactness reader
                                    return . Number $ resolveDecimal n prec power e reader

parseDecimal :: Exactness -> Reader -> Parser LispVal
parseDecimal exactness reader = try (parseDecimal1 exactness reader)
                                <|> try (parseDecimal2 exactness reader)
                                <|> try (parseDecimal3 exactness reader)
                                <|> parseDecimal4 exactness reader

parseUReal :: Exactness -> Reader -> Parser LispVal
parseUReal exactness reader = try (parseRational exactness reader)
                              <|> try (parseDecimal exactness reader)
                              <|> parseUInteger exactness reader

parseImaginary exactness reader = do (i, e) <- parseBareImaginary
                                     let val = case exactness & e of
                                                 Inexact -> Number $ LInexactComplex 0 $ fromIntegral i
                                                 _       -> Number $ LExactComplex 0 i
                                     return val
    where parseBareImaginary = do s <- parseSign
                                  d <- parseDecimal exactness reader <|> return (1, Unknown) -- XXX
                                  oneOf "iI" -- XXX
                                  return (fromIntegral s * d, exactness) -- XXX

parsePolar :: Exactness -> Reader -> Parser LispVal
parsePolar = undefined

parseComplex :: Exactness -> Reader -> Parser LispVal
parseComplex exactness reader = try (parseImaginary exactness reader)
                                <|> try (parsePolar exactness reader)
                                <|> parseUReal exactness reader

parseNumber :: Parser LispVal
parseNumber = do (exactness, reader) <- parsePrefix
                 parseComplex exactness reader

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (parseEscape <|> noneOf "\"")
                 char '"'
                 return $ String x
    where escapes = "\\\"nrt"
          parseEscape = do char '\\'
                           next  <- oneOf escapes
                           return . read $ ['\'', '\\', next, '\'']
parseSymbol :: Parser LispVal
parseSymbol = do first <- letter <|> symbolChar
                 rest <- many (letter <|> digit <|> symbolChar <|> char '#')
                 let atom = first:rest
                 return $ Symbol atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do  char '\''
                  x <- parseExpr
                  return $ List [Symbol "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseBool
            <|> try parseChar
            <|> try parseVector
            <|> try parseNumber
            <|> parseSymbol
            <|> parseString
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _  -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn . readExpr . head $ args
