module Main where

import Control.Applicative
import Data.Char

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Eq, Show)
              
newtype Parser a = Parser { runParser:: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = 
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charParser :: Char -> Parser Char
charParser c = Parser $ f
  where 
    f (x:xs)
      | x == c    = Just (xs, x)
      | otherwise = Nothing
    f [] = Nothing
                
stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser $ \input -> 
  let (token, rest) = span f input
  in Just(rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

stringLiteral :: Parser String
stringLiteral = charParser '"' *> spanParser (/= '"') <* charParser '"'

whitespaceParser :: Parser String
whitespaceParser = spanParser isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many(sep *> whitespaceParser *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringParser "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false")
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        f _       = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanParser isDigit)
  where f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charParser '[' *> whitespaceParser *> elements <* whitespaceParser <*charParser ']')
  where elements = sepBy sep jsonValue
        sep      = whitespaceParser *> charParser ',' <* whitespaceParser

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> whitespaceParser *> pairs <* whitespaceParser<* charParser '}')
  where pairs = sepBy sep pair
        pair = (\key _ value ->  (key, value)) 
          <$> stringLiteral 
          <*> (whitespaceParser *> charParser ':' *> whitespaceParser) 
          <*> jsonValue
        sep = sepBy (whitespaceParser *> charParser ',' <* whitespaceParser) pair

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO()
main = undefined