import System.Environment
import qualified System.IO as IO
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Control.Applicative

main = do
  args <- getArgs
  inh <- IO.openFile (args !! 0) IO.ReadMode
  body <- IO.hGetContents inh
  IO.putStr $ pickCompanyName body
  IO.hClose inh

pickCompanyName :: String -> String
pickCompanyName str =
    case parse (concat <$> manyTill pickup eof) "" str of
        Left err -> ""
        Right str -> str

pickup :: Parser String
pickup = (many $ noneOf " ") *> spaces *> (many anyChar)
--pickup = (many $ noneOf " ") *> (many space) <*> (many anyChar)


--
row :: Parser String
row = choice
    [ try hoge
    , otherlines
    ]

hoge :: Parser String
hoge = string "hoge" <* (many anyChar)


-- examples
doSomething :: String -> String
doSomething lines = case parse (concat <$> manyTill block eof) "" lines of
  Left  err -> ""
  Right str -> str

block :: Parser String
block = choice [ try japara
               , otherlines ]

japara :: Parser String
japara = string "<p>" *> (conc <$> manyTill anyChar (try $ string "</p>"))
  where conc = ("<p>"++) . (++"</p>") . replaceParen

otherlines :: Parser String
otherlines = manyTill anyChar $ (try $ string "\n")

replaceParen :: String -> String
replaceParen line = case parse (concat <$> many1 strOrParen) "" line of
  Left err -> ""
  Right str -> str

strOrParen :: Parser String
strOrParen = choice [ try noParens, try inParen, parens ]

inParen :: Parser String
inParen = string "(" *> (wrapDP <$> (manyTill strOrParen (string ")")))
  where wrapDP = ("（"++) . (++"）") . concat

noParens :: Parser String
noParens = many1 $ noneOf "()"

parens :: Parser String
parens = many1 $ oneOf "()"
