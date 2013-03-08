import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many)

num :: Parser Int
num = read <$> many1 digit

ipNext :: Parser Int
ipNext = char '.' *> num

ipv4 :: Parser [Int]
ipv4 = (:) <$> num <*> (count 3 ipNext)

main = parseTest ipv4 "255.128.64.10.22"
