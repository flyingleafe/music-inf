module Parser where

import Data.Functor (($>))
import Data.Text (Text)
import Text.Parsec hiding (Line)
import Text.Parsec.Text (Parser)

import Grammar

symbol :: String -> Parser String
symbol s = string s <* spaces

tune :: Parser Tune
tune = Tune <$> line <*> line

line :: Parser Line
line = Line <$>
  tonic <*>
  bar <*>
  bar <*>
  tonic

bar :: Parser Bar
bar = try (TonicBar <$> tonic)
  <|> try (SubBar <$> subdominant)
  <|> (DomBar <$> dominant)

tonic :: Parser Tonic
tonic = Tonic <$> ton <*> byTon <*> ton <*> ton <*> byTon <*> ton <* symbol "|"

dominant :: Parser Dominant
dominant = Dominant <$> dom <*> byDom <*> dom <*> dom <*> byDom <*> dom <* symbol "|"

subdominant :: Parser Subdominant
subdominant = Subdominant <$> subd <*> bySubd <*> subd <*> subd <*> bySubd <*> subd <* symbol "|"

ton :: Parser Ton
ton = (symbol "a" $> At)
  <|> (symbol "d" $> Dt)
  <|> (symbol "f" $> Ft)
  <|> (symbol "A" $> Aht)

byTon :: Parser ByTon
byTon = (symbol "b" $> Bt) <|> (ByTon <$> ton)

dom :: Parser Dom
dom = (symbol "a" $> Ad)
  <|> (symbol "c" $> Cd)
  <|> (symbol "e" $> Ed)
  <|> (symbol "A" $> Ahd)

byDom :: Parser ByDom
byDom = (symbol "f" $> Fd) <|> (ByDom <$> dom)

subd :: Parser Subd
subd = (symbol "b" $> Bs)
  <|> (symbol "d" $> Ds)
  <|> (symbol "g" $> Gs)
  <|> (symbol "B" $> Bhs)

bySubd :: Parser BySubd
bySubd = (symbol "e" $> Es) <|> (BySubd <$> subd)

parseTune :: Text -> Either ParseError Tune
parseTune = parse tune ""
