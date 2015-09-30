{-# LANGUAGE TemplateHaskell #-}
module Pieces
    ( piece
    , pieces
    , parsePiecesFromFile

    , Piece
    ) where

import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec

type Piece = (String,[[String]])

piece :: Parser Piece
piece = (,) <$> (line <* sepLine) <*> manyTill plane (try endLine)

pieces :: Parser [Piece]
pieces = sepEndBy1 piece blankLine

plane :: Parser [String]
plane = manyTill line (try sepLine)

line = manyTill anyChar newline
blankLine = manyTill (char ' ' <|> char '\t') newline
sepLine = many1Till (char '-') newline
endLine = many1Till (char '.') ((newline *> return ()) <|> eof)

many1Till p e = (:) <$> p <*> manyTill p e

parsePiecesFromFile :: FilePath -> IO (Either ParseError [Piece])
parsePiecesFromFile f = parseFromFile pieces f
