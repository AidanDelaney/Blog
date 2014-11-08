module Publications where

import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.ParserCombinators.Parsec as Parsec

import Data.List (intercalate)

import Control.Monad (liftM)
import Data.Maybe (fromJust)

import Hakyll
import Hakyll.Core.Compiler (unsafeCompiler)

import Text.Pandoc

htmlize :: String -> String
htmlize latex = writeHtmlString def (readLaTeX def latex)

getField :: Entry.T -> String -> String
getField entry field = fromJust (lookup field (Entry.fields (Entry.lowerCaseFieldNames entry)))

getFields :: Entry.T -> [String] -> [String]
getFields entry xs = map (htmlize . getField entry) xs

getArticle :: Entry.T -> Item String
getArticle entry = Item {itemIdentifier = id,
                          itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = intercalate ", " (getFields entry ["author", "title", "journal", "pages", "year"])

getProceedings :: Entry.T -> Item String
getProceedings entry = Item {itemIdentifier = id,
                              itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = intercalate ", " (getFields entry ["editor", "title", "year"])

cite :: Entry.T -> Item String
cite entry =
  case (Entry.entryType entry) of
    "article" -> getArticle entry
    "proceedings" -> getProceedings entry

getCitations :: String -> IO [Entry.T]
getCitations fname =
  Parsec.parseFromFile (Parsec.skipMany Parsec.space >> Parse.file) fname >>= either reportError return
  where
    reportError = error "Moo"

listCitations :: IO [Entry.T] -> IO [Item String]
listCitations es = liftM (map cite) es

getCiteCompiler :: String -> Compiler [Item String]
getCiteCompiler fname = unsafeCompiler (listCitations (getCitations fname))
