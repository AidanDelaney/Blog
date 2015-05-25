module Publications where

import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.ParserCombinators.Parsec as Parsec

import Data.List (intercalate)
import Data.String.Utils (replace)

import Control.Monad (liftM)
import Data.Maybe (fromJust)

import Hakyll
import Hakyll.Core.Compiler (unsafeCompiler)

import Text.Pandoc

paraToPlain :: Block -> Block
paraToPlain (Para x) = Plain x

htmlize :: String -> String
htmlize latex = writeHtmlString def p
  where
    p = topDown paraToPlain (readLaTeX def latex)

getField :: Entry.T -> String -> String
getField entry field = htmlize (fromJust (lookup field (Entry.fields (Entry.lowerCaseFieldNames entry))))

getFields :: Entry.T -> [String] -> [String]
getFields entry xs = map (getField entry) xs

getAuthor :: Entry.T -> String
getAuthor entry = (replace "Aidan Delaney" "<strong>Aidan Delaney</strong>") (getField entry "author")

getEditor :: Entry.T -> String
getEditor entry = (replace "Aidan Delaney" "<strong>Aidan Delaney</strong>") (getField entry "editor")

getArticle :: Entry.T -> Item String
getArticle entry = Item {itemIdentifier = id,
                          itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = getAuthor entry ++ ", " ++ (intercalate ", " (getFields entry ["title", "journal", "pages", "year"]))

getProceedings :: Entry.T -> Item String
getProceedings entry = Item {itemIdentifier = id,
                              itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = getEditor entry ++ ", "
           ++ (intercalate ", " (getFields entry ["title", "series", "year"]))

getConf :: Entry.T -> Item String
getConf entry = Item {itemIdentifier = id,
                          itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = getAuthor entry ++ ", "
           ++ (intercalate ", " (getFields entry ["title", "booktitle", "pages", "year"]))

cite :: Entry.T -> Item String
cite entry =
  case (Entry.entryType entry) of
    "article" -> getArticle entry
    "proceedings" -> getProceedings entry
    "inproceedings" -> getConf entry

getCitations :: String -> IO [Entry.T]
getCitations fname =
  Parsec.parseFromFile (Parsec.skipMany Parsec.space >> Parse.file) fname >>= either reportError return
  where
    reportError = error "Moo"

listCitations :: IO [Entry.T] -> IO [Item String]
listCitations es = liftM (map cite) es

getCiteCompiler :: String -> Compiler [Item String]
getCiteCompiler fname = unsafeCompiler (listCitations (getCitations fname))
