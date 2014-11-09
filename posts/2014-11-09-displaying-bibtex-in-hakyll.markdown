# Displaying bibTeX in Hakyll

My personal site has been powered by [Hakyll](http://jaspervdj.be/hakyll/) for a while.  I like being able to rsync static html to my Raspberry Pi server.  It gives me fewer security issues to worry about.  Being an academic, one of the reasons I have a website is to list my publications for the world.  Not that the world is too interested, but it helps when someone Google's your name.  Since I started using Hakyll I've had to duplicate my publications data, once for use with [LaTeX](http://www.latex-project.org/) (the least worse system for producing a print document) and once as my publications.html on this site.  The computer scientist in my hates this as it violates the *D*on't *R*epeat *Y*ourself principle.  So, how can I get Hakyll to generate my publications.html from my BibTeX data?  Read on for a solution.

The core idea here is to generate `IO [Item String]` so that I can use Hakyll's `unsafeCompiler` to generate a `Compiler [Item String]` which can then list each of the items as a bibliographic entry.  It'd be a bit smarter to generate `[Item Bibentry]` where `Bibentry` is some complex bibliography record, but for the moment this suffices.  In this simple solution I use `Parsec` to parse the BibTeX file:

```haskell
getCitations :: String -> IO [Text.BibTeX.Entry.T]
getCitations fname =
  Parsec.parseFromFile (Parsec.skipMany Parsec.space >> Parse.file) fname >>= either reportError return
  where
    reportError = error "Moo"
```

Yeah, error handling leaves a _lot_ to be desired.

Given the `IO [Entry.T]` we need to turn them into HTML strings.  For this we'll employ the awesome [Pandoc](http://johnmacfarlane.net/pandoc/) to change any LaTeX formatted strings into HTML structure.

```haskell
paraToPlain :: Block -> Block
paraToPlain (Para x) = Plain x

htmlize :: String -> String
htmlize latex = writeHtmlString def p
  where
    p = topDown paraToPlain (readLaTeX def latex)
```

The only complexity here is that, by default, Pandoc will generate the string `\textbf{foo}` as `<p><strong>foo</strong></p>` whereas I'd prefer `<strong>foo</strong>`.  Hence I have to do the `topDown` walking of the Pandoc document structure converting any `Para` into a `Plain`.  After that, it's a case of pulling out each bibliographic entry and htmlizing it:

```haskell
getField :: Entry.T -> String -> String
getField entry field = htmlize (fromJust (lookup field (Entry.fields (Entry.lowerCaseFieldNames entry))))

getFields :: Entry.T -> [String] -> [String]
getFields entry xs = map (getField entry) xs

getConf :: Entry.T -> Item String
getConf entry = Item {itemIdentifier = id,
                          itemBody = body}
  where
    id = fromFilePath (Entry.identifier entry)
    body = intercalate ", " (getFields entry ["author", "title", "booktitle", "pages", "year"])

cite :: Entry.T -> Item String
cite entry =
  case (Entry.entryType entry) of
    "article" -> getArticle entry
    "proceedings" -> getProceedings entry
    "inproceedings" -> getConf entry
```

As we now have something suitable for `unsafeCompiler` we can then use Hakyll to generate a HTML file as we normally would, where the content of `templates/publications.html` is a simple `<ul>`.

```haskell
create ["publications.html"] $ do
  route idRoute
  compile $ do
    books <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/books.bib"
    journals <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/journals.bib"
    confs <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/conferences.bib"
    let pubCtx =
          listField "books" defaultContext (return books) `mappend`
          listField "journals" defaultContext (return journals) `mappend`
          listField "confs" defaultContext (return confs) `mappend`
          defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/publications.html" pubCtx
      >>= loadAndApplyTemplate "templates/default.html" pubCtx
      >>= relativizeUrls
```

A full solution fills in the blanks of the above description.  As my Blog (and the rest of my life) is stored in git, I've made it available on [github](https://github.com/AidanDelaney/Blog).