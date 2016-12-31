#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

{- Debian dependencies:
$ sudo apt-get install libghc-hakyll-dev libghc-pandoc-dev libghc-filestore-dev libghc-feed-dev libghc-tagsoup-dev imagemagick s3cmd git
(ImageMagick, s3cmd, & git are runtime dependencies used to help optimize images and upload to hosting/Github respectively)

$ cd ~/wiki/ && ghc -rtsopts -threaded -O2 -fforce-recomp -optl-s --make hakyll.hs &&
  ./hakyll rebuild +RTS -N3 -RTS && echo -n -e '\a' && emacs -nw _site/Modafinil &&
  s3cmd -v -v --human-readable-sizes --reduced-redundancy --guess-mime-type --default-mime-type=text/html
        --add-header="Cache-Control: max-age=604800, public" --delete-removed sync _site/ s3://www.gwern.net/ &&
  s3cmd --reduced-redundancy --mime-type=text/css --add-header="Cache-Control: max-age=604800, public" put ./_site/static/css/default.css s3://www.gwern.net/static/css/ &&
  rm -rf ~/wiki/_cache/ ~/wiki/_site/ && rm ./hakyll *.o *.hi ;
  git push; echo -n -e '\a'

Explanations:

- we could run Hakyll with a command like `./hakyll.hs build` but this would run much slower than if we compile an optimized parallelized binary & run it with multiple threads; this comes at the cost of considerable extra complexity in the invocation, though, since we need to compile it with fancy options, run it with other options, and then at the end clean up by deleting the compiled binary & intermediates (GHC cannot take care of them on its own: https://ghc.haskell.org/trac/ghc/ticket/4114 )
- `rebuild` instead of 'build' because IIRC there was some problem where Hakyll didn't like extension-less files so incremental syncs/builds don't work; this tells Hakyll
 to throw everything away without even trying to check
- Emacs: I manually edit the ads on the Modafinil page away from the default to the current sponsor; I should probably figure out how to do this automatically with the templating system but meh, I don't sync gwern.net *that* often
- s3cmd:

    - `--reduced-redundancy` saves a bit of money; no need for high-durability since everything is backed up locally in the git repo, after all
    - s3cmd's MIME type detection has been unreliable in the past, so we need to force a default, especially for the extension-less (HTML) files
    - the second, apparently redundant, CSS upload is an example of the MIME flakiness: for some reason, s3cmd sometimes uploads `*.css` files as the default MIME type!
      Of curse, browsers ignore any CSS served as text/HTML or the further default of octetstream, so gwern.net then breaks.
      By forcing an upload with a hardwired CSS MIME, we can avoid this being an issue ever again.
- after that, we clean up after ourselves and sync with the Github mirror as well
- the 'echo' calls are there to ring the terminal bell and notify the user that he needs to edit the Modafinil file or that the whole thing is done
-}

import Codec.Binary.UTF8.String (encode)
import Control.Exception (onException)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isAlphaNum, isAscii)
import Data.List (isInfixOf, nub, sort)
import Data.Monoid ((<>))
import Data.Set (filter)
import Network.HTTP (urlEncode)
import Network.URI (unEscapeString)
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import qualified Data.Map as M (fromList, lookup, Map)

import Data.FileStore (gitFileStore)
import Data.FileStore.Utils (runShellCommand)
import Feed (filestoreToXmlFeed, FeedConfig(..))
import Hakyll ((.&&.), applyTemplateList, buildTags, compile, complement, compressCssCompiler, constField,
               copyFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions,
               defaultHakyllWriterOptions, fromCapture, getRoute, hakyll, idRoute, itemIdentifier,
               loadAll, loadAndApplyTemplate, loadBody, makeItem, match, modificationTimeField,
               pandocCompilerWithTransform, preprocess, relativizeUrls, route, setExtension,
               tagsField, tagsRules, templateCompiler, Compiler, Context, Item, Pattern, Tags)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath (dropFileName)
import Text.HTML.TagSoup (renderTagsOptions,parseTags,renderOptions, optMinimize, Tag(TagOpen))
import Text.Pandoc (bottomUp, nullAttr, Extension(Ext_markdown_in_html_blocks), HTMLMathMethod(MathML), Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), ReaderOptions(..), WriterOptions(..))

main :: IO ()
main = hakyll $ do -- RSS/ATOM setup
             preprocess $ do rss <- filestoreToXmlFeed rssConfig (gitFileStore "./")  Nothing
                             createDirectoryIfMissing False "_site"
                             writeFile "_site/atom.xml" rss

             -- create static redirect pages for outdated/broken incoming links:
             preprocess $ createRedirects "_site" brokenLinks

             -- handle the simple static non-.page files
             let static = route idRoute >> compile copyFileCompiler
             mapM_ (`match` static) [ -- WARNING: match everything *except* Markdown
                                      -- since rules are mutually-exclusive!
                                     complement "docs/**.page" .&&. "docs/**",
                                     "haskell/**.hs",
                                     "images/**",
                                     "**.hs",
                                     "**.sh",
                                     "static/*",
                                     "static/img/**",
                                     "static/js/**",
                                     "index"]
             match "**.css" $ route idRoute >> compile compressCssCompiler
             match "static/templates/*.html" $ compile templateCompiler

             tags <- buildTags "**.page" (fromCapture "tags/*")

             match "**.page" $ do
                 route $ setExtension "" -- cool URLs
                 -- https://groups.google.com/forum/#!topic/pandoc-discuss/HVHY7-IOLSs
                 let readerOptions = defaultHakyllReaderOptions { readerExtensions = Data.Set.filter (/=Ext_markdown_in_html_blocks) $ readerExtensions defaultHakyllReaderOptions }
                 compile $ pandocCompilerWithTransform readerOptions woptions pandocTransform
                     >>= loadAndApplyTemplate "static/templates/default.html" (postCtx tags)
                     >>= imgUrls
                     >>= relativizeUrls

             tagsRules tags $ \tag pattern -> do
                 let title = "Tag: " ++ tag
                 route idRoute
                 compile $ tagPage tags title pattern

woptions :: WriterOptions
woptions = defaultHakyllWriterOptions{ writerSectionDivs = True,
                                       writerTableOfContents = True,
                                       writerColumns = 120,
                                       writerTemplate = Just "<div id=\"TOC\">$toc$</div>\n<div id=\"markdownBody\">$body$</div>",
                                       writerHtml5 = True,
                                       writerHtmlQTags = True,
                                       writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                       writerEmailObfuscation = NoObfuscation }


rssConfig :: FeedConfig
rssConfig = FeedConfig { fcTitle = "Gwern", fcBaseUrl  = "https://www.gwern.net", fcFeedDays = 30 }

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTemplate <- loadBody "static/templates/postitem.html"
    posts' <- loadAll pattern
    posts <- preprocess' posts'
    applyTemplateList postItemTemplate (postCtx tags) posts
tagPage :: Tags -> String -> Pattern -> Compiler (Item String)
tagPage tags title pattern = do
    list <- postList tags pattern (return . id)
    makeItem ""
        >>= loadAndApplyTemplate "static/templates/tags.html"
                (constField "posts" list <> constField "title" title <>
                    defaultContext)
        >>= relativizeUrls

imgUrls :: Item String -> Compiler (Item String)
imgUrls item = do
    rte <- getRoute $ itemIdentifier item
    return $ case rte of
        Nothing -> item
        Just _  -> fmap (unsafePerformIO . addImgDimensions) item

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags <>
    defaultContext <>
    dateField "created" "%d %b %Y" <>
    modificationTimeField "modified" "%d %b %Y" <>
    constField "author" "gwern" <>
    constField "status" "N/A" <>
    constField "belief" "N/A" <>
    constField "description" "N/A"

pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp (map (convertInterwikiLinks . convertHakyllLinks . addAmazonAffiliate))

-- For Amazon links, there are two scenarios: there are parameters (denoted by a
-- '?' in the URL), or there are not. In the former, we need to append the tag as
-- another item ('&tag='), while in the latter, we need to set up our own
-- parameter ('?tag='). The transform may be run many times since
-- they are supposed to be pure, so we
-- need to also check a tag hasn't already been appended.
--
-- For non-Amazon links, we just return them unchanged.
addAmazonAffiliate :: Inline -> Inline
addAmazonAffiliate x@(Link _ r (l, t)) = if (("amazon.com/" `isInfixOf` l) && not ("tag=gwernnet-20" `isInfixOf` l)) then
                                        if ("?" `isInfixOf` l) then Link nullAttr r (l++"&tag=gwernnet-20", t) else Link nullAttr r (l++"?tag=gwernnet-20", t)
                                       else x
addAmazonAffiliate x = x

-- GITIT -> HAKYLL LINKS PLUGIN
-- | Convert links with no URL to wikilinks.
convertHakyllLinks :: Inline -> Inline
convertHakyllLinks (Link _ ref ("", "")) = let ref' = inlinesToURL ref in Link nullAttr ref (ref', "Go to wiki page: " ++ ref')
convertHakyllLinks x = x

-- FASTER HTML RENDERING BY STATICLY SPECIFYING ALL IMAGE DIMENSIONS
-- read HTML string with TagSoup, process `<img>` tags to read the file's dimensions, and hardwire them
-- this optimizes HTML rendering since browsers know before downloading the image how to layout the page
addImgDimensions :: String -> IO String
addImgDimensions = fmap (renderTagsOptions renderOptions{optMinimize=whitelist}) . mapM staticImg . parseTags
                 where whitelist s = s /= "div" && s /= "script"
{- example illustration:
 TagOpen "img" [("src","/images/201201-201207-traffic-history.png")
                ("alt","Plot of page-hits (y-axis) versus date (x-axis)")],
 TagOpen "figcaption" [],TagText "Plot of page-hits (y-axis) versus date (x-axis)",
 TagClose "figcaption",TagText "\n",TagClose "figure" -}
staticImg :: Tag String -> IO (Tag String)
staticImg x@(TagOpen "img" xs) = do let optimized = lookup "height" xs
                                    case optimized of
                                      Just _ -> return x
                                      Nothing -> do let path = lookup "src" xs
                                                    case path of
                                                          Nothing -> return x
                                                          Just p -> do let p' = if head p == '/' then tail p else p
                                                                       (height,width) <- imageMagick p' `onException` (putStrLn p)
                                                                       -- body max-width is 1100 px, sidebar is 150px, so any image wider than 900px
                                                                       -- will wind up being reflowed by the 'img { max-width: 100%; }' responsive-image CSS declaration;
                                                                       -- let's avoid that specific case by lying about its width, although this doesn't fix all the reflowing.
                                                                       -- No images should be more than a screen in height either, so we'll set a maximum of 900
                                                                       let width' =  show ((read width::Int) `min` 900)
                                                                       let height' = show ((read height::Int) `min` 900)
                                                                       return (TagOpen "img" (uniq ([("height", height'), ("width", width')]++xs)))
            where uniq = nub . sort
staticImg x = return x
-- | Use FileStore util to run imageMagick's 'identify', & extract the dimensions
-- Note that for animated GIFs, 'identify' returns width/height for each frame of the GIF, which in
-- most cases will all be the same, so we take the first line of whatever dimensions 'identify' returns.
imageMagick :: FilePath -> IO (String,String)
imageMagick f = do (status,_,bs) <- runShellCommand "./" Nothing "identify" ["-format", "%h %w\n", f]
                   case status of
                     ExitFailure _ -> error f
                     _ -> do let [height, width] = words $ head $ lines $ (unpack bs)
                             return (height, width)


-- INTERWIKI PLUGIN
-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL x = let x' = inlinesToString x
                     (a,b) = break (=='%') x'
                 in escape a ++ b
-- copied from "XMonad.Actions.Search"
escape :: String -> String
escape = concatMap escapeURIChar
         where escapeURIChar :: Char -> String
               escapeURIChar c | isAscii c && isAlphaNum c = [c]
                               | otherwise                 = concatMap (printf "%%%02X") $ encode [c]

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s    -> s
               Code _ s -> s
               _        -> " "
convertInterwikiLinks :: Inline -> Inline
convertInterwikiLinks (Link _ ref (interwiki, article)) =
  case interwiki of
    ('!':interwiki') ->
        case M.lookup interwiki' interwikiMap of
                Just url  -> case article of
                                  "" -> Link nullAttr ref (url `interwikiurl` inlinesToString ref, summary $ unEscapeString $ inlinesToString ref)
                                  _  -> Link nullAttr ref (url `interwikiurl` article, summary article)
                Nothing -> Link nullAttr ref (interwiki, article)
            where -- 'https://starwars.wikia.com/wiki/Emperor_Palpatine'
                  interwikiurl u a = u ++ urlEncode (deunicode a)
                  deunicode = map (\x -> if x == '’' then '\'' else x)
                  -- 'Wookieepedia: Emperor Palpatine'
                  summary a = interwiki' ++ ": " ++ a
    _ -> Link nullAttr ref (interwiki, article)
convertInterwikiLinks x = x
-- | Large table of constants; this is a mapping from shortcuts to a URL. The URL can be used by
--   appending to it the article name (suitably URL-escaped, of course).
interwikiMap :: M.Map String String
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap
wpInterwikiMap, customInterwikiMap :: [(String, String)]
customInterwikiMap = [("Hackage", "https://hackage.haskell.org/package/"),
                      ("Hawiki", "https://haskell.org/haskellwiki/"),
                      ("Hoogle", "https://www.haskell.org/hoogle/?hoogle=")]
wpInterwikiMap = [("Wikipedia", "https://en.wikipedia.org/wiki/"),
                  ("Wikiquote", "https://en.wikiquote.org/wiki/"),
                  ("Wiktionary", "https://en.wiktionary.org/wiki/")]

-- | Create in the sync/compilation directory, using a database mapping broken URLs to working ones, HTML files which will do
--   HTML META tag redirect pages (since, as a static site, we can't use web-server-level 301 redirects, and using JS is gross).
--   This is useful for sending people using old URLs to renamed versions, dealing with common typos etc, and will increase site traffic.
--   Such broken URLs can be found by looking at server logs or by using Google Webmaster Tools.
--   Broken URLs must be valid Haskell strings, escaped & valid POSIX filenames, and relative links,
--   since they will be defined in a 'hakyll.hs' and during generation, written to disk with the filename corresponding to the broken URLs.
--   (Target URLs can be absolute or relative, but should also be escaped.) So broken incoming links like "http://www.gwern.net/foo/" which should be
--   "http://www.gwern.net/foo" cannot be fixed (since you cannot create a HTML file named "foo/" on disk, as that would be a directory).
--   See https://groups.google.com/d/msg/hakyll/sWc6zxfh-uM/fUpZPsFNDgAJ
createRedirects :: FilePath -> [(FilePath,FilePath)] -> IO()
createRedirects target db = mapM_ (\(broken,working) -> writeRedirect target broken (createRedirect working)) db

writeRedirect :: String -> FilePath -> String -> IO ()
writeRedirect prefix fileName html = do -- ensure prefix directory exists, and also that any implied subdirectories do too:
                                        createDirectoryIfMissing True (prefix ++ dropFileName fileName)
                                        writeFile (prefix ++ fileName) html

createRedirect :: FilePath -> String
createRedirect working = "<html><head><meta http-equiv=\"refresh\" content=\"0; url=" ++ working ++
                         "\"><link rel=\"canonical\" href=\"" ++ working ++
                         "\"></head><body><p>The page has moved to: <a href=\"" ++ working ++
                         "\">this page</a></p></body></html>"

-- | Mapping of gwern.net URLs people mistakenly visit with current correct URL, based on Google Webmaster Tool report 2016-12-31.
brokenLinks :: [(FilePath,FilePath)]
brokenLinks = [
 ("/2015-10-27-modafinilsurvey-feedback.csv", "/docs/modafinil/survey/2015-10-27-modafinilsurvey-feedback.csv"),
 ("/2docs/dnb/1978-zimmer.pdf", "/docs/music-distraction/1978-zimmer.pdf"),
 ("/AB", "/AB%20testing"),
 ("/About.html", "/About"),
 ("/Aria's%20past", "/Aria's%20past,%20present,%20and%20future"),
 ("/Bitcoin", "/Bitcoin%20is%Worse%20is%20Better"),
 ("/Black-Market-archives", "/Black-market%20archives"),
 ("/Black-market%20", "/Black-market%20archives"),
 ("/Black-market%20archive", "/Black-market%20archives"),
 ("/Black-market%20archivesDec", "/Black-market%20archives"),
 ("/Black-market%25archives", "/Black-market%20archives"),
 ("/Black-market%archives", "/Black-market%20archives"),
 ("/Black-market+archives", "/Black-market%20archives"),
 ("/Blackmarket%20arrests", "/Black-market%20arrests"),
 ("/Book", "/Book%20reviews"),
 ("/Book%20reading%20list", "/Book%20reviews"),
 ("/Book%20reviews%3Cbr%20/%3E", "/Book%20reviews"),
 ("/Book%20reviewsSimilarAug", "/Book%20reviews"),
 ("/Candy", "/Candy%20Japan"),
 ("/Complexity", "/Complexity%20vs%20AI"),
 ("/Culture%20is%20not%20about%20esthetics", "/Culture%20is%20not%20about%20Esthetics"),
 ("/DNB", "/DNB%20FAQ"),
 ("/DNB%", "/DNB%20FAQ"),
 ("/DNB%20F", "/DNB%20FAQ"),
 ("/DNB%20FAQ!", "/DNB%20FAQ"),
 ("/DNB%20FAQOder", "/DNB%20FAQ"),
 ("/DNB%20FAQOppure", "/DNB%20FAQ"),
 ("/DNB%20FAQOu", "/DNB%20FAQ"),
 ("/DNB%20FAQSau", "/DNB%20FAQ"),
 ("/DNB%2520FAQ", "/DNB%20FAQ"),
 ("/DNB+meta-analysis", "/DNB%20meta-analysis"),
 ("/Death%20Note%20Ending.html", "/Death%20Note%20Ending"),
 ("/Docs/melatonin/1998-lewy.PDF", "/docs/melatonin/1998-lewy.pdf"),
 ("/Drug", "/Drug%20heuristics"),
 ("/Ethical", "/Ethical%20sperm%20donation"),
 ("/Genshiken", "/fiction/Genshiken"),
 ("/Girl%20Scouts%20and%20good%20governance.html", "/Girl%20Scouts%20and%20good%20governance"),
 ("/Google", "/Google%20shutdowns"),
 ("/Google%2520shutdowns", "/Google%20shutdowns"),
 ("/Google+shutdowns", "/Google%20shutdowns"),
 ("/Haskell%20Summer%20of%20Code.html", "/Haskell%20Summer%20of%20Code"),
 ("/In%2520Defense%2520Of%2520Inclusionism", "/In%20Defense%20Of%20Inclusionism"),
 ("/LSD", "/LSD%20microdosing"),
 ("/LSD%20microdosingis", "/LSD%20microdosing"),
 ("/LSDmicrodosing", "/LSD%20microdosing"),
 ("/Mela", "/Melatonin"),
 ("/Melatonin)", "/Melatonin"),
 ("/Melatonin.html", "/Melatonin"),
 ("/Mnemosyne", "/Spaced%20repetition"),
 ("/Mnemosyne.html", "/Spaced%20repetition"),
 ("/Modafinil.Buy", "/Modafinil"),
 ("/Modafinil.Modafinil", "/Modafinil"),
 ("/Modafinil.html", "/Modafinil"),
 ("/Modafinil.page", "/Modafinil"),
 ("/Modafinil1", "/Modafinil"),
 ("/Modafinil;", "/Modafinil"),
 ("/ModafinilCachedModafinil", "/Modafinil"),
 ("/Modafinil_", "/Modafinil"),
 ("/N", "/DNB%20FAQ"),
 ("/N-back", "/DNB%20FAQ"),
 ("/N-back%20FAQ", "/DNB%20FAQ"),
 ("/N-back%20FAQ.html", "/DNB%20FAQ"),
 ("/N-back%20FAQThis", "/DNB%20FAQ"),
 ("/Noot", "/Nootropics"),
 ("/Nootropics.html", "/Nootropics"),
 ("/Notes.html", "/Notes"),
 ("/On%20Disrespect.html", "/On%20Disrespect"),
 ("/Prediction", "/Prediction%20markets"),
 ("/Prediction%20Markets", "/Prediction%20markets"),
 ("/Prediction%20markets.html", "/Prediction%20markets"),
 ("/RNN", "/RNN%20metadata"),
 ("/Resilient%20Haskell%20Software.html", "/Resilient%20Haskell%20Software"),
 ("/Selfdecrypting", "/Self-decrypting%20files"),
 ("/Silk", "/Silk%20Road"),
 ("/Silk%", "/Silk%20Road"),
 ("/Silk%2", "/Silk%20Road"),
 ("/Silk%20", "/Silk%20Road"),
 ("/Silk%20%20%20%20Road", "/Silk%20Road"),
 ("/Silk%20Road%22", "/Silk%20Road"),
 ("/Silk%20Road)", "/Silk%20Road"),
 ("/Silk%20Road.LargeVisitorGlobe.User", "/Silk%20Road"),
 ("/Simulation", "/Simulatin%20inferences"),
 ("/Slowing", "/Slowing%20Moore's%20Law"),
 ("/Slowing%2520Moore%2527s%2520Law", "/Slowing%20Moore's%20Law"),
 ("/SlowingMoore%20sLaw", "/Slowing%20Moore's%20Law"),
 ("/SlowingMoore%E2%80%99sLaw", "/Slowing%20Moore's%20Law"),
 ("/Spaced", "/Spaced%20repetition"),
 ("/Spaced%20Repetition", "/Spaced%20repetition"),
 ("/Statistical", "/Statistical%20notes"),
 ("/Sunk%20costAdding", "/Sunk%20cost"),
 ("/Terrorism", "/Terrorism%20is%20not%20about%20Terror"),
 ("/The%20Melancholy%20of%20Kyon.html", "/The%20Melancholy%20of%20Kyon"),
 ("/Tool", "/Tool%20AI"),
 ("/Tool%20AIs", "/Tool%20AI"),
 ("/Wikipedia%20RSS%20Archive%20Bot", "/haskell/Wikipedia%20RSS%20Archive%20Bot"),
 ("/Yawgoog", "/Yawgoog%20injustice"),
 ("/Zeo%5B1%5D", "/Zeo"),
 ("/ZeoCached", "/Zeo"),
 ("/atom.xml%22", "/atom.xml"),
 ("/bitcoin%20is%20worse%20is%20better", "/Bitcoin%20is%Worse%20is%20Better"),
 ("/changelog", "/Changelog"),
 ("/choosing%20software", "/Choosing%20Software"),
 ("/doc/statistics/1933-elderton.pdf", "/docs/statistics/1933-elderton.pdf"),
 ("/doc/statistics/2015-pedroza.pdf", "/docs/statistics/2015-pedroza.pdf"),
 ("/docs/1939-spitzer.pdf", "/docs/spacedrepetition/1939-spitzer.pdf"),
 ("/docs/1963-peterson.pdf", "/docs/spacedrepetition/1963-peterson.pdf"),
 ("/docs/1967-keppel.pdf", "/docs/spacedrepetition/1967-keppel.pdf"),
 ("/docs/1968-fierro-ben%C3%ADtez.pdf", "/docs/iodine/1968-fierro-benítez.pdf"),
 ("/docs/1975-brown.pdfMaking", "/docs/1975-brown.pdf"),
 ("/docs/1975-laporte.pdf", "/docs/spacedrepetition/1975-laporte.pdf"),
 ("/docs/1976-efron.pdf", "/docs/statistics/1976-efron.pdf"),
 ("/docs/1977-glenberg.pdf", "/docs/spacedrepetition/1977-glenberg.pdf"),
 ("/docs/1980-damos.pdf", "/docs/dnb/1980-damos.pdf"),
 ("/docs/1981-duchastel.pdf", "/docs/spacedrepetition/1981-duchastel.pdf"),
 ("/docs/1981-staw.pdf", "/docs/sunkcosts/1981-staw.pdf"),
 ("/docs/1982-nungester.pdf", "/docs/spacedrepetition/1982-nungester.pdf"),
 ("/docs/1983-lee.pdf", "/docs/spacedrepetition/1983-lee.pdf"),
 ("/docs/1984-bazerman.pdf", "/docs/sunkcosts/1984-bazerman.pdf"),
 ("/docs/1985-godfrey.pdf", "/docs/dnb/1985-godfrey.pdf"),
 ("/docs/1985-hofstadte......-lottery", "/docs/1985-hofstadter"),
 ("/docs/1985-mckenna.pdf", "/docs/spacedrepetition/1985-mckenna.pdf"),
 ("/docs/1985-pirolli.pdf", "/docs/spacedrepetition/1985-pirolli.pdf"),
 ("/docs/1987-bahrick.pdf", "/docs/spacedrepetition/1987-bahrick.pdf"),
 ("/docs/1987-pocock.pdf", "/docs/dnb/1987-pocock.pdf"),
 ("/docs/1987-smith.pdf", "/docs/dnb/1987-smith.pdf"),
 ("/docs/1989-glover.pdf", "/docs/spacedrepetition/1989-glover.pdf"),
 ("/docs/1990-larrick.pdf", "/docs/sunkcosts/1990-larrick.pdf"),
 ("/docs/1990-rothman.pdf", "/docs/dnb/1990-rothman.pdf"),
 ("/docs/1991-shute.pdf", "/docs/dnb/1991-shute.pdf"),
 ("/docs/1991-shute.pdf--gwern--You", "/docs/dnb/1991-shute.pdf"),
 ("/docs/1992-berry.pdf", "/docs/algernon/1992-berry.pdf"),
 ("/docs/1993-cooper.pdf", "/docs/sunkcosts/1993-cooper.pdf"),
 ("/docs/1993-friedman.pdf", "/docs/conscientiousness/1993-friedman.pdf"),
 ("/docs/1994-falk", "/docs/statistics/1994-falk"),
 ("/docs/1994-graesser.pdf", "/docs/spacedrepetition/1994-graesser.pdf"),
 ("/docs/1994-heishman.pdf", "/docs/nicotine/1994-heishman.pdf"),
 ("/docs/1994-krueger.pd", "/docs/conscientiousness/1994-krueger.pdf"),
 ("/docs/1994-lehouezec.pdf", "/docs/nicotine/1994-lehouezec.pdf"),
 ("/docs/1995-kramer.pdf", "/docs/dnb/1995-kramer.pdf"),
 ("/docs/1995-tan.pdf", "/docs/sunkcosts/1995-tan.pdf"),
 ("/docs/1996-animerica-conscience-otaking", "/docs/eva/1996-animerica-conscience-otaking"),
 ("/docs/1996-conners.pdf", "/docs/nicotine/1996-conners.pdf"),
 ("/docs/1996-costa.pdf", "/docs/conscientiousness/1996-costa.pdf"),
 ("/docs/1996-foulds.pdf", "/docs/nicotine/1996-foulds.pdf"),
 ("/docs/1996icosta.pdf", "/docs/conscientiousness/1996-costa.pdf"),
 ("/docs/1997-hatton.pdf", "/docs/dnb/1997-hatton.pdf"),
 ("/docs/1997-utena.page", "/docs/1997-utena"),
 ("/docs/1998-dales-truthinmathematics.pdf", "/docs/math/1998-dales-truthinmathematics.pdf"),
 ("/docs/1998-feskanich.pdf", "/docs/nootropics/1998-feskanich.pdf"),
 ("/docs/1998-henneberg.pdf", "/docs/algernon/1998-henneberg.pdf"),
 ("/docs/1999-bradbury", "/docs/1999-bradbury-matrioshkabrains.pdf"),
 ("/docs/1999-bradbury-matrioshkabrains.pdf%20", "/docs/1999-bradbury-matrioshkabrains.pdf"),
 ("/docs/1999-fratiglioni.pdf", "/docs/dnb/1999-fratiglioni.pdf"),
 ("/docs/2000-duval.pdf", "/docs/dnb/2000-duval.pdf"),
 ("/docs/2000-fratiglioni.pdf", "/docs/nicotine/2000-fratiglioni.pdf"),
 ("/docs/2000-moffatt.pdf", "/docs/nicotine/2000-moffatt.pdf"),
 ("/docs/2001-berlin.pdf", "/docs/nicotine/2001-berlin.pdf"),
 ("/docs/2001-principlesforecasting.pdf", "/docs/predictions/2001-principlesforecasting.pdf"),
 ("/docs/2002-klauer.pdf", "/docs/dnb/2002-klauer.pdf"),
 ("/docs/2002-notenki-memoirs", "/docs/eva/2002-notenki-memoirs"),
 ("/docs/2002-notenki-memoirs.html", "/docs/eva/2002-notenki-memoirs"),
 ("/docs/2002-notenki-memoirs.page", "/docs/eva/2002-notenki-memoirs"),
 ("/docs/2002-watanabe.pdf", "/docs/creatine/2002-watanabe.pdf"),
 ("/docs/2003-jaeggi.pdf", "/docs/dnb/2003-jaeggi.pdf"),
 ("/docs/2003-murray-human-accomplishment.pdf)", "/docs/2003-murray-human-accomplishment.pdf"),
 ("/docs/2003-salinpascual.pdf", "/docs/nicotine/2003-salinpascual.pdf"),
 ("/docs/2003-villegier.pdf", "/docs/nicotine/2003-villegier.pdf"),
 ("/docs/2004-baranski.pdf", "/docs/modafinil/2004-baranski.pdf"),
 ("/docs/2004-mueller.pdf", "/docs/modafinil/2004-mueller.pdf"),
 ("/docs/2004-okada", "/docs/eva/2004-okada"),
 ("/docs/2005-ackerman.pdf", "/docs/dnb/2005-ackerman.pdf"),
 ("/docs/2005-buehner.pdf", "/docs/dnb/2005-buehner.pdf"),
 ("/docs/2005-denburg.pdf", "/docs/dnb/2005-denburg.pdf"),
 ("/docs/2005-duckworth.pdf", "/docs/dnb/2005-duckworth.pdf"),
 ("/docs/2005-james.pdf", "/docs/nootropics/2005-james.pdf"),
 ("/docs/2005-little-boy", "/docs/eva/2005-little-boy"),
 ("/docs/2005-murakami", "/docs/eva/2005-murakami"),
 ("/docs/2005-murakami.pdf", "/docs/eva/2005-murakami.pdf"),
 ("/docs/2005-sawaragi", "/docs/eva/2005-sawaragi"),
 ("/docs/2005-unsworth.pdf", "/docs/dnb/2005-unsworth.pdf"),
 ("/docs/2006-francis.pdf", "/docs/nootropics/2006-francis.pdf"),
 ("/docs/2006-guillem.pdf", "/docs/nicotine/2006-guillem.pdf"),
 ("/docs/2006-mcmorris.pdf", "/docs/creatine/2006-mcmorris.pdf"),
 ("/docs/2006-vaishnavi.pdf", "/docs/modafinil/2006-vaishnavi.pdf"),
 ("/docs/2007-bishopclark.pdf", "/docs/conscientiousness/2007-bishopclark.pdf"),
 ("/docs/2007-danigelis.pdf", "/docs/dnb/2007-danigelis.pdf"),
 ("/docs/2007-dimitrova.pdf", "/docs/linkrot/2007-dimitrova.pdf"),
 ("/docs/2007-mccmorris.pdf", "/docs/creatine/2007-mcmorris-1.pdf"),
 ("/docs/2007-westerberg.pdf", "/docs/dnb/2007-westerberg.pdf"),
 ("/docs/2007-wolfe%EF%BC%89%EF%BC%9A", "/docs/2007-wolfe"),
 ("/docs/2008-appel.pdf", "/docs/culture/2008-appel.pdf"),
 ("/docs/2008-dahlin.pdf", "/docs/dnb/2008-dahlin.pdf"),
 ("/docs/2008-fennema.pdf", "/docs/sunkcosts/2008-fennema.pdf"),
 ("/docs/2008-gardner.pdf", "/docs/eva/2008-gardner.pdf"),
 ("/docs/2008-gonzalezalvarez.pdf", "/docs/dnb/2008-gonzalezalvarez.pdf"),
 ("/docs/2008-jaeggi.pdf", "/docs/dnb/2008-jaeggi.pdf"),
 ("/docs/2008-kern.pdf", "/docs/conscientiousness/2008-kern.pdf"),
 ("/docs/2008-kuriyama.pdf", "/docs/dnb/2008-kuriyama.pdf"),
 ("/docs/2008-minzenberg.pdf", "/docs/modafinil/2008-minzenberg.pdf"),
 ("/docs/2008-rawson.pdf", "/docs/creatine/2008-rawson.pdf"),
 ("/docs/2008-wren.pdf", "/docs/linkrot/2008-wren.pdf"),
 ("/docs/2009-alloway.pdf", "/docs/dnb/2009-alloway.pdf"),
 ("/docs/2009-anda.pdf", "/docs/dnb/2009-anda.pdf"),
 ("/docs/2009-poropat.pdf", "/docs/conscientiousness/2009-poropat.pdf"),
 ("/docs/2009-qiu.pdf", "/docs/dnb/2009-qiu.pdf"),
 ("/docs/2009-qiu.pdfIn", "/docs/dnb/2009-qiu.pdf"),
 ("/docs/2010-alloway.pdf", "/docs/dnb/2010-alloway.pdf"),
 ("/docs/2010-borella.pdf", "/docs/dnb/2010-borella.pdf"),
 ("/docs/2010-chein.pdf", "/docs/dnb/2010-chein.pdf"),
 ("/docs/2010-chooi-table.pdf", "/docs/dnb/2010-chooi-table.pdf"),
 ("/docs/2010-colom.pdf", "/docs/dnb/2010-colom.pdf"),
 ("/docs/2010-crc", "/docs/eva/2010-crc"),
 ("/docs/2010-jaeggi.pdf", "/docs/dnb/2010-jaeggi.pdf"),
 ("/docs/2010-moe.pdf", "/docs/dnb/2010-moe.pdf"),
 ("/docs/2010-peen.pdf", "/docs/nature/2010-peen.pdf"),
 ("/docs/2010-schmeichel.pdf", "/docs/dnb/2010-schmeichel.pdf"),
 ("/docs/2010-seidler.pdf", "/docs/dnb/2010-seidler.pdf"),
 ("/docs/2010-zeidan.pdf", "/docs/dnb/2010-zeidan.pdf"),
 ("/docs/2011-bidwell-adhd.pdf", "/docs/nicotine/2011-bidwell-adhd.pdf"),
 ("/docs/2011-davishttp://www.fastcompany.com/1785445/bitcoin-crypto-currency-mystery-reopenedSymantec", "/docs/2011-davis"),
 ("/docs/2011-eisenegger.pdf", "/docs/nootropics/2011-eisenegger.pdf"),
 ("/docs/2011-falchi.pdf", "/docs/melatonin/2011-falchi.pdf"),
 ("/docs/2011-gwern-yourmorals.org/rel_norms_process.html", "/docs/personal/2011-gwern-yourmorals.org/rel_norms_process.html"),
 ("/docs/2011-gwern-yourmorals.org/sacredness_cartoon_process.html", "/docs/personal/2011-gwern-yourmorals.org/sacredness_cartoon_process.html"),
 ("/docs/2011-house", "/docs/eva/2011-house"),
 ("/docs/2011-levine.pdf", "/docs/nicotine/2011-levine.pdf"),
 ("/docs/2011-loosli.pdf", "/docs/dnb/2011-loosli.pdf"),
 ("/docs/2011-lynch.pdf", "/docs/algernon/2011-lynch.pdf"),
 ("/docs/2011-muflax-backup.pdf", "/docs/linkrot/2011-muflax-backup.pdf"),
 ("/docs/2011-roughan.pdf", "/docs/dnb/2011-roughan.pdf"),
 ("/docs/2011-schubert.pdf", "/docs/dnb/2011-schubert.pdf"),
 ("/docs/2011-shiran.pdf", "/docs/dnb/2011-shiran.pdf"),
 ("/docs/2011-soderqvist.pdf", "/docs/dnb/2011-soderqvist.pdf"),
 ("/docs/2011-wade.pdf", "/docs/melatonin/2011-wade.pdf"),
 ("/docs/2011-zhao.pdf", "/docs/dnb/2011-zhao.pdf"),
 ("/docs/2012-barton.pdf", "/docs/nature/2012-barton.pdf"),
 ("/docs/2012-chooi.pdf", "/docs/dnb/2012-chooi.pdf"),
 ("/docs/2012-election-statemargin", "/docs/elections/2012-statemargin.csv"),
 ("/docs/2012-gominak.pdf", "/docs/zeo/2012-gominak.pdf"),
 ("/docs/2012-green.pdf", "/docs/dnb/2012-green.pdf"),
 ("/docs/2012-hartwig.pdf", "/docs/spacedrepetition/2012-hartwig.pdf"),
 ("/docs/2012-matricciani.pdf", "/docs/melatonin/2012-matricciani.pdf"),
 ("/docs/2012-mitchell.pdf", "/docs/dnb/2012-mitchell.pdf"),
 ("/docs/2012-penner.pdf", "/docs/dnb/2012-penner.pdf"),
 ("/docs/2012-shipstead.pdf", "/docs/dnb/2012-shipstead.pdf"),
 ("/docs/2012-studerluethi.pdf", "/docs/dnb/2012-studerluethi.pdf"),
 ("/docs/2012-takeuchi.pdf", "/docs/dnb/2012-takeuchi.pdf"),
 ("/docs/2012-woodley.pdf", "/docs/algernon/2012-woodley.pdf"),
 ("/docs/2012-zimmerman.pdf", "/docs/iodine/2012-zimmerman.pdf"),
 ("/docs/2013-button.pdf", "/docs/dnb/2013-button.pdf"),
 ("/docs/2013-gwern-gjp-forecastresults.pdf", "/docs/personal/2013-gwern-gjp-forecastresults.pdf"),
 ("/docs/2013-heal.pdf", "/docs/modafinil/2013-heal.pdf"),
 ("/docs/2013-kidd.pdf", "/docs/culture/2013-kidd.pdf"),
 ("/docs/2013-rietveld.pdf", "/docs/iq/2013-rietveld.pdf"),
 ("/docs/2013-romagna.pdf", "/docs/nicotine/2013-romagna.pdf"),
 ("/docs/2014-sariaslan-1.pdf", "/docs/genetics/2014-sariaslan-1.pdf"),
 ("/docs/2014-sariaslan-2.pdf", "/docs/genetics/2014-sariaslan-2.pdf"),
 ("/docs/2015-johnson.pdf", "/docs/iq/2015-johnson.pdf"),
 ("/docs/2015-mathieson.pdf", "/docs/genetics/2015-mathieson.pdf"),
 ("/docs/2015-polderman.pdf", "/docs/genetics/2015-polderman.pdf"),
 ("/docs/2015-robinson.pdf", "/docs/genetics/2015-robinson.pdf"),
 ("/docs/2015-rottensteiner.pdf", "/docs/genetics/2015-rottensteiner.pdf"),
 ("/docs/algernon/2006-harris.pdff", "/docs/algernon/2006-harris.pdf"),
 ("/docs/chein2010.pdf", "/docs/dnb/2010-chein.pdf"),
 ("/docs/dnb/2000-butzlaff", "/docs/dnb/2000-butzlaff.pdf"),
 ("/docs/dnb/2002-hallam.pdf", "/docs/music-distraction/2002-hallam.pdf"),
 ("/docs/dnb/2011-young.pdf", "/docs/statistics/2011-young.pdf"),
 ("/docs/dnb/2012-perham.pdf", "/docs/music-distraction/2012-perham.pdf"),
 ("/docs/dnb/2013-brodsky.pdf", "/docs/music-distraction/2013-brodsky.pdf"),
 ("/docs/eva/1997-animeland-may-hideakianno-intervie", "/docs/eva/1997-animeland-may-hideakianno-interview"),
 ("/docs/eva/1997-animeland-may-hideakianno-interview-english.page", "/docs/eva/1997-animeland-may-hideakianno-interview-english"),
 ("/docs/eva/1997-animeland-may-hideakianno-interview-english;", "/docs/eva/1997-animeland-may-hideakianno-interview-english"),
 ("/docs/eva/2002-notenki-memoirs.page", "/docs/eva/2002-notenki-memoirs"),
 ("/docs/genetics/2012-plomin.pdf", "/docs/genetics/2012-plomin-behavioralgenetics.pdf"),
 ("/docs/genetics/2015-power.pdf", "/docs/genetics/correlation/2015-power.pdf"),
 ("/docs/genetics/2015-weight.pdf", "/docs/genetics/2016-weight.pdf"),
 ("/docs/genetics/2016-day.pdf", "/docs/genetics/correlation/2016-day.pdf"),
 ("/docs/genetics/2016-hyde.pdf", "/docs/genetics/correlation/2016-hyde.pdf"),
 ("/docs/genetics/2016-pickrell.pdf", "/docs/genetics/correlation/2016-pickrell.pdf"),
 ("/docs/genetics/2016-rees.pdf", "/docs/genetics/correlation/2016-rees.pdf"),
 ("/docs/genetics/2016-tucker-drob.pdf", "/docs/genetics/correlation/2016-tucker-drob.pdf"),
 ("/docs/genetics/2016-zheng-ldhub-49x49geneticcorrelation.csv", "/docs/genetics/correlation/2016-zheng-ldhub-49x49geneticcorrelation.csv"),
 ("/docs/gwern-bw-stats.txt", "/docs/personal/gwern-bw-stats.txt"),
 ("/docs/gwern-goodreads.csv", "/docs/personal/gwern-goodreads.csv"),
 ("/docs/gwern-google-reader-subscriptions.xml", "/docs/personal/gwern-google-reader-subscriptions.xml"),
 ("/docs/iq/2016-okbay-2", "/docs/iq/2016-okbay-2.pdf"),
 ("/docs/jaeggi2010.pdf", "/docs/dnb/2010-jaeggi.pdf"),
 ("/docs/melatonin/melatonin/2003-vandongen.pdf", "/docs/melatonin/2003-vandongen.pdf"),
 ("/docs/nicotine/1992-warburton.p", "/docs/nicotine/1992-warburton.pdf"),
 ("/docs/nootropics/1990-schrauzer.pdf", "/docs/lithium/1990-schrauzer.pdf"),
 ("/docs/personal/aotdisposition_process.html", "/docs/personal/2011-gwern-yourmorals.org/aotdisposition_process.html"),
 ("/docs/power.29", "/docs/sr/2013-power"),
 ("/docs/predictions/2001-principlesforecasting.pdfTime", "/docs/predictions/2001-principlesforecasting.pdf"),
 ("/docs/qiu2009.pdf", "/docs/dnb/2009-qiu.pdf"),
 ("/docs/spacedrepetition/1978-baddeley.pdf%20", "/docs/spacedrepetition/1978-baddeley.pdf"),
 ("/docs/spacedrepetition/2010-seamon.pdfJohn", "/docs/spacedrepetition/2010-seamon.pdf"),
 ("/docs/sr/2016-06-02-heraldscotland-topixt.mht", "/docs/sr/2016-06-02-heraldscotland-topix2.mht"),
 ("/docs/sr/modafinil/2013-05-28.tar.xz", "/docs/modafinil/blackmarkets/2013-05-28.tar.xz"),
 ("/docs/sr/modafinil/2013-07-03.tar.xz", "/docs/modafinil/blackmarkets/2013-07-03.tar.xz"),
 ("/docs/sr/modafinil/2013-08-03.tar.xz", "/docs/modafinil/blackmarkets/2013-08-03.tar.xz"),
 ("/docs/src/2016-ho.pdf", "/docs/sr/2016-ho.pdf"),
 ("/docs/statistics/2016-findley.pdf", "/docs/statistics/peerreview/2016-findley.pdf"),
 ("/docs/sunkcosts/1995-hoang.pdfStaw", "/docs/sunkcosts/1995-hoang.pdf"),
 ("/docs/teika", "/docs/japanese/teika"),
 ("/docs/zeidan2010.pdf", "/docs/dnb/2010-zeidan.pdf"),
 ("/docs/zeo/gwern", "/docs/zeo/gwern-zeodata.csv"),
 ("/drug%20heuristics?2", "/Drug%20heuristics"),
 ("/genetics/docs/genetics/2015-robinson.pdf", "/docs/genetics/2015-robinson.pdf"),
 ("/girl%20scouts%20and%20good%20governance", "/Girl%20Scouts%20and%20good%20governance"),
 ("/haskell/Archiving%20GitHub.html", "/haskell/Archiving%20GitHub"),
 ("/haskell/Wikipedia%20RSS%20Archive%20Bot.html", "/haskell/Wikipedia%20RSS%20Archive%20Bot"),
 ("/haskell/Wikipedia%20RSS%20Archive%20Bot3", "/haskell/Wikipedia%20RSS%20Archive%20Bot"),
 ("/in%20defense%20of%20inclusionism", "/In%20Defense%20Of%20Inclusionism"),
 ("/index.html", "/index"),
 ("/links", "/Links"),
 ("/lunar%20sleep", "/Lunar%20sleep"),
 ("/m/docs/2006-feist.pdf", "/docs/2006-feist.pdf"),
 ("/m/docs/melatonin/2010-serfaty.pdf", "/docs/melatonin/2010-serfaty.pdf"),
 ("/melatonin", "/Melatonin"),
 ("/mobile/docs/2006-feist.pdf", "/docs/2006-feist.pdf"),
 ("/mobile/docs/melatonin/2010-serfaty.pdf", "/docs/melatonin/2010-serfaty.pdf"),
 ("/modafinil/2015-10-27-modafinilsurvey-", "/docs/modafinil/survey/2015-10-27-modafinilsurvey.csv"),
 ("/modafinilhow", "/Modafinil"),
 ("/modafinilpeak", "/Modafinil"),
 ("/modafinilvice", "/Modafinil"),
 ("/nicotine", "/Nicotine"),
 ("/nootropics", "/Nootropics"),
 ("/power.29", "/docs/sr/2013-power.page"),
 ("/self-decrypting%20files", "/Self-decrypting%20files"),
 ("/sicp/Chapter%201.3.html", "/sicp/Chapter%201.3"),
 ("/silk%20road", "/Silk%20Road"),
 ("/silk%20road", "/Silk%20road"),
 ("/silk%20road?revision=20121015013418-f7719-c2d41233ee27eacdb0252a7c6b6f975a30bb220b", "/Silk%20Road"),
 ("/spaced%20repetition", "/Spaced%20repetition"),
 ("/spaced%20repetition", "/Spaced%20repetition"),
 ("/subscribe", "https://gwern.us3.list-manage.com/subscribe?u=ed1f6b2799208b40f5f18be8f&id=91b4d51355"),
 ("/zeo", "/Zeo")
 ]
