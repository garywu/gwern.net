#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

{- Debian dependencies:
$ sudo apt-get install libghc-hakyll-dev libghc-pandoc-dev libghc-filestore-dev libghc-feed-dev libghc-tagsoup-dev imagemagick s3cmd git

(GHC is needed for Haskell; Hakyll & Pandoc do the heavy lifting of compiling Markdown files to HTML; filestore queries the git history for the RSS feed, and feed turns it into an RSS feed; tag soup & ImageMagick are runtime dependencies used to help optimize images, and s3cmd/git upload to hosting/Github respectively.)

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
import Hakyll (applyTemplateList, buildTags, compile, compressCssCompiler, constField,
               copyFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions,
               defaultHakyllWriterOptions, fromCapture, getRoute, hakyll, idRoute, itemIdentifier,
               loadAll, loadAndApplyTemplate, loadBody, makeItem, match, modificationTimeField,
               pandocCompilerWithTransform, preprocess, relativizeUrls, route, setExtension,
               tagsField, tagsRules, templateCompiler, version, Compiler, Context, Identifier, Item, Pattern, Tags)
import Hakyll.Web.Redirect (createRedirects)
import System.Exit (ExitCode(ExitFailure))
import Text.HTML.TagSoup (renderTagsOptions,parseTags,renderOptions, optMinimize, Tag(TagOpen))
import Text.Pandoc (bottomUp, nullAttr, Extension(Ext_markdown_in_html_blocks), HTMLMathMethod(MathML), Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), ReaderOptions(..), WriterOptions(..))

main :: IO ()
main = hakyll $ do -- RSS/ATOM setup
             preprocess $ do rss <- filestoreToXmlFeed rssConfig (gitFileStore "./")  Nothing
                             createDirectoryIfMissing False "_site"
                             writeFile "_site/atom.xml" rss

             -- create static redirect pages for outdated/broken incoming links (goes first so any collisions with content, the redirects will lose)
             version "redirects" $ createRedirects brokenLinks

             -- handle the simple static non-.page files
             let static = route idRoute >> compile copyFileCompiler
             version "static" $ mapM_ (`match` static) [
                                     "docs/**",
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
    constField "confidence" "log" <>
    constField "description" "N/A" <>
    constField "importance" "0"

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

-- | Mapping of gwern.net URLs people mistakenly visit with current correct URL, based on Google Webmaster Tool report 2016-12-31.
--   For use with `createRedirects`.
brokenLinks :: [(Identifier,FilePath)]
brokenLinks = [
   ("2015-10-27-modafinilsurvey-feedback.csv", "/docs/modafinil/survey/2015-10-27-modafinilsurvey-feedback.csv")
 , ("2docs/dnb/1978-zimmer.pdf", "/docs/music-distraction/1978-zimmer.pdf")
 , ("AB", "/AB%20testing")
 , ("Aboot", "/About")
 , ("about", "/About")
 , ("About.html", "/About")
 , ("a-b-testing", "/AB%20testing")
 , ("adrafinil", "/Modafinil")
 , ("Archivin", "/Archiving%20URLs")
 , ("Archiving%20URLs.html", "/Archiving%20URLs")
 , ("Archiving", "/Archiving%20URLs")
 , ("archiving urls", "/Archiving%20URLs")
 , ("Aria's past", "/Aria's%20past,%20present,%20and%20future")
 , ("atom.xml\"", "/atom.xml")
 , ("Bitcoin", "/Bitcoin%20is%Worse%20is%20Better")
 , ("bitcoin is worse is better", "/Bitcoin%20is%Worse%20is%20Better")
 , ("Black- arrests", "/Black-market%20arrests")
 , ("Black-market%25archives", "/Black-market%20archives")
 , ("Black-market archive", "/Black-market%20archives")
 , ("black-market archives", "/Black-market%20archives")
 , ("Black- market archives", "/Black-market%20archives")
 , ("Black-market%archives", "/Black-market%20archives")
 , ("Black-market+archives", "/Black-market%20archives")
 , ("Black-marketarchives", "/Black-market%20archives")
 , ("Black-Market-archives", "/Black-market%20archives")
 , ("Black-market archivesDec", "/Black-market%20archives")
 , ("Black-market archivesdeepdotweb.com/2015/07/16/gwern-archives-of-all-dark-net", "/Black-market%20archives")
 , ("Black-market archives", "/DNM%20archives")
 , ("black-market arrests", "/Black-market%20arrests")
 , ("Black-market arrests.", "/Black-market%20arrests")
 , ("Black-marketarrests", "/Black-market%20arrests")
 , ("Blackmarket arrests", "/Black-market%20arrests")
 , ("Black-market arrests", "/DNM%20arrests")
 , ("Black-market ", "/Black-market%20archives")
 , ("Black-market_survival", "/Black-market%20survival")
 , ("Black-market!survival", "/Black-market%20survival")
 , ("black-market survival", "/Black-market survival")
 , ("Black-market survival", "/DNM%20survival")
 , ("Black-market", "/tags/Silk%20Road")
 , ("Black-", "/tags/Silk%20Road")
 , ("Book%20reading%20list.html", "/Book%20reviews")
 , ("Book", "/Book%20reviews")
 , ("Book reading list", "/Book%20reviews")
 , ("Book reading list.html", "/Book%20reviews")
 , ("Book reviewsSimilarAug", "/Book%20reviews")
 , ("Candy", "/Candy%20Japan")
 , ("causality", "/Causality")
 , ("changelog", "/Changelog")
 , ("choosing software", "/Choosing%20Software")
 , ("coin-flip", "/Coin-flip")
 , ("Complexity", "/Complexity%20vs%20AI")
 , ("Culture%20is%2", "/Culture%20is%20not%20about%20Esthetics")
 , ("Culture is not about esthetics", "/Culture%20is%20not%20about%20Esthetics")
 , ("death note anonymity", "/Death%20Note%20anonymity")
 , ("Death Note anonymity", "/Death Note Anonymity")
 , ("Death+Note+Anonymity", "/Death Note Anonymity")
 , ("Death Note Ending.html", "/Death%20Note%20Ending")
 , ("DNB20FAQbrbrOr", "/DNB%20FAQ")
 , ("DNB%2520FAQ", "/DNB%20FAQ")
 , ("DNB", "/DNB%20FAQ")
 , ("DNB%", "/DNB%20FAQ")
 , ("dnb faq", "/DNB%20FAQ")
 , ("DNB FAQ!", "/DNB%20FAQ")
 , ("DNB FAQ~~HEAD=dobj", "/DNB%20FAQ")
 , ("DNB FAQOder", "/DNB%20FAQ")
 , ("DNB FAQOppure", "/DNB%20FAQ")
 , ("DNB FAQOu", "/DNB%20FAQ")
 , ("DNB FAQSau", "/DNB%20FAQ")
 , ("dnb f", "/DNB%20FAQ")
 , ("DNB F", "/DNB%20FAQ")
 , ("DNB+meta-analysis", "/DNB%20meta-analysis")
 , ("dnm archives", "DNM%20archives")
 , ("dnm arrests", "/DNM%20arrests")
 , ("DNM", "/tags/Silk%20Road")
 , ("doc/music-distraction/1999 furnham-2", "/doc/music-distraction/1999-furnham-2.pdf")
 , ("docs/1912-culler.pdf", "/docs/spacedrepetition/1912-culler.pdf")
 , ("docs/1916-murphy.pdf", "/docs/spacedrepetition/1916-murphy.pdf")
 , ("docs/1939-spitzer.pdf", "/docs/spacedrepetition/1939-spitzer.pdf")
 , ("docs/1950-gagne.pdf", "/docs/spacedrepetition/1950-gagne.pdf")
 , ("docs/1956-kurtz.pdf", "/docs/spacedrepetition/1956-kurtz.pdf")
 , ("docs/1963-peterson.pdf", "/docs/spacedrepetition/1963-peterson.pdf")
 , ("docs/1967-keppel.pdf", "/docs/spacedrepetition/1967-keppel.pdf")
 , ("docs/1968-cohen.pdf", "/docs/statistics/1968-cohen.pdf")
 , ("docs/1968-fierro-ben%C3%ADtez.pdf", "/docs/iodine/1968-fierro-benítez.pdf")
 , ("docs/1968-fierro-benítez.pdf", "/docs/iodine/1968-fierro-benítez.pdf")
 , ("docs/1971-pharoah.pdf", "/docs/iodine/1971-pharoah.pdf")
 , ("docs/1972-fierro-ben%C3%ADtez.pdf", "/docs/iodine/1962-fierro-benítez.pdf")
 , ("docs/1972-fierro-benítez.pdf", "/docs/iodine/1972-fierro-benítez.pdf")
 , ("docs/1972-pharoah.pdf", "/docs/iodine/1972-pharoah.pdf")
 , ("docs/1974-fierro-ben%C3%ADtez.pdf", "/docs/iodine/1974-fierro-benítez.pdf")
 , ("docs/1974-fierro-benítez.pdf", "/docs/iodine/1974-fierro-benítez.pdf")
 , ("docs/1975-brown.pdfMaking", "/docs/1975-brown.pdf")
 , ("docs/1975-laporte.pdf", "/docs/spacedrepetition/1975-laporte.pdf")
 , ("docs/1976-carman.pdf", "/docs/melatonin/1976-carman.pdf")
 , ("docs/1976-efron.pdf", "/docs/statistics/1976-efron.pdf")
 , ("docs/1977-glenberg.pdf", "/docs/spacedrepetition/1977-glenberg.pdf")
 , ("docs/1978-baddeley.pdf", "/docs/spacedrepetition/1978-baddeley.pdf")
 , ("docs/1978-fischhoff.pdf", "/docs/predictions/1978-fischhoff.pdf")
 , ("docs/1979-mendlewicz.pdf", "/docs/melatonin/1979-mendlewicz.pdf")
 , ("docs/1979-weatherhead.pdf", "/docs/sunkcosts/1979-weatherhead.pdf")
 , ("docs/1980-damos.pdf", "/docs/dnb/1980-damos.pdf")
 , ("docs/1980-dawkins.pdf", "/docs/sunkcosts/1980-dawkins.pdf")
 , ("docs/1981-biermann.pdf", "/docs/sunkcosts/1981-biermann.pdf")
 , ("docs/1981-brockner.pdf", "/docs/sunkcosts/1981-brockner.pdf")
 , ("docs/1981-duchastel", "/docs/spacedrepetition/1981-duchastel")
 , ("docs/1981-duchastel.pdf", "/docs/spacedrepetition/1981-duchastel.pdf")
 , ("docs/1981-staw.pdf", "/docs/sunkcosts/1981-staw.pdf")
 , ("docs/1982-bazerman.pdf", "/docs/sunkcosts/1982-bazerman.pdf")
 , ("docs/1982-brockner.pdf", "/docs/sunkcosts/1982-brockner.pdf")
 , ("docs/1982-nungester.pdf", "/docs/spacedrepetition/1982-nungester.pdf")
 , ("docs/1983-lee.pdf", "/docs/spacedrepetition/1983-lee.pdf")
 , ("docs/1983-regelmann.pdf", "/docs/sunkcosts/1983-regelmann.pdf")
 , ("docs/1984-bazerman.pdf", "/docs/sunkcosts/1984-bazerman.pdf")
 , ("docs/1985-godfrey.pdf", "/docs/dnb/1985-godfrey.pdf")
 , ("docs/1985-hofstadte......-lottery", "/docs/1985-hofstadter")
 , ("docs/1985-mckenna.pdf", "/docs/spacedrepetition/1985-mckenna.pdf")
 , ("docs/1985-pirolli.pdf", "/docs/spacedrepetition/1985-pirolli.pdf")
 , ("docs/1986-davis.pdf", "/docs/sunkcosts/1986-davis.pdf")
 , ("docs/1986-osberg.pdf", "/docs/predictions/1986-osberg.pdf")
 , ("docs/1986-yeh.pdf", "/docs/nootropics/1986-yeh.pdf")
 , ("docs/1987-adams.pdf", "/docs/spacedrepetition/1987-adams.pdf")
 , ("docs/1987-ayton.pdf", "/docs/statistics/1987-ayton.pdf")
 , ("docs/1987-bahrick.pdf", "/docs/spacedrepetition/1987-bahrick.pdf")
 , ("docs/1987-curio.pdf", "/docs/sunkcosts/1987-curio.pdf")
 , ("docs/1987-krull.pdf", "/docs/math/1987-krull.pdf")
 , ("docs/1987-pocock.pdf", "/docs/dnb/1987-pocock.pdf")
 , ("docs/1987-smith.pdf", "/docs/dnb/1987-smith.pdf")
 , ("docs/1988-ammons.pdf", "/docs/spacedrepetition/1988-ammons.pdf")
 , ("docs/1988-armstrong.pdf", "/docs/sunkcosts/1988-armstrong.pdf")
 , ("docs/1988-bastuji.pdf", "/docs/modafinil/1988-bastuji.pdf")
 , ("docs/1988-montgomerie.pdf", "/docs/sunkcosts/1988-montgomerie.pdf")
 , ("docs/1988-newell.pdf", "/docs/spacedrepetition/1988-newell.pdf")
 , ("docs/1989-burger.pdf", "/docs/sunkcosts/1989-burger.pdf")
 , ("docs/1989-glover.pdf", "/docs/spacedrepetition/1989-glover.pdf")
 , ("docs/1989-shimojo.pdf", "/docs/statistics/1989-shimojo.pdf")
 , ("docs/1990-garland.pdf", "/docs/sunkcosts/1990-garland.pdf")
 , ("docs/1990-larrick.pdf", "/docs/sunkcosts/1990-larrick.pdf")
 , ("docs/1990-rothman.pdf", "/docs/dnb/1990-rothman.pdf")
 , ("docs/1990-schrauzer.pdf", "/docs/lithium/1990-schrauzer.pdf")
 , ("docs/1990-thomason.pdf", "/docs/math/1990-thomason.pdf")
 , ("docs/1990-wells.pdf", "/docs/math/1990-wells.pdf")
 , ("docs/1991-drennan.pdf", "/docs/zeo/1991-drennan.pdf")
 , ("docs/1991-garland.pdf", "/docs/sunkcosts/1991-garland.pdf")
 , ("docs/1991-phillips.pdf", "/docs/sunkcosts/1991-phillips.pdf")
 , ("docs/1991-shute.pdf", "/docs/dnb/1991-shute.pdf")
 , ("docs/1991-shute.pdf--gwern--You", "/docs/dnb/1991-shute.pdf")
 , ("docs/1992-berry.pdf", "/docs/algernon/1992-berry.pdf")
 , ("docs/1992-heikinheimo.pdf", "/docs/nootropics/1992-heikinheimo.pdf")
 , ("docs/1992-warburton.pdf", "/docs/nicotine/1992-warburton.pdf")
 , ("docs/1993-conlon.pdf", "/docs/sunkcosts/1993-conlon.pdf")
 , ("docs/1993-cooper.pdf", "/docs/sunkcosts/1993-cooper.pdf")
 , ("docs/1993-everson.pdf", "/docs/algernon/1993-everson.pdf")
 , ("docs/1993-friedman.pdf", "/docs/conscientiousness/1993-friedman.pdf")
 , ("docs/1993-mccarthy.pdf", "/docs/sunkcosts/1993-mccarthy.pdf")
 , ("docs/1993-schrauzer.pdf", "/docs/lithium/1993-schrauzer.pdf")
 , ("docs/1993-warot.pdf", "/docs/modafinil/1993-warot.pdf")
 , ("docs/1993-whyte.pdf", "/docs/sunkcosts/1993-whyte.pdf")
 , ("docs/1994-falk", "/docs/statistics/1994-falk")
 , ("docs/1994-falk.pdf", "/docs/statistics/1994-falk.pdf")
 , ("docs/1994-graesser.pdf", "/docs/spacedrepetition/1994-graesser.pdf")
 , ("docs/1994-heishman.pdf", "/docs/nicotine/1994-heishman.pdf")
 , ("docs/1994-krueger.pd", "/docs/conscientiousness/1994-krueger.pdf")
 , ("docs/1994-krueger.pdf", "/docs/conscientiousness/1994-krueger.pdf")
 , ("docs/1994-lehouezec.pdf", "/docs/nicotine/1994-lehouezec.pdf")
 , ("docs/1994-shrestha.pdf", "/docs/iodine/1994-shrestha.pdf")
 , ("docs/1995-gold.pdf", "/docs/modafinil/1995-gold.pdf")
 , ("docs/1995-hoang.pdf", "/docs/sunkcosts/1995-hoang.pdf")
 , ("docs/1995-kramer.pdf", "/docs/dnb/1995-kramer.pdf")
 , ("docs/1995-lagarde.pdf", "/docs/modafinil/1995-lagarde.pdf")
 , ("docs/1995-rechtschaffen.pdf", "/docs/algernon/1995-rechtschaffen.pdf")
 , ("docs/1995-simon.pdf", "/docs/modafinil/1995-simon.pdf")
 , ("docs/1995-tan.pdf", "/docs/sunkcosts/1995-tan.pdf")
 , ("docs/1996-animerica-conscience-otaking", "/docs/eva/1996-animerica-conscience-otaking")
 , ("docs/1996-conners.pdf", "/docs/nicotine/1996-conners.pdf")
 , ("docs/1996-coren.pdf", "/docs/melatonin/1996-coren.pdf")
 , ("docs/1996-costa.pdf", "/docs/conscientiousness/1996-costa.pdf")
 , ("docs/1996-csada.pdf", "/docs/dnb/1996-csada.pdf")
 , ("docs/1996-foulds.pdf", "/docs/nicotine/1996-foulds.pdf")
 , ("docs/1996icosta.pdf", "/docs/conscientiousness/1996-costa.pdf")
 , ("docs/1996-slater.pdf", "/docs/culture/1996-slater.pdf")
 , ("docs/1996-spyker.pdf", "/docs/modafinil/1996-spyker.pdf")
 , ("docs/1996-steele.pdf", "/docs/sunkcosts/1996-steele.pdf")
 , ("docs/1997-fainstein.pdf", "/docs/melatonin/1997-fainstein.pdf")
 , ("docs/1997-hatton.pdf", "/docs/dnb/1997-hatton.pdf")
 , ("docs/1998-dales-truthinmathematics.pdf", "/docs/math/1998-dales-truthinmathematics.pdf")
 , ("docs/1998-feskanich.pdf", "/docs/nootropics/1998-feskanich.pdf")
 , ("docs/1998-garland.pdf", "/docs/sunkcosts/1998-garland.pdf")
 , ("docs/1998-greenland.pdf", "/docs/nicotine/1998-greenland.pdf")
 , ("docs/1998-henneberg.pdf", "/docs/algernon/1998-henneberg.pdf")
 , ("docs/1998-lewy.pdf", "/docs/melatonin/1998-lewy.pdf")
 , ("docs/1998-ricklefs.pdf", "/docs/algernon/1998-ricklefs.pdf")
 , ("docs/1998-stivalet.pdf", "/docs/modafinil/1998-stivalet.pdf")
 , ("docs/1999-batejat.pdf", "/docs/modafinil/1999-batejat.pdf")
 , ("docs/1999-bradbury", "/docs/1999-bradbury-matrioshkabrains.pdf")
 , ("docs/1999-bradbury-matrioshkabrains.pdf ", "/docs/1999-bradbury-matrioshkabrains.pdf")
 , ("docs/1999-fratiglioni.pdf", "/docs/dnb/1999-fratiglioni.pdf")
 , ("docs/1999-kendrick.pdf", "/docs/modafinil/1999-kendrick.pdf")
 , ("docs/2000-butzlaff.pdf", "/docs/dnb/2000-butzlaff.pdf")
 , ("docs/2000-caldwell.pdf", "/docs/modafinil/2000-caldwell.pdf")
 , ("docs/2000-duval.pdf", "/docs/dnb/2000-duval.pdf")
 , ("docs/2000-fratiglioni.pdf", "/docs/nicotine/2000-fratiglioni.pdf")
 , ("docs/2000-jamieson.pdf", "/docs/spacedrepetition/2000-jamieson.pdf")
 , ("docs/2000-jasinski.pdf", "/docs/modafinil/2000-jasinski.pdf")
 , ("docs/2000-kendrick.pdf", "/docs/modafinil/2000-kendrick.pdf")
 , ("docs/2000-moffatt.pdf", "/docs/nicotine/2000-moffatt.pdf")
 , ("docs/2001-axelsson.pdf", "/docs/nicotine/2001-axelsson.pdf")
 , ("docs/2001-berlin.pdf", "/docs/nicotine/2001-berlin.pdf")
 , ("docs/2001-klaczynski.pdf", "/docs/sunkcosts//2001-klaczynski.pdf")
 , ("docs/2001-pacchierotti.pdf", "/docs/melatonin/2001-pacchierotti.pdf")
 , ("docs/2001-principlesforecasting.pdf", "/docs/predictions/2001-principlesforecasting.pdf")
 , ("docs/2001-rowe.pdf", "/docs/predictions/2001-rowe.pdf")
 , ("docs/2001-sternberg.pdf", "/docs/dnb/2001-sternberg.pdf")
 , ("docs/2002-ahdaya.pdf", "/docs/nootropics/2002-ahdaya.pdf")
 , ("docs/2002-beracochea.pdf", "/docs/algernon/2002-beracochea.pdf")
 , ("docs/2002-boehne.pdf", "/docs/sunkcosts/2002-boehne.pdf")
 , ("docs/2002-klauer.pdf", "/docs/dnb/2002-klauer.pdf")
 , ("docs/2002-mikuls.pdf", "/docs/nootropics/2002-mikuls.pdf")
 , ("docs/2002-notenki", "/docs/eva/2002-notenki-memoirs")
 , ("docs/2002-notenki-memoirs", "/docs/eva/2002-notenki-memoirs")
 , ("docs/2002-notenki-memoirs.html", "/docs/eva/2002-notenki-memoirs")
 , ("docs/2002-notenki-memoirs.page", "/docs/eva/2002-notenki-memoirs.page")
 , ("docs/2002-rico.pdf", "/docs/nootropics/2002-rico.pdf")
 , ("docs/2002-watanabe.pdf", "/docs/creatine/2002-watanabe.pdf")
 , ("docs/2002-wesensten.pdf", "/docs/modafinil/2002-wesensten.pdf")
 , ("docs/2003-chapotot.pdf", "/docs/modafinil/2003-chapotot.pdf")
 , ("docs/2003-elvers.pdf", "/docs/conscientiousness/2003-elvers.pdf")
 , ("docs/2003-jaeggi.pdf", "/docs/dnb/2003-jaeggi.pdf")
 , ("docs/2003-lounsbury.pdf", "/docs/conscientiousness/2003-lounsbury.pdf")
 , ("docs/2003-murray-human-accomplishment.pdf)", "/docs/2003-murray-human-accomplishment.pdf")
 , ("docs/2003-page.pdf", "/docs/nootropics/2003-page.pdf")
 , ("docs/2003-randall.pdf", "/docs/modafinil/2003-randall.pdf")
 , ("docs/2003-salinpascual.pdf", "/docs/nicotine/2003-salinpascual.pdf")
 , ("docs/2003-turner.pdf", "/docs/modafinil/2003-turner.pdf")
 , ("docs/2003-villegier.pdf", "/docs/nicotine/2003-villegier.pdf")
 , ("docs/2004-baranski.pdf", "/docs/modafinil/2004-baranski.pdf")
 , ("docs/2004-cohen.pdf", "/docs/culture/2004-cohen.pdf")
 , ("docs/2004-crasson.pdf", "/docs/melatonin/2004-crasson.pdf")
 , ("docs/2004-franconi.pdf", "/docs/nootropics/2004-franconi.pdf")
 , ("docs/2004-irani.pdf", "/docs/conscientiousness/2004-irani.pdf")
 , ("docs/2004-kim.pdf", "/docs/conscientiousness/2004-kim.pdf")
 , ("docs/2004-klaczynski.pdf", "/docs/sunkcosts/2004-klaczynski.pdf")
 , ("docs/2004-lesk.pdf", "/docs/nootropics/2004-lesk.pdf")
 , ("docs/2004-lundberg.pdf", "/docs/nicotine/2004-lundberg.pdf")
 , ("docs/2004-makris.pdf", "/docs/modafinil/2004-makris.pdf")
 , ("docs/2004-mueller.pdf", "/docs/modafinil/2004-mueller.pdf")
 , ("docs/2004-okada", "/docs/eva/2004-okada")
 , ("docs/2004-randall.pdf", "/docs/modafinil/2004-randall.pdf")
 , ("docs/2004-ward.pdf", "/docs/algernon/2004-ward.pdf")
 , ("docs/2004-wesensten.pdf", "/docs/modafinil/2004-wesensten.pdf")
 , ("docs/2004-yildiz.pdf", "/docs/nootropics/2004-yildiz.pdf")
 , ("docs/2005-ackerman.pdf", "/docs/dnb/2005-ackerman.pdf")
 , ("docs/2005-bongers-schokking.pdf", "/docs/iodine/2005-bongers-schokking.pdf")
 , ("docs/2005-brzezinski.pdf", "/docs/melatonin/2005-brzezinski.pdf")
 , ("docs/2005-buehner.pdf", "/docs/dnb/2005-buehner.pdf")
 , ("docs/2005-denburg.pdf", "/docs/dnb/2005-denburg.pdf")
 , ("docs/2005-duckworth.pdf", "/docs/dnb/2005-duckworth.pdf")
 , ("docs/2005-dunn.pdf", "/docs/lithium/2005-dunn.pdf")
 , ("docs/2005-fregni.pdf", "/docs/dnb/2005-fregni.pdf")
 , ("docs/2005-hart.pdf", "/docs/modafinil/2005-hart.pdf")
 , ("docs/2005-james.pdf", "/docs/nootropics/2005-james.pdf")
 , ("docs/2005-little-boy", "/docs/eva/2005-little-boy")
 , ("docs/2005-little-boy.pdf", "/docs/eva/2005-little-boy.pdf")
 , ("docs/2005-murakami", "/docs/eva/2005-murakami")
 , ("docs/2005-murakami.pdf", "/docs/eva/2005-murakami.pdf")
 , ("docs/2005-sawaragi", "/docs/eva/2005-sawaragi")
 , ("docs/2005-sawaragi.pdf", "/docs/eva/2005-sawaragi.pdf")
 , ("docs/2005-schniederjans.pdf", "/docs/conscientiousness/2005-schniederjans.pdf")
 , ("docs/2005-unsworth.pdf", "/docs/dnb/2005-unsworth.pdf")
 , ("docs/2005-vorspan.pdf", "/docs/modafinil/2005-vorspan.pdf")
 , ("docs/2006-abma.pdf", "/docs/algernon/2006-abma.pdf")
 , ("docs/2006-abrahms.pdf", "/docs/terrorism/2006-abrahms.pdf")
 , ("docs/2006-amsterdam.pdf", "/docs/nicotine/2006-amsterdam.pdf")
 , ("docs/2006-dinges.pdf", "/docs/nootropics/2006-dinges.pdf")
 , ("docs/2006-francis.pdf", "/docs/nootropics/2006-francis.pdf")
 , ("docs/2006-galluccio.pdf", "/docs/spacedrepetition/2006-galluccio.pdf")
 , ("docs/2006-guillem.pdf", "/docs/nicotine/2006-guillem.pdf")
 , ("docs/2006-harris.pdf", "/docs/algernon/2006-harris.pdf")
 , ("docs/2006-mcmorris.pdf", "/docs/creatine/2006-mcmorris.pdf")
 , ("docs/2006-selvi.pdf", "/docs/zeo/2006-selvi.pdf")
 , ("docs/2006-terao.pdf", "/docs/lithium/2006-terao.pdf")
 , ("docs/2006-vaishnavi.pdf", "/docs/modafinil/2006-vaishnavi.pdf")
 , ("docs/2007-abrahms-2.pdf", "/docs/terrorism/2007-abrahms-2.PDF")
 , ("docs/2007-bishopclark.pdf", "/docs/conscientiousness/2007-bishopclark.pdf")
 , ("docs/2007-danigelis.pdf", "/docs/dnb/2007-danigelis.pdf")
 , ("docs/2007-dimitrova.pdf", "/docs/linkrot/2007-dimitrova.pdf")
 , ("docs/2007-gouzouasis.pdf", "/docs/dnb/2007-gouzouasis.pdf")
 , ("docs/2007-guzzetta.pdf", "/docs/lithium/2007-guzzetta.pdf")
 , ("docs/2007-mccmorris-1.pdf", "/docs/creatine/2007-mcmorris-1.pdf")
 , ("docs/2007-mccmorris.pdf", "/docs/creatine/2007-mcmorris-1.pdf")
 , ("docs/2007-morgan.pdf", "/docs/algernon/2007-morgan.pdf")
 , ("docs/2007-noftle.pdf", "/docs/conscientiousness/2007-noftle.pdf")
 , ("docs/2007-taneja.pdf", "/docs/modafinil/2007-taneja.pdf")
 , ("docs/2007-teigen.pdf", "/docs/statistics/2007-teigen.pdf")
 , ("docs/2007-westerberg.pdf", "/docs/dnb/2007-westerberg.pdf")
 , ("docs/2007-wittman.pdf", "/docs/algernon/2007-wittman.pdf")
 , ("docs/2007-wolfe%EF%BC%89%EF%BC%9A", "/docs/2007-wolfe")
 , ("docs/2008-appel.pdf", "/docs/culture/2008-appel.pdf")
 , ("docs/2008-boggio.pdf", "/docs/dnb/2008-boggio.pdf")
 , ("docs/2008-dahlin.pdf", "/docs/dnb/2008-dahlin.pdf")
 , ("docs/2008-fennema.pdf", "/docs/sunkcosts/2008-fennema.pdf")
 , ("docs/2008-gardner.pdf", "/docs/eva/2008-gardner.pdf")
 , ("docs/2008-gerlach.pdf", "/docs/nicotine/2008-gerlach.pdf")
 , ("docs/2008-gonzalezalvarez.pdf", "/docs/dnb/2008-gonzalezalvarez.pdf")
 , ("docs/2008-gonzalez.pdf", "/docs/lithium/2008-gonzalez.pdf")
 , ("docs/2008-jaeggi.pdf", "/docs/dnb/2008-jaeggi.pdf")
 , ("docs/2008-jost.pdf", "/docs/terrorism/2008-jost.pdf")
 , ("docs/2008-kern.pdf", "/docs/conscientiousness/2008-kern.pdf")
 , ("docs/2008-kueber.pdf", "/docs/modafinil/2008-kueber.pdf")
 , ("docs/2008-kumar.pdf", "/docs/modafinil/2008-kumar.pdf")
 , ("docs/2008-kuriyama.pdf", "/docs/dnb/2008-kuriyama.pdf")
 , ("docs/2008-macdonald.pdf", "/docs/lithium/2008-macdonald.pdf")
 , ("docs/2008-minzenberg.pdf", "/docs/modafinil/2008-minzenberg.pdf")
 , ("docs/2008-rawson.pdf", "/docs/creatine/2008-rawson.pdf")
 , ("docs/2008-wren.pdf", "/docs/linkrot/2008-wren.pdf")
 , ("docs/2008-yeh.pdf", "/docs/lithium/2008-yeh.pdf")
 , ("docs/2009-alloway.pdf", "/docs/dnb/2009-alloway.pdf")
 , ("docs/2009-anda.pdf", "/docs/dnb/2009-anda.pdf")
 , ("docs/2009-copplestone.pdf", "/docs/algernon/2009-copplestone.pdf")
 , ("docs/2009-derrick.pdf", "/docs/culture/2009-derrick.pdf")
 , ("docs/2009-hampel.pdf", "/docs/lithium/2009-hampel.pdf")
 , ("docs/2009-lecacheux.pdf", "/docs/nicotine/2009-lecacheux.pdf")
 , ("docs/2009-nemanich.pdf", "/docs/conscientiousness/2009-nemanich.pdf")
 , ("docs/2009-phillips.pdf", "/docs/modafinil/2009-phillips.pdf")
 , ("docs/2009-poropat.pdf", "/docs/conscientiousness/2009-poropat.pdf")
 , ("docs/2009-qiu.pdf", "/docs/dnb/2009-qiu.pdf")
 , ("docs/2009-qiu.pdfIn", "/docs/dnb/2009-qiu.pdf")
 , ("docs/2009-silberberg.pdf", "/docs/algernon/2009-silberberg.pdf")
 , ("docs/2009-sinclair.pdf", "/docs/math/2009-sinclair.pdf")
 , ("docs/2009-zehdner.pdf", "/docs/dnb/2009-zehdner.pdf")
 , ("docs/201010-201102-gwern.net-analytics.pdf", "/docs/traffic/201010-201102-gwern.net-analytics.pdf")
 , ("docs/2010-alloway.pdf", "/docs/dnb/2010-alloway.pdf")
 , ("docs/2010-borella.pdf", "/docs/dnb/2010-borella.pdf")
 , ("docs/2010-chein.pdf", "/docs/dnb/2010-chein.pdf")
 , ("docs/2010-chooi-table.pdf", "/docs/dnb/2010-chooi-table.pdf")
 , ("docs/2010-colom.pdf", "/docs/dnb/2010-colom.pdf")
 , ("docs/2010-cook.pdf", "/docs/algernon/2010-cook.pdf")
 , ("docs/2010-crc", "/docs/eva/2010-crc")
 , ("docs/2010-jaeggi.pdf", "/docs/dnb/2010-jaeggi.pdf")
 , ("docs/2010-judde.pdf", "/docs/music-distraction/2010-judde.pdf")
 , ("docs/2010-koh.pdf", "/docs/eva/2010-koh.pdf")
 , ("docs/2010-lee.pdf", "/docs/culture/2010-lee.pdf")
 , ("docs/2010-moe.pdf", "/docs/dnb/2010-moe.pdf")
 , ("docs/2010-oneill.pdf", "/docs/spacedrepetition/2010-oneill.pdf")
 , ("docs/2010-peen.pdf", "/docs/nature/2010-peen.pdf")
 , ("docs/2010-schmeichel.pdf", "/docs/dnb/2010-schmeichel.pdf")
 , ("docs/2010-seidler.pdf", "/docs/dnb/2010-seidler.pdf")
 , ("docs/2010-zeidan.pdf", "/docs/dnb/2010-zeidan.pdf")
 , ("docs/20110702-20120102-gwern.net-analytics.pdf", "docs/traffic/20110702-20120102-gwern.net-analytics.pdf")
 , ("docs/2011-abreu.pdf", "/docs/nootropics/2011-abreu.pdf")
 , ("docs/2011-bidwell-adhd.pdf", "/docs/nicotine/2011-bidwell-adhd.pdf")
 , ("docs/2011-dannenbaum.pdf", "/docs/terrorism/2011-dannenbaum.pdf")
 , ("docs/2011-davishttp://www.fastcompany.com/1785445/bitcoin-crypto-currency-mystery-reopenedSymantec", "/docs/2011-davis")
 , ("docs/2011-eisenegger.pdf", "/docs/nootropics/2011-eisenegger.pdf")
 , ("docs/2011-falchi.pdf", "/docs/melatonin/2011-falchi.pdf")
 , ("docs/2011-gooneratne.pdf", "/docs/melatonin/2011-gooneratne.pdf")
 , ("docs/2011-gwern-yourmorals.org/5f_new2_process.html", "/docs/personal/2011-gwern-yourmorals.org/5f_new2_process.html")
 , ("docs/2011-gwern-yourmorals.org/aos_process.html", "/docs/personal/2011-gwern-yourmorals.org/aos_process.html")
 , ("docs/2011-gwern-yourmorals.org/aotdisposition_process.html", "/docs/personal/2011-gwern-yourmorals.org/aotdisposition_process.html")
 , ("docs/2011-gwern-yourmorals.org/bigfive_process.html", "/docs/personal/2011-gwern-yourmorals.org/bigfive_process.html")
 , ("docs/2011-gwern-yourmorals.org/busethics_process.html", "/docs/personal/2011-gwern-yourmorals.org/busethics_process.html")
 , ("docs/2011-gwern-yourmorals.org/clarity_process.html", "/docs/personal/2011-gwern-yourmorals.org/clarity_process.html")
 , ("docs/2011-gwern-yourmorals.org/cogref_process.html", "/docs/personal/2011-gwern-yourmorals.org/cogref_process.html")
 , ("docs/2011-gwern-yourmorals.org/crowne_process.html", "/docs/2011-gwern-yourmorals.org/crowne_process.html")
 , ("docs/2011-gwern-yourmorals.org/current_events_process.html", "/docs/personal/2011-gwern-yourmorals.org/current_events_process.html")
 , ("docs/2011-gwern-yourmorals.org/diessnerebs_process.html", "/docs/personal/2011-gwern-yourmorals.org/diessnerebs_process.html")
 , ("docs/2011-gwern-yourmorals.org/disgust_process.html", "/docs/personal/2011-gwern-yourmorals.org/disgust_process.html")
 , ("docs/2011-gwern-yourmorals.org/docs/personal/2011-gwern-yourmorals.org/ztpi_process.html", "/docs/personal/2011-gwern-yourmorals.org/docs/personal/2011-gwern-yourmorals.org/ztpi_process.html")
 , ("docs/2011-gwern-yourmorals.org/epbs2_process.html", "/docs/personal/2011-gwern-yourmorals.org/epbs2_process.html")
 , ("docs/2011-gwern-yourmorals.org/epq_process.html", "/docs/personal/2011-gwern-yourmorals.org/epq_process.html")
 , ("docs/2011-gwern-yourmorals.org/general_political_knowledge_process.html", "/docs/personal/2011-gwern-yourmorals.org/general_political_knowledge_process.html")
 , ("docs/2011-gwern-yourmorals.org/gov_econ_process.html", "/docs/personal/2011-gwern-yourmorals.org/gov_econ_process.html")
 , ("docs/2011-gwern-yourmorals.org/justice_process.html", "/docs/personal/2011-gwern-yourmorals.org/justice_process.html")
 , ("docs/2011-gwern-yourmorals.org/mindfulness_process.html", "/docs/personal/2011-gwern-yourmorals.org/mindfulness_process.html")
 , ("docs/2011-gwern-yourmorals.org/mis_process.html", "/docs/personal/2011-gwern-yourmorals.org/mis_process.html")
 , ("docs/2011-gwern-yourmorals.org/relmob_process.html", "/docs/personal/2011-gwern-yourmorals.org/relmob_process.html")
 , ("docs/2011-gwern-yourmorals.org/rel_norms_process.html", "/docs/personal/2011-gwern-yourmorals.org/rel_norms_process.html")
 , ("docs/2011-gwern-yourmorals.org/sacredness_cartoon_process.html", "/docs/personal/2011-gwern-yourmorals.org/sacredness_cartoon_process.html")
 , ("docs/2011-gwern-yourmorals.org/schwartz_process.html", "/docs/personal/2011-gwern-yourmorals.org/schwartz_process.html")
 , ("docs/2011-gwern-yourmorals.org/srpsf_process.html", "/docs/personal/2011-gwern-yourmorals.org/srpsf_process.html")
 , ("docs/2011-gwern-yourmorals.org/stories_process.html", "/docs/personal/2011-gwern-yourmorals.org/stories_process.html")
 , ("docs/2011-gwern-yourmorals.org/systems_feelings_process.html", "/docs/personal/2011-gwern-yourmorals.org/systems_feelings_process.html")
 , ("docs/2011-gwern-yourmorals.org/ztpi_process.html", "docs/personal/2011-gwern-yourmorals.org/ztpi_process.html")
 , ("docs/2011-house", "/docs/eva/2011-house")
 , ("docs/2011-house.page", "/docs/eva/2011-house.page")
 , ("docs/2011-jaeggi-poster.pdf", "/docs/dnb/2011-jaeggi-poster.pdf")
 , ("docs/2011-levine.pdf", "/docs/nicotine/2011-levine.pdf")
 , ("docs/2011-loosli.pdf", "/docs/dnb/2011-loosli.pdf")
 , ("docs/2011-lynch.pdf", "/docs/algernon/2011-lynch.pdf")
 , ("docs/2011-mccabe.pdf", "/docs/spacedrepetition/2011-mccabe.pdf")
 , ("docs/2011-mccabe.pdf]", "/docs/spacedrepetition/2011-mccabe.pdf")
 , ("docs/2011-mindframe.pdf", "/docs/eva/2011-mindframe.pdf")
 , ("docs/2011-muflax-backup.pdf", "/docs/linkrot/2011-muflax-backup.pdf")
 , ("docs/2011-raval.pdf", "/docs/nicotine/2011-raval.pdf")
 , ("docs/2011-rindermann.pdf", "https://lesacreduprintemps19.files.wordpress.com/2011/05/rindermann-and-thompson-2011-cognitive-capitalism.pdf")
 , ("docs/2011-roughan.pdf", "/docs/dnb/2011-roughan.pdf")
 , ("docs/2011-schubert.pdf", "/docs/dnb/2011-schubert.pdf")
 , ("docs/2011-shiran", "/docs/dnb/2011-shiran.pdf")
 , ("docs/2011-shiran.pdf", "/docs/dnb/2011-shiran.pdf")
 , ("docs/2011-soderqvist.pdf", "/docs/dnb/2011-soderqvist.pdf")
 , ("docs/2011-wade.pdf", "/docs/melatonin/2011-wade.pdf")
 , ("docs/2011-zhao.pdf", "/docs/dnb/2011-zhao.pdf")
 , ("docs/20120103-20120702-gwern.net-analytics.pdf", "/docs/traffic/20120103-20120702-gwern.net-analytics.pdf")
 , ("docs/20120703-20130102-gwern.net-analytics.pdf", "/docs/traffic/20120703-20130102-gwern.net-analytics.pdf")
 , ("docs/2012-amer.pdf", "/docs/nootropics/2012-amer.pdf")
 , ("docs/2012-barton.pdf", "/docs/nature/2012-barton.pdf")
 , ("docs/2012-bedrosian.pdf", "/docs/melatonin/2012-bedrosian.pdf")
 , ("docs/2012-burgess.pdf", "/docs/melatonin/2012-burgess.pdf")
 , ("docs/2012-cannon.pdf", "/docs/nature/2012-cannon.pdf")
 , ("docs/2012-chooi.pdf", "/docs/dnb/2012-chooi.pdf")
 , ("docs/2012-echeverria.pdf", "/docs/nicotine/2012-echeverria.pdf")
 , ("docs/2012-election-gwern-notes.txt", "/docs/elections/2012-gwern-notes.txt")
 , ("docs/2012-election-linzer-statewinprob.csv", "/docs/elections/2012-linzer-statewinprob.csv")
 , ("docs/2012-election-presidential.csv", "/docs/elections/2012-presidential.csv")
 , ("docs/2012-election-senatemargin.csv", "/docs/elections/2012-senatemargin.csv")
 , ("docs/2012-election-senatewin.csv", "/docs/elections/2012-senatewin.csv")
 , ("docs/2012-election-statemargin.csv", "/docs/elections/2012-statemargin.csv")
 , ("docs/2012-election-statemargin", "/docs/elections/2012-statemargin.csv")
 , ("docs/2012-election-statewin.csv", "/docs/elections/2012-statewin.csv")
 , ("docs/2012-estrada.pdf", "/docs/modafinil/2012-estrada.pdf")
 , ("docs/2012-fu.pdf", "/docs/genetics/2012-fu.pdf")
 , ("docs/2012-geng.pdf", "/docs/modafinil/2012-geng.pdf")
 , ("docs/2012-gominak.pdf", "/docs/zeo/2012-gominak.pdf")
 , ("docs/2012-green.pdf", "/docs/dnb/2012-green.pdf")
 , ("docs/2012-gwern-personalityproject-2.html", "/docs/personal/2012-gwern-personalityproject-2.html")
 , ("docs/2012-gwern-personalityproject-2.html?utm_source=RSS&utm_medium=feed&utm_campaign=1", "/docs/personal/2012-gwern-personalityproject-2.html")
 , ("docs/2012-hartwig.pdf", "/docs/spacedrepetition/2012-hartwig.pdf")
 , ("docs/2012-huang.pdf", "/docs/zeo/2012-huang.pdf")
 , ("docs/2012-kelley.pdf", "/docs/modafinil/2012-kelley.pdf")
 , ("docs/2012-klein.pdf", "/docs/algernon/2012-klein.pdf")
 , ("docs/2012-lithium-experiment.csv", "/docs/lithium/2012-lithium-experiment.csv")
 , ("docs/2012-masicampo.pdf", "/docs/dnb/2012-masicampo.pdf")
 , ("docs/2012-matricciani.pdf", "/docs/melatonin/2012-matricciani.pdf")
 , ("docs/2012-mitchell.pdf", "/docs/dnb/2012-mitchell.pdf")
 , ("docs/2012-penner.pdf", "/docs/dnb/2012-penner.pdf")
 , ("docs/2012-perham.pdf", "/docs/music-distraction/2012-perham.pdf")
 , ("docs/2012-shipstead.pdf", "/docs/dnb/2012-shipstead.pdf")
 , ("docs/2012-shuman.pdf", "/docs/modafinil/2012-shuman.pdf")
 , ("docs/2012-son.pdf", "/docs/spacedrepetition/2012-son.pdf")
 , ("docs/2012-studerluethi.pdf", "/docs/dnb/2012-studerluethi.pdf")
 , ("docs/2012-susser.pdf", "/docs/spacedrepetition/2012-susser.pdf")
 , ("docs/2012-takeuchi.pdf", "/docs/dnb/2012-takeuchi.pdf")
 , ("docs/2012-woodley.pdf", "/docs/algernon/2012-woodley.pdf")
 , ("docs/2012-wylie.pdf", "/docs/nicotine/2012-wylie.pdf")
 , ("docs/2012-young-2.pdf", "/docs/culture/2012-young-2.pdf")
 , ("docs/2012-young.pdf", "/docs/culture/2012-young.pdf")
 , ("docs/2012-zeo-vitamind-morning-control.csv", "/docs/zeo/2012-zeo-vitamind-morning-control.csv")
 , ("docs/2012-zimmerman.pdf", "/docs/iodine/2012-zimmerman.pdf")
 , ("docs/20130103-20130702-gwern.net-analytics.pdf", "/docs/traffic/20130103-20130702-gwern.net-analytics.pdf")
 , ("docs/20130703-20140102-gwern.net-analytics.pdf", "/docs/traffic/20130703-20140102-gwern.net-analytics.pdf")
 , ("docs/2013-bluml.pdf", "/docs/lithium/2013-bluml.pdf")
 , ("docs/2013-button.pdf", "/docs/dnb/2013-button.pdf")
 , ("docs/2013-elpus.pdf", "/docs/dnb/2013-elpus.pdf")
 , ("docs/2013-gwern-gjp-forecastresults.pdf", "/docs/personal/2013-gwern-gjp-forecastresults.pdf")
 , ("docs/2013-gwernnet-anonymousfeedback.csv", "/docs/traffic/2013-gwernnet-anonymousfeedback.csv")
 , ("docs/2013-gwern-personalityproject.html", "/docs/personal/2013-gwern-personalityproject.html")
 , ("docs/2013-gwern-touhoumusic-torrent.csv", "/docs/touhou/2013-torrent.csv")
 , ("docs/2013-hahn.pdf", "/docs/nicotine/2013-hahn.pdf")
 , ("docs/2013-heal.pdf", "/docs/modafinil/2013-heal.pdf")
 , ("docs/2013-heinzel.pdf", "/docs/dnb/2013-heinzel.pdf")
 , ("docs/2013-kidd.pdf", "/docs/culture/2013-kidd.pdf")
 , ("docs/2013-quisenberry.pdf", "/docs/modafinil/2013-quisenberry.pdf")
 , ("docs/2013-rietveld.pdf", "/docs/iq/2013-rietveld.pdf")
 , ("docs/2013-romagna.pdf", "/docs/nicotine/2013-romagna.pdf")
 , ("docs/2013-vartanian.pdf", "/docs/dnb/2013-vartanian.pdf")
 , ("docs/20140103-20140702-gwern.net-analytics.pdf", "/docs/traffic/20140103-20140702-gwern.net-analytics.pdf")
 , ("docs/20140703-20150103-gwern.net-analytics.pdf", "/docs/traffic/20140703-20150103-gwern.net-analytics.pdf")
 , ("docs/2014-sariaslan-1.pdf", "/docs/genetics/2014-sariaslan-1.pdf")
 , ("docs/2014-sariaslan-2.pdf", "/docs/genetics/2014-sariaslan-2.pdf")
 , ("docs/2014-simons.pdf", "/docs/genetics/2014-simons.pdf")
 , ("docs/2014-simons-supplementary.pdf", "/docs/genetics/2014-simons-supplementary.pdf")
 , ("docs/2015-gauk", "/docs/2015-gaukler.pdf")
 , ("docs/2015-henn.pdf", "/docs/genetics/2015-henn.pdf")
 , ("docs/2015-johnson.pdf", "/docs/iq/2015-johnson.pdf")
 , ("docs/2015-mathieson.pdf", "/docs/genetics/2015-mathieson.pdf")
 , ("docs/2015-polderman.pdf", "/docs/genetics/2015-polderman.pdf")
 , ("docs/2015-polderman-supplement-1.pdf", "/docs/genetics/2015-polderman-supplement-1.pdf")
 , ("docs/2015-polderman-supplement-2.xlsx", "/docs/genetics/2015-polderman-supplement-2.xlsx")
 , ("docs/2015-robinson.pdf", "/docs/genetics/2015-robinson.pdf")
 , ("docs/2015-rottensteiner.pdf", "/docs/genetics/2015-rottensteiner.pdf")
 , ("docs/2016-shor…", "/docs/2016-shortland.pdf")
 , ("docs/algernon/2006-harris.pdff", "/docs/algernon/2006-harris.pdf")
 , ("docs/chein2010.pdf", "/docs/dnb/2010-chein.pdf")
 , ("docs/colom2010.pdf", "/docs/dnb/2010-colom.pdf")
 , ("docs/conscientiousness/2013-", "/docs/conscientiousness/2013-fariba.pdf")
 , ("docs/dnb/1945-henderson.pdf", "/docs/music-distraction/1945-henderson.pdf")
 , ("docs/dnb/1952-freeburne.pdf", "/docs/music-distraction/1952-freeburne.pdf")
 , ("docs/dnb/1989-mayfield.pdf", "/docs/music-distraction/1989-mayfield.pdf")
 , ("docs/dnb/2000-butzlaff", "/docs/dnb/2000-butzlaff.pdf")
 , ("docs/dnb/2002-furnham.pdf", "/docs/music-distraction/2002-furnham.pdf")
 , ("docs/dnb/2002-hallam.pdf", "/docs/music-distraction/2002-hallam.pdf")
 , ("docs/dnb/2003-pool.pdf", "/docs/music-distraction/2003-pool.pdf")
 , ("docs/dnb/2004-costa-giomi.pdfrnec", "/docs/dnb/2004-costa-giomi.pdf")
 , ("docs/dnb/2005-lesiuk.pdf", "/docs/music-distraction/2005-lesiuk.pdf")
 , ("docs/dnb/2007-anderson.pdf", "/docs/music-distraction/2007-anderson.pdf")
 , ("docs/dnb/2007-mccmorris-1.pdf", "/docs/creatine/2007-mcmorris-1.pdf")
 , ("docs/dnb/2007-mcmorris-2.pdf",  "/docs/creatine/2007-mcmorris-2.pdf")
 , ("docs/dnb/2008-rawson.pdf", "/docs/creatine/2008-rawson.pdf")
 , ("docs/dnb/2010-judde.pdf", "/docs/music-distraction/2010-judde.pdf")
 , ("docs/dnb/2011-young.pdf", "/docs/statistics/2011-young.pdf")
 , ("docs/dnb/2012-doyle.pdf", "/docs/music-distraction/2012-doyle.pdf")
 , ("docs/dnb/2012-perham.pdf", "/docs/music-distraction/2012-perham.pdf")
 , ("docs/dnb/2013-brodsky.pdf", "/docs/music-distraction/2013-brodsky.pdf")
 , ("docs/dnb/2014-perham.pdf", "/docs/music-distraction/2014-perham.pdf")
 , ("docs/eva/1996-newtype-anno-interview.", "/docs/eva/1996-newtype-anno-interview")
 , ("docs/eva/1997-animeland-may-hideakianno-intervie", "/docs/eva/1997-animeland-may-hideakianno-interview")
 , ("docs/eva/1997-animeland-may-hideakianno-interview-english;", "/docs/eva/1997-animeland-may-hideakianno-interview-english")
 , ("docs/eva/2003-oshii-izubuchi.", "/docs/eva/2003-oshii-izubuchi")
 , ("docs/eva/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/eva")
 , ("docs/genetics/2012-plomin.pdf", "/docs/genetics/2012-plomin-behavioralgenetics.pdf")
 , ("docs/genetics/2015-polderman-supplement-", "/docs/genetics/2015-polderman-supplement-1.pdf")
 , ("docs/genetics/2015-power.pdf", "/docs/genetics/correlation/2015-power.pdf")
 , ("docs/genetics/2015-weight.pdf", "/docs/genetics/2016-weight.pdf")
 , ("docs/genetics/2016-day.pdf", "/docs/genetics/correlation/2016-day.pdf")
 , ("docs/genetics/2016-hyde.pdf", "/docs/genetics/correlation/2016-hyde.pdf")
 , ("docs/genetics/2016-pickrell.pdf", "/docs/genetics/correlation/2016-pickrell.pdf")
 , ("docs/genetics/2016-plomin.pdfBy", "/docs/genetics/2016-plomin.pdf")
 , ("docs/genetics/2016-plomin.pdf)", "/docs/genetics/2016-plomin.pdf")
 , ("docs/genetics/2016-rees.pdf", "/docs/genetics/correlation/2016-rees.pdf")
 , ("docs/genetics/2016-tucker-drob.pdf", "/docs/genetics/correlation/2016-tucker-drob.pdf")
 , ("docs/genetics/2016-zheng-ldhub-49x49geneticcorrelation.csv", "/docs/genetics/correlation/2016-zheng-ldhub-49x49geneticcorrelation.csv")
 , ("docs/gwern-bw-stats.txt", "/docs/personal/gwern-bw-stats.txt")
 , ("docs/gwern-goodreads.csv", "/docs/personal/gwern-goodreads.csv")
 , ("docs/gwern-google-cse.xml", "/docs/personal/google-cse.xml")
 , ("docs/gwern-google-cse.xml?revision=20120209203953-f7719-59b402754f9ac826c447e7b446b51a38a9da5ed4", "/docs/personal/google-cse.xml")
 , ("docs/gwern-google-cse.xml?revision=20121112162037-f7719-e491205f42965a79e1f03447442ca9e41214047d", "/docs/personal/google-cse.xml")
 , ("docs/gwern-google-cse.xml?revision=20130103004026-f7719-8ebb4420215499a8e98e46c812f0800e23926c43", "/docs/personal/google-cse.xml")
 , ("docs/gwern-google-reader-subscriptions.xml", "/docs/personal/gwern-google-reader-subscriptions.xml")
 , ("docs/gwern-mal-anime.xml", "/docs/personal/mal-anime.xml")
 , ("docs/gwern-mal-manga.xml", "/docs/personal/mal-manga.xml")
 , ("docs/gwern-mnemosyne-python-hard-way.txt?revision=20110420222417-f7719-177b29b05d0e582a18e076b84358b2024ce57315", "/docs/personal/mnemosyne-python-hard-way.txt")
 , ("docs/gwern-mnemosyne-python-hard-way.txt?revision=20111001160250-f771..", "/docs/personal/mnemosyne-python-hard-way.txt")
 , ("docs/gwern-mnemosyne-python-hard-way.txt?revision=20121013192307-f7719-34f8eb04250cf611e6d4a74ce527c52b54d38d47", "/docs/personal/mnemosyne-python-hard-way.txt")
 , ("docs/gwern-mnemosyne-python-hard-way.txt?revision=20130112192010-f7719-ea0c38a0d7cef392b18a8bca2bda5533d3a0ec34", "/docs/personal/mnemosyne-python-hard-way.txt")
 , ("docs/gwern-mnemosyne-python-hard-way.txt?revision=20130114032532-f7719-ed7d25f3fb88d920fe600c33198fa260e3ed3c7b", "/docs/personal/mnemosyne-python-hard-way.txt")
 , ("docs/gwern-rss-subscriptions.opml", "/docs/personal/rss-subscriptions.opml")
 , ("docs/index.html", "https://github.com/gwern/gwern.net/tree/master/docs")
 , ("docs/iq/2015-stirenze", "/docs/iq/2015-stirenze.pdf")
 , ("docs/iq/2015-stirenze.pdf", "/docs/iq/2015-strenze.pdf")
 , ("docs/iq/2016-okbay-2", "/docs/iq/2016-okbay-2.pdf")
 , ("docs/jaeggi2003.pdf", "/docs/dnb/2003-jaeggi.pdf")
 , ("docs/jaeggi2010.pdf", "/docs/dnb/2010-jaeggi.pdf")
 , ("Docs/melatonin/1998-lewy.PDF", "/docs/melatonin/1998-lewy.pdf")
 , ("docs/melatonin/2001-zhadanova.pdf", "/docs/melatonin/2001-zhdanova.pdf")
 , ("docs/melatonin/2009-burkhart.pdf on 2/16/16", "/docs/melatonin/2009-burkhart.pdf")
 , ("docs/melatonin/2014-boegers.pdf", "/docs/melatonin/2014-boergers.pdf")
 , ("docs/ melatonin/2014-boergers.pdf", "/docs/melatonin/2014-boergers.pdf")
 , ("docs/melatonin/2016-boergers.pdf", "/docs/melatonin/2014-boergers.pdf")
 , ("docs/melatonin/2016oboegers.pdf", "/docs/melatonin/2014-boergers.pdf")
 , ("docs/melatonin/melatonin/2003-vandongen.pdf", "/docs/melatonin/2003-vandongen.pdf")
 , ("docs/music-distraction/1972-fox-pdf", "/docs/music-distraction/1972-fox.pdf")
 , ("docs/music-distraction/1973-mowsesian.pdf&ved=0ahUKEwi3-p-MhLjSAhUBbbwKHUxYDUgQFggZMAA&usg=AFQjCNH9bzZgdXOTgkwkSlb-2a-U6WOI4w&sig2=m1cuH-uKOe_fbC-CF_dZJQ", "/docs/music-distraction/1973-mowsesian.pdf")
 , ("docs/music-distraction/ 1989-mayfield.pdf", "/docs/music-distraction/1989-mayfield.pdf")
 , ("docs/music-istraction/1972-fox.pdf", "/docs/music-distraction/1972-fox.pdf")
 , ("docs/nicotine/1992-warburton.p", "/docs/nicotine/1992-warburton.pdf")
 , ("docs/nicotine/1994-hei", "/docs/nicotine/1994-heishman.pdf")
 , ("docs/nicotine/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/nicotine")
 , ("docs/nootropics/1970-dawson.pdf", "/docs/lithium/1970-dawson.pdf")
 , ("docs/nootropics/1972-dawson.pdf", "/docs/lithium/1972-dawson.pdf")
 , ("docs/nootropics/1972-pokorny.pdf", "/docs/lithium/1972-pokorny.pdf")
 , ("docs/nootropics/1983-walsh.pdf", "/docs/lithium/1983-walsh.pdf")
 , ("docs/nootropics/1990-schrauzer.pdf", "/docs/lithium/1990-schrauzer.pdf")
 , ("docs/nootropics/1993-schrauzer.pdf", "/docs/lithium/1993-schrauzer.pdf")
 , ("docs/nootropics/1995-klemfuss.pdf", "/docs/lithium/1995-klemfuss.pdf")
 , ("docs/nootropics/2005-dunn.pdf", "/docs/lithium/2005-dunn.pdf")
 , ("docs/nootropics/2006-terao.pdf", "/docs/lithium/2006-terao.pdf")
 , ("docs/nootropics/2007-guzzetta.pdf", "/docs/lithium/2007-guzzetta.pdf")
 , ("docs/nootropics/2008-macdonald.pdf", "/docs/lithium/2008-macdonald.pdf")
 , ("docs/nootropics/2008-yeh.pdf", "/docs/lithium/2008-yeh.pdf")
 , ("docs/nootropics/2013-bluml.pdf", "/docs/lithium/2013-bluml.pdf")
 , ("docs/nootropics/2013-giotakos.pdf", "/docs/lithium/2013-giotakos.pdf")
 , ("docs/personal/2011-gwern-yourmorals.../index.html", "/docs/personal/2011-gwern-yourmorals.org/index.html")
 , ("docs/personal/aotdisposition_process.html", "/docs/personal/2011-gwern-yourmorals.org/aotdisposition_process.html")
 , ("docs/personal/gwern-bw-stats.txt", "/docs/dnb/gwern-bw-stats.txt")
 , ("docs/personal/gwern-goodreads.csv", "/docs/personal/goodreads.csv")
 , ("docs/personal/gwern-google-reader-subscriptions.xml", "/docs/personal/rss-subscriptions.opml")
 , ("docs/power.29", "/docs/sr/2013-power")
 , ("docs/predictions/2001-principlesforecasting.pdfTime", "/docs/predictions/2001-principlesforecasting.pdf")
 , ("docs/predictions/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/predictions")
 , ("docs/qiu2009.pdf", "/docs/dnb/2009-qiu.pdf")
 , ("docs/spacedrepetition/1978-baddeley.pdf ", "/docs/spacedrepetition/1978-baddeley.pdf")
 , ("docs/spacedrepetition/2001-maquet.PDF", "/docs/spacedrepetition/2001-maquet.pdf")
 , ("docs/spacedrepetition/2010-seamon.pdfJohn", "/docs/spacedrepetition/2010-seamon.pdf")
 , ("docs/sr/2016-06-02-heraldscotland-topixt.mht", "/docs/sr/2016-06-02-heraldscotland-topix2.mht")
 , ("docs/src/2016-ho.pdf", "/docs/sr/2016-ho.pdf")
 , ("docs/sr/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/sr")
 , ("docs/sr/modafinil/2013-05-28.tar.xz", "/docs/modafinil/blackmarkets/2013-05-28.tar.xz")
 , ("docs/sr/modafinil/2013-07-03.tar.xz", "/docs/modafinil/blackmarkets/2013-07-03.tar.xz")
 , ("docs/sr/modafinil/2013-08-03.tar.xz", "/docs/modafinil/blackmarkets/2013-08-03.tar.xz")
 , ("docs/statistics/1930-stacks.pdf", "/docs/genetics/1930-stocks.pdf")
 , ("docs/statistics/1957-bellman-dynamicprogramming.djvu", "/docs/statistics/1957-bellman-dynamicprogrammer.djvu")
 , ("docs/statistics/2004-hunterschmidt-ethodsofmetaanalysis.pdf", "/docs/statistics/2004-hunterschmidt-methodsofmetaanalysis.pdf")
 , ("docs/statistics/2016-findley.pdf", "/docs/statistics/peerreview/2016-findley.pdf")
 , ("docs/statistics/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/statistics")
 , ("docs/sunkcosts/1995-hoang.pdfStaw", "/docs/sunkcosts/1995-hoang.pdf")
 , ("docs/sunkcosts/1998-", "/docs/sunkcosts/1998-garland.pdf")
 , ("doc/statistics/1933-elderton.pdf", "/docs/statistics/1933-elderton.pdf")
 , ("doc/statistics/2015-pedroza.pdf", "/docs/statistics/2015-pedroza.pdf")
 , ("docs/teika", "/docs/japanese/teika")
 , ("docs/terrorism/2007-abrahms-2.PDF", "/docs/terrorism/2007-abrahms.pdf")
 , ("docs/unsworth2005.pdf", "/docs/dnb/2005-unsworth.pdf")
 , ("docs/zeidan2010.pdf", "/docs/dnb/2010-zeidan.pdf")
 , ("docs/zeo/gwern", "/docs/zeo/gwern-zeodata.csv")
 , ("Drug%20heuristics.", "/Drug%20heuristics")
 , ("Drug", "/Drug%20heuristics")
 , ("drug heuristics?2", "/Drug%20heuristics")
 , ("Drug heuristics.", "/Drug%20heuristics")
 , ("Embro selection", "/Embryo%20selection")
 , ("Embryo", "/Embro%20selection")
 , ("episteme/", "/About#belief-tags")
 , ("Ethical", "/Ethical%20sperm%20donation")
 , ("Evolutionary%20Lic", "/Evolutionary%20Licenses")
 , ("favicon.ico", "/static/img/favicon.ico")
 , ("fiction/2005 Europe trip", "/2005 Europe trip")
 , ("fiction/Cloud%20Nine.html", "/fiction/Cloud%20Nine")
 , ("fiction/Cloud Nine.html", "/fiction/Cloud%20Nine")
 , ("genetics/docs/genetics/2015-robinson.pdf", "/docs/genetics/2015-robinson.pdf")
 , ("Genshiken", "/fiction/Genshiken")
 , ("girl scouts and good governance", "/Girl%20Scouts%20and%20good%20governance")
 , ("Girl Scouts and good governance.html", "/Girl%20Scouts%20and%20good%20governance")
 , ("Google%2520shutdowns", "/Google%20shutdowns")
 , ("Google", "/Google%20shutdowns")
 , ("google shutdowns", "/Google%20shutdowns")
 , ("Google+shutdowns", "/Google%20shutdowns")
 , ("greenland", "Greenland")
 , ("hafu)", "/Hafu")
 , ("haskell/Archiving GitHub.html", "/haskell/Archiving%20GitHub")
 , ("haskell/hakyll.hs", "/hakyll.hs")
 , ("haskell/memo2.hs", "/haskell/mnemo2.hs")
 , ("haskell/memo3.hs", "/haskell/mnemo3.hs")
 , ("haskell/Run%20Length%20Encoding.html", "/haskell/Run%20Length%20Encoding")
 , ("haskell/Run Length Encoding.html", "/haskell/Run%20Length%20Encoding")
 , ("Haskell Summer of Code.html", "/Haskell%20Summer%20of%20Code")
 , ("haskell/Wikipedia%20Archive%20Bot.html", "/haskell/Wikipedia%20Archive%20Bot")
 , ("haskell/Wikipedia RSS Archive Bot3", "/haskell/Wikipedia%20RSS%20Archive%20Bot")
 , ("haskell/Wikipedia RSS Archive Bot.html", "/haskell/Wikipedia%20RSS%20Archive%20Bot")
 , ("images/2011-modalert200-front-pn.jpg", "/images/modafinil/2011-modalert200-front-pn.jpg")
 , ("images/2011-modalert-pill-gb.jpg", "/images/modafinil/2011-modalert-pill-gb.jpg")
 , ("images/silkroad/index.html", "https://github.com/gwern/gwern.net/tree/master/images/silkroad")
 , ("images/spaced-repetition-forgetting-curve-wired-wozniak.jpg", "/images/spacedrepetition/forgetting-curve-wired-wozniak.jpg")
 , ("In%2520Defense%2520Of%2520Inclusionism", "/In%20Defense%20Of%20Inclusionism")
 , ("in defense of inclusionism", "/In%20Defense%20Of%20Inclusionism")
 , ("In Defense of Inclusionism", "/In%20Defense%20Of%20Inclusionism")
 , (")", "/index")
 , ("index.html", "/index")
 , ("komm-susser-tod.html", "/komm-susser-tod")
 , ("links.html", "/Links")
 , ("Links.html", "/Links")
 , ("links", "/Links")
 , ("login", "/index.html")
 , ("longevity", "/Longevity")
 , ("LSD", "/LSD%20microdosing")
 , ("LSD microdosingis", "/LSD%20microdosing")
 , ("lsd microdosing", "/LSD%20microdosing")
 , ("LSDmicrodosing", "/LSD%20microdosing")
 , ("lunar sleep", "/Lunar%20sleep")
 , ("masturbation?revision=20120325151719-f7719-ff650f69fced1e8d7e4e451a2cc43763b96a9519", "/static/404.html")
 , ("masturbation", "/static/404.html")
 , ("m/docs/2006-feist.pdf", "/docs/2006-feist.pdf")
 , ("m/docs/melatonin/2010-serfaty.pdf", "/docs/melatonin/2010-serfaty.pdf")
 , ("Mela", "/Melatonin")
 , ("Melatonin.html", "/Melatonin")
 , ("melatonin", "/Melatonin")
 , ("Melatonin)", "/Melatonin")
 , ("mistakes", "/Mistakes")
 , ("Mnemosyne.html", "/Spaced%20repetition")
 , ("Mnemosyne", "/Spaced%20repetition")
 , ("mobile/docs/2006-feist.pdf", "/docs/2006-feist.pdf")
 , ("mobile/docs/melatonin/2010-serfaty.pdf", "/docs/melatonin/2010-serfaty.pdf")
 , ("Modafinil1", "/Modafinil")
 , ("Modafinil.Buy", "/Modafinil")
 , ("ModafinilCachedModafinil", "/Modafinil")
 , ("modafinil-f512e0b9272f66030ad26824cacffc44.pdf", "/Modafinil")
 , ("modafinilhow", "/Modafinil")
 , ("Modafinil.html", "/Modafinil")
 , ("modafinil", "/Modafinil")
 , ("Modafinil_", "/Modafinil")
 , ("Modafinil-", "/Modafinil")
 , ("Modafinil;", "/Modafinil")
 , ("Modafinil.", "/Modafinil")
 , ("Modafinil)", "/Modafinil")
 , ("Modafinil.Modafinil", "/Modafinil")
 , ("Modafinil.page", "/Modafinil")
 , ("modafinilpeak", "/Modafinil")
 , ("modafinilvice", "/Modafinil")
 , ("Mo", "/Modafinil")
 , ("N-back", "/DNB%20FAQ")
 , ("N-back FAQ", "/DNB%20FAQ")
 , ("N-back FAQ.html", "/DNB%20FAQ")
 , ("N-back FAQThis", "/DNB%20FAQ")
 , ("N", "/DNB%20FAQ")
 , ("neuroscience", "/tags/psychology")
 , ("news", "/Changelog")
 , ("newsletter/2017/1", "/newsletter/2017/01")
 , ("newsletter/index.html", "/tags/newsletter")
 , ("nicotine", "/Nicotine")
 , ("Noopept", "/Nootropics#noopept")
 , ("Noot", "/Nootropics")
 , ("Nootropics.html", "/Nootropics")
 , ("nootropics", "/Nootropics")
 , ("Nootropics-", "/Nootropics")
 , ("NootropicsThis", "/Nootropics")
 , ("Notes.html", "/Notes")
 , ("Nudes", "/index")
 , ("null", "/index")
 , ("On Disrespect.html", "/On%20Disrespect")
 , ("Ontological%20pantheism.html", "/Ontological%20pantheism")
 , ("Ontological pantheism.html", "/Ontological%20pantheism")
 , ("otaku-essay.html", "/otaku-essay")
 , ("otaku.html", "/otaku")
 , ("otaku-", "/otaku-essay")
 , ("plan?revision=20121127035526-f7719-999ce1acdfb62b5c9d7fd096fd1b2d8f7b3df35f", "/static/404.html")
 , ("plan", "/static/404.html")
 , ("power.29", "/docs/sr/2013-power")
 , ("Prediction markets.html", "/Prediction%20markets")
 , ("Prediction Markets", "/Prediction%20markets")
 , ("Prediction", "/Prediction%20markets")
 , ("Resilient Haskell Software.html", "/Resilient%20Haskell%20Software")
 , ("RNN", "/RNN%20metadata")
 , ("rss.xml", "/atom.xml")
 , ("self-decrypting files", "/Self-decrypting%20files")
 , ("Selfdecrypting", "/Self-decrypting%20files")
 , ("sicp/Chapter 1.3.html", "/sicp/Chapter%201.3")
 , ("Silk%20Road3", "/Silk%20Road")
 , ("Silk%25", "/Silk%20Road")
 , ("Silk%2", "/Silk%20Road")
 , ("Silk Road%22", "/Silk%20Road")
 , ("Silk Road3", "/Silk%20Road")
 , ("Silk Road - bitcoin", "/Silk%20Road")
 , ("Silk Road.LargeVisitorGlobe.User", "/Silk%20Road")
 , ("silk road?revision=20121015013418-f7719-c2d41233ee27eacdb0252a7c6b6f975a30bb220b", "/Silk%20Road")
 , ("silk road", "/Silk%20Road")
 , ("silk_road", "/Silk%20Road")
 , ("silk Road", "/Silk%20Road")
 , ("Silk    Road", "/Silk%20Road")
 , ("Silk Road)", "/Silk%20Road")
 , ("Silk ", "/Silk%20Road")
 , ("Silk", "/Silk%20Road")
 , ("Silk%", "/Silk%20Road")
 , ("Simulation%20inferences.html", "/Simulation%20inferences")
 , ("Simulation inferences.html", "/Simulation%20inferences")
 , ("Simulation", "/Simulatin%20inferences")
 , ("Slowing%20Moore", "/Slowing%20Moore's%20Law")
 , ("Slowing%2520Moore%2527s%2520Law", "/Slowing%20Moore's%20Law")
 , ("SlowingMoore%E2%80%99sLaw", "/Slowing%20Moore's%20Law")
 , ("SlowingMoore sLaw", "/Slowing%20Moore's%20Law")
 , ("SlowingMoore’sLaw", "/Slowing%20Moore's%20Law")
 , ("Slowing", "/Slowing%20Moore's%20Law")
 , ("software/backup/", "/Archiving%20URLs")
 , ("spaced%2520repetition", "/Spaced%20repetition")
 , ("Spaced%2520repetition", "/Spaced%20repetition")
 , ("spaced repetition", "/Spaced%20repetition")
 , ("spaced_repetition", "/Spaced%20repetition")
 , ("Spaced repetition.", "/Spaced%20repetition")
 , ("Spaced+repetition", "/Spaced%20repetition")
 , ("Spaced Repetition", "/Spaced%20repetition")
 , ("Spacedrepetition", "/Spaced%20reptition")
 , ("Spaced", "/Spaced%20repetition")
 , ("Spirulina", "/2014%20spirulina")
 , ("Statistical", "/Statistical%20notes")
 , ("story of your life", "/Story%20Of%20Your%20Life")
 , ("Story+Of+Your+Life", "/Story%20Of%20Your%20Life")
 , ("StoryOfYourLife", "/Story%20Of%20Your%20Life")
 , ("Story Of Y…", "/Story%20Of%20Your%20Life")
 , ("subscribe", "https://gwern.us3.list-manage.com/subscribe?u=ed1f6b2799208b40f5f18be8f&id=91b4d51355")
 , ("Sunk costAdding", "/Sunk%20cost")
 , ("Sunk costs", "/Sunk%20cost")
 , ("tags/IQ", "/tags/psychology")
 , ("teas", "/Tea")
 , ("terrorism is not about terror", "/Terrorism%20is%20not%20about%20Terror")
 , ("Terrorism", "/Terrorism%20is%20not%20about%20Terror")
 , ("The%20Melancholy%20of%20Subculture%20Societ", "/The%20Melancholy%20of%20Subculture%20Society")
 , ("The%20Narrowin", "/The%20Narrowing%20Circle")
 , ("The Melancholy of Kyon.html", "/The%20Melancholy%20of%20Kyon")
 , ("the melancholy of subculture society", "/The%20Melancholy%20of%20Subculture%20Society")
 , ("The Three Grenades", "/The%203%20Grenades")
 , ("TODO", "/static/404.html")
 , ("Tool AIs", "/Tool%20AI")
 , ("Tool", "/Tool%20AI")
 , ("Wikipedia%20And%20Other%20Wikis.html", "/Wikipedia%20And%20Other%20Wikis")
 , ("Wikipedia RSS Archive Bot", "/haskell/Wikipedia%20RSS%20Archive%20Bot")
 , ("Woodenpillows", "/Wooden%20pillows")
 , ("Yawgoog", "/Yawgoog%20injustice")
 , ("Zeo%5B1%5D", "/Zeo")
 , ("ZeoCached", "/Zeo")
 , ("Danbooru", "/Danbooru2017")
 , ("d", "/Danbooru2017")
 , ("soylent", "/Soylent")
 , ("docs/rl/armstrong-controlproblem/indedx.html", "docs/rl/armstrong-controlproblem/index.html")
 , ("docs/rl/armstrong-controlproblem/null", "docs/rl/armstrong-controlproblem/index.html")
 , ("docs/spacedrepet", "https://github.com/gwern/gwern.net/tree/master/docs/spacedrepetition")
 , ("docs/spacedrepetition/index.html", "https://github.com/gwern/gwern.net/tree/master/docs/spacedrepetition")
 ]
