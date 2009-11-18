We let [Wikipedia Archive Bot.lhs]() at the point where it could take its stdin (newline-delimited article names), download the English Wikipedia article, parse the HTML for hyperlinks, and fire off an archive request to [WebCitation.org](!Wikipedia).

But where does one get the list of names? Wikipedia [dumps](!Wikipedia "WP:DUMP") are occasionally available, but we have to get them manually, and they are irregularly created. Our bot will never archive links in very new pages because by the time they show up in a database dump and actually get processed, the page is no longer 'very new'. Worse, it tends to be the new pages that most need their hyperlinks backed up in WebCitation, because the [Internet Archive](!Wikipedia) has a rolling 9-month blackout; here too, by the time a link is publicly available in the Internet Archive, the article is no longer new. Sufficiently ephemeral links might never even be spidered by the Internet Archive, for that matter.

Dead links are an issue for new articles because often key claims or their very notability rests on online newspaper articles (frequently paywalled after a few days) or on blogs which might delete away the embarrassing information, or change or disappear for any of a thousand reasons. So we want our bot to be able to handle *new* pages. This is going to require extensive changes.

As usual, we begin by identifying the 'pure' core to our program. We can separate the program into a few pieces of functionality:

1. get the user's email & target wiki
2. somehow acquire a list of 'new' articles by downloading & parsing something on the target wiki
3. download the HTML of those new articles
4. parse said HTML for hyperlinks
5. fire off requests to WebCitation to archive all those hyperlinks

Point #4 is the pure core. A raw webpage is just a long String, and we want a list of URLs, which are String too; so:

> extractURLs :: String -> [String]

As before, our tool of choice is TagSoup. TagSoup supplies `parseTags :: String -> [Tags]`, and then all we need to do is look for any hyperlink which is a HTTP links, a basic [list comprehension](!Wikipedia):

> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
>
> extractURLs :: String -> [String]
> extractURLs page = [x | TagOpen "a" atts <- parseTags page, (_,x) <- atts, "http://" `isPrefixOf` x]

We could also desugar the list comprehension into something uglier like this:

> extractURLs' page = map tagfilter $ parseTags page
> where tagfilter (TagOpen "a" ((x, _):_)) = if "http://" `isPrefixOf` x then x else []
>       tagfilter _ = []

Now we can solve #5. Given `extractURLs`, we fire off the request using `openURL` as before. But note that this time we aren't hardwiring the email WebCitation requires. We'll be getting it from the user later.

> archiveURL :: String -> String -> IO ()
> archiveURL email url = openURL url' >> return ()
>               where url' = "http://www.webcitation.org/archive?url=" ++ escapeURIString isAllowedInURI url ++ "&email=" ++ email

And point #3 is trivial given the URL to new articles, so we are left with #1 and #2. Now, the first thing that should pop into any good Wikipedian's head upon being asked how to get new articles is, 'go to [Special:Newpages](!Wikipedia)!' But how is a poor little bot to deal with that human-centric interface? Fortunately, upon visiting we notice that little RSS icon. That's how we will do it: download the RSS feed for Newpages and parse it.

We will use the [feed](http://hackage.haskell.org/package/feed) library to parse the RSS feed. Looking at the types, we see a RSS feed is a list of RSS items, `[RSSItem]`, and each RSSItem has several fields - but we only need the title. So:

> items :: String -> [RSSItem]
> items feed = rssItems $ rssChannel $ fromJust $ parseFeedString feed

parseFeedString is actually `String -> Maybe Feed`, but we know Wikipedia won't give us a bad feed, so we bypass error checking; then we drill down past the metadata into a RSSChannel (an [RSS channel](http://www.mnot.net/rss/channel.html)[](http://www.w3schools.com/rss/rss_channel.asp) is a finer subdivision of the overall feed; you might have 2 channels, 1 for articles & 1 for pics, or aggregating multiple websites or streams into 1 feed), and then we pull out the RSSItems, and then finally the actual URL links:

> titles :: [RSSItem] -> [String]
> titles itms = map (fromJust . rssItemLink) itms

So we could write:

> parseRSS :: String -> [String]
> parseRSS str = titles $ items str

And then we plug in the IO:

> parseRSSIO :: String -> IO [String]
> parseRSSIO str = fmap parseRSS $ openURL str

So #2 is done. Now for #1, interfacing with the user. We'll just be quick & lazy with some CLI arguments

> main :: IO ()
> main = do (email:website) <- getArgs
>           if (null website) then archiveBot email "http://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss"
>            else archiveBot email website

What is 'archiveBot'? It pulls together all the pieces. We need to download the supplied URL, parse it into further URLs (each being a new article), download each of *those*, parse them into sublists of HTTP links (each to be archived), and then archive each of *those*! Fortunately, with a few higher order functions and the little pieces already assembled, we can do this relatively easily:

> archiveBot :: String -> String -> IO ()
> archiveBot email website = do rss <- parseRSSIO website
>                               articles <- mapM openURL rss
>                               urls <- mapM extractURLs articles
>                               concatMap (archiveURL email) urls
>                               return ()

Tada! This gives us the completed program:

> import Control.Monad (mapM)
> import Data.List (isPrefixOf)
> import Data.Maybe (fromJust)
> import Network.URI (escapeURIString, isAllowedInURI)
> import System.Environment (getArgs)

> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
> import Text.HTML.Download (openURL)

> import Text.Feed.Import (parseFeedString)
> import Text.Feed.Types (Feed(RSSFeed))
> import Text.RSS.Syntax (rssChannel, rssItems, RSSItem(..))


> main :: IO ()
> main = do (email:website:_) <- getArgs
>           if (null website) then archiveBot email "http://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss"
>            else archiveBot email website

> archiveBot :: String -> String -> IO ()
> archiveBot email website = do rss <- parseRSSIO website
>                               articles <- mapM openURL rss
>                               let urls = concat $ map extractURLs articles
>                               mapM (archiveURL email) urls
>                               return ()

> extractURLs :: String -> [String]
> extractURLs page = [x | TagOpen "a" atts <- parseTags page, (_,x) <- atts, "http://" `isPrefixOf` x]

> items :: String -> [RSSItem]
> items feed = let RSSFeed r = fromJust $ parseFeedString feed
>                  in rssItems $ rssChannel r

> titles :: [RSSItem] -> [String]
> titles itms = map (fromJust . rssItemLink) itms

> parseRSS :: String -> [String]
> parseRSS str = titles $ items str

> parseRSSIO :: String -> IO [String]
> parseRSSIO str = fmap parseRSS $ openURL str

> archiveURL :: String -> String -> IO ()
> archiveURL email url = openURL url' >> return ()
>               where url' = "http://www.webcitation.org/archive?url=" ++ escapeURIString isAllowedInURI url ++ "&email=" ++ email

We can tweak it further if we want. [hlint](http://community.haskell.org/~ndm/hlint/)[](http://hackage.haskell.org/package/hlint) will tell us that `archiveBot` could be improved by using `mapM_` instead of `mapM...return ()`, and `concat $ map` is better written `concatMap`. We could probably fuse `items`, `titles`, and `parseRSS` into just one line without hurting readability too much. And as ever, the type signatures are optional, which would save 6 lines.

Now we have our finished prototype. We can compile it and run it like:

> $ wp-archivebot "gwern0@gmail.com"

(Installation is just as easy; this bot is available on Hackage, so it's a `cabal install wp-archivebot` away.)

How's the efficiency? Not too bad, but as with the other bot, we'll find network latency a bottleneck. No reason that the archives couldn't proceed in parallel:

> import Control.Concurrent (forkIO)

> archiveURL email url = forkIO (openURL url') >> return ()

This only parallelizes the archive request. We could independently parallelize, using [MVars](http://haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html) [for synchronization](http://bartoszmilewski.wordpress.com/2009/02/26/message-passing-atoms-mvars/), the downloads of the articles, but now things are getting complex. It'd be better to try to parallelize each article - have each article be a single thread which downloads the article, parses it for links, and fire off archive requests. With lightweight [green thread](!Wikipedia)s, this is actually the best way.

This might sound difficult, but here too writing from the bottom-up with independent functions will make it easier to reorganize the program. In fact, we will only need to modify `archiveBot` a little bit, factoring out the independent bit and wrapping it in a `forkIO`:

> archiveBot :: String -> String -> IO ()
> archiveBot email website = do rss <- parseRSSIO website
>                               mapM (forkIO . archiveOne) rss
>                               return ()
>     where archiveOne :: String -> IO ()
>           archiveOne article =  do text <- openURL article
>                                    let urls = extractURLs text
>                                    mapM (archiveURL email) urls
>                                    return ()

(Hlint will wisely suggest we get rid of the 2 `return ()` statements by just using `mapM_` instead of `mapM`.)

So now if `parseRSSIO` returns 100 entries, we will fork off 100 `archiveOne` workers. Combine this with the parallel version of `archiveURL`, and there would not seem to be no more real parallelism left to use.

What more can we do? Well, as constructed, the bot is one-shot: it downloads the Newpages RSS feed *once*, processes it, and exits.

TODO update with cleaner code

> import Control.Concurrent (forkIO)
> import Control.Monad (liftM, forM_)
> import Data.Char (intToDigit)
> import Data.List (isInfixOf, isPrefixOf, foldl', deleteBy, nubBy)
> import Data.Maybe (fromJust)
> import Network.HTTP hiding (port)
> import Network.Stream (ConnError)
> import Network.URI
> import System.Environment (getArgs)

> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))

> import Text.Feed.Import (parseFeedString)
> import Text.Feed.Types (Feed(RSSFeed))
> import Text.RSS.Syntax (rssChannel, rssItems, RSSItem(..))

> main :: IO ()
> main = do args <- getArgs
>           -- Webcite requires a valid email, and they filter out public
>           -- emails like mailinator.com. So we demand an email from the user.
>           let email = head args
>           -- This is largely intended for the English Wikipedia, so we default to En's NewPages
>           -- but we let the user override; any second argument is assumed to be a MediaWiki RSS
>           let url = if length args > 1 then args !! 1
>                      else "http://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss"
>           reader email url

> -- inspiried by rss2irc
> -- | wait on an RSS thread, updating every so often. Each RSS item links to some diff or page,
> -- in addition to whatever other content the RSS item may contain (date, summary, etc.)
> -- This runs 'archiveBot' on just that link, ignoring the rest.
> reader :: String -> String -> IO ()
> reader email url = items url >>= go
>  where
>    go old = do new <- items url
>                -- remove duplicates
>                let diff = foldl' (flip $ deleteBy matchingTitles) new old
>                forM_ (take 100 diff) $ \itm ->
>                 case rssItemLink itm of
>                     Nothing -> return ()
>                     Just t  -> ignore $ forkIO $ archiveBot email t
>                go new

>    matchingTitles :: RSSItem -> RSSItem -> Bool
>    matchingTitles x y = let title = (fromJust . rssItemTitle) in title x == title y

>    -- Actually fetch a RSS feed and turn it from String to [RSSItem]
>    items :: String -> IO [RSSItem]
>    items rurl = do s <- get' rurl
>                    let RSSFeed r = fromJust $ parseFeedString s
>                    return $ nubBy matchingTitles $ rssItems $ rssChannel r

> -- | Given the URL of an article, we suck down the HTML, grep it for http:// links,
> -- filter out certain links that we don't want to archive (boilerplate links, interwiki links)
> -- and then fire off an archive request for each link left.
> archiveBot :: String -> String -> IO ()
> archiveBot email ls = liftM uniq (fetchArticleURLs ls) >>= mapM_ (archiveURL email)
>  where  uniq :: [String] -> [String] -- So hideous
>         uniq = filter (\x ->not $ any (flip isInfixOf x) exceptions)

>         exceptions :: [String]
>         exceptions = ["wikimediafoundation", "http://www.mediawiki.org/", "wikipedia",
>                       "&curid=", "index.php?title=", "&action="]

> -- | Run 'extractURLs' on some page's raw HTML
> fetchArticleURLs ::  String -> IO [String]
> fetchArticleURLs = fmap extractURLs . get'

> -- | Use the TagSoup library to extract all the hyperlinks in a page. This is really parsing the HTML,
> -- so hopefully there won't be any spurious links.
> extractURLs :: String -> [String]
> extractURLs arg = [x | TagOpen "a" atts <- parseTags arg, (_,x) <- atts, "http://" `isPrefixOf` x]

> -- | Webcitation.org is set up so one can archive a url just by doing a request for 'webcitation.org/archive?url=url&email=email'
> -- So it's very easy, given a URL and an email, to archive a page. You don't even need to see what the response was.
> archiveURL :: String -> String -> IO ()
> archiveURL email url = print url' >> ignore (get' url')
>               where url' = "http://www.webcitation.org/archive?url=" ++ escapeURIString isAllowedInURI url ++ "&email=" ++ email

> -- | Convenient wrapper over the complexity of Network.HTTP. Given a URL, we get the raw HTML. No fuss, no muss.
> -- Of course, this means we paper over a bunch of possible errors and issues, but we've no time for them! There are links to archive!
> get' :: String -> IO String
> get' = get . fromJust . parseURI
>  where get :: URI -> IO String
>        get uri = do resp <- simpleHTTP (request uri) >>= handleE (error . show)
>                     case rspCode resp of
>                         (2,0,0) -> return (rspBody resp)
>                         _ -> error (httpError resp)
>            where
>                httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp
>                showRspCode (a,b,c) = map intToDigit [a,b,c]

>                request :: URI -> Request String
>                request ur = Request{rqURI=ur, rqMethod=GET, rqHeaders=[], rqBody=""}

>                handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
>                handleE h (Left e) = h e
>                handleE _ (Right v) = return v

> -- | Convenience function. 'forkIO' and 'forM_' demand return types of 'IO ()', but most interesting
> -- IO functions don't return void. So one adds a call to 'return ()'; this just factors it out.
> ignore :: Functor f => f a -> f ()
> ignore = fmap $ const ()