{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (mzero, when, unless)
import Data.Aeson.Types (FromJSON(..), (.:), Value(..))
import Data.Aeson (eitherDecode)
import qualified Data.Text    as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.HTTP.Client
import Data.List (sort, intercalate)
import Text.Printf (printf, PrintfArg)
import Control.Error
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)
import Options.Applicative
import System.Exit (exitFailure)
import System.Process (spawnProcess, waitForProcess)

type RedditQuery a = EitherT String IO a

fetch :: String -> B.ByteString -> IO L.ByteString
fetch url q = do
  i <- parseUrl url
  let req = i { queryString = q }
  withManager defaultManagerSettings $ \m ->
    withResponse req m $ \resp -> L.fromStrict . B.concat <$> (brConsume $ responseBody resp)

newtype TempUTCTime = TempUTCTime { fromTempUTCTime :: UTCTime }
                    deriving (Eq, Show, Read, Ord)

tempUTCTimeFromString :: (Applicative m, Monad m) => String -> m TempUTCTime
tempUTCTimeFromString s =
  case parseTime defaultTimeLocale "%s%Q" s of
     Nothing -> fail $ "couldn't parse '" ++ s ++ "' as a unix time!"
     Just t  -> pure $ TempUTCTime t

instance FromJSON TempUTCTime where
  parseJSON (Number n) = pure $ TempUTCTime $ posixSecondsToUTCTime $ realToFrac n
  parseJSON (String s) = tempUTCTimeFromString $ T.unpack s
  parseJSON _          = mzero

data RedditResponse a = RedditResponse { rr_kind :: Text
                                       , rr_data :: RedditListing a
                                       } deriving (Eq, Show, Read, Ord)

data RedditListing a = RedditListing { rl_modhash :: Text
                                     , rl_children :: [PostWrapper a]
                                     } deriving (Eq, Show, Read, Ord)

data PostWrapper a = PostWrapper { pw_kind :: Text
                                 , pw_data :: Post a
                                 } deriving (Eq, Show, Read, Ord)

data Post a = Post { post_score :: Double
                   , post_id :: Text
                   , post_created_utc :: UTCTime
                   , post_title :: Text
                   , post_url :: Text
                   , post_comments :: a
                   , post_subreddit :: Text
                   } deriving (Eq, Show, Read)

instance (Ord a) => Ord (Post a) where
  p `compare` q = post_score q `compare` post_score p

instance (FromJSON a, Integral a) => FromJSON (RedditResponse a) where
  parseJSON (Object v) = RedditResponse <$>
                         v .: "kind" <*>
                         v .: "data"
  parseJSON _          = mzero

instance (FromJSON a, Integral a) => FromJSON (RedditListing a) where
  parseJSON (Object v) = RedditListing <$>
                         v .: "modhash" <*>
                         v .: "children"
  parseJSON _          = mzero

instance (FromJSON a, Integral a) => FromJSON (PostWrapper a) where
  parseJSON (Object v) = PostWrapper <$>
                         v .: "kind" <*>
                         v .: "data"
  parseJSON _          = mzero

instance (FromJSON a, Integral a) => FromJSON (Post a) where
  parseJSON (Object v) = Post <$>
                         v .: "score" <*>
                         v .: "id" <*>
                         (fromTempUTCTime <$> v .: "created_utc") <*>
                         v .: "title" <*>
                         v .: "url" <*>
                         v .: "num_comments" <*>
                         v .: "subreddit"
  parseJSON _          = mzero


getPostsFromResponse :: RedditResponse a -> [Post a]
getPostsFromResponse = map pw_data . rl_children . rr_data

getRedditPosts :: String -> Int -> String -> RedditQuery [Post Int]
getRedditPosts interval count redditName = do
  let q = C.pack $ "sort=top&t=" ++ interval ++ "&limit=" ++ show count
  bs <- scriptIO $ fetch ("http://www.reddit.com/r/" ++ redditName ++ ".json") q
  fmapRT getPostsFromResponse $ hoistEither $ eitherDecode bs

getRedditStats :: String -> Int -> String -> RedditQuery (Double, Double)
getRedditStats interval count redditName = do
  posts <- getRedditPosts interval count redditName
  let scores :: [Double]
      scores = map post_score posts
      n :: Double
      n = fromIntegral $ length scores
  return (sum scores / n, maximum scores)

defaultReddits :: [String]
defaultReddits = ["gifs"
                 , "unexpected"
                 , "whatcouldgowrong"
                 , "nononono"
                 , "holdmybeer"
                 , "idiotsfightingthings"
                 , "instant_regret"
                 , "vastlystupid"]

normalizeReddit :: Integral a => ((Double, Double) -> Double)
                -> (Double, Double) -> [Post a] -> [Post a]
normalizeReddit pick stats = map (\p -> p { post_score = (post_score p) / (pick stats) })

printf' :: PrintfArg a => String -> a -> Text
printf' st = T.pack . printf st

block :: Int -> Text -> Text
block n t | l > wpad     = T.take wpad t `T.append` pad
          | otherwise    = T.justifyLeft n ' ' t
  where
    l = T.length t
    pad = "..."
    lpad = T.length pad
    wpad = n - lpad

displayPost :: PrintfArg a => Post a -> Text
displayPost Post{..} = T.concat [ printf' "[%.2f]\t" post_score
                                , block 20 (T.concat ["(/r/", post_subreddit, ")"]), "\t"
                                , block 30 post_title, "\t"
                                , post_url
                                ]

toGfycat :: Text -> Text
toGfycat url' | T.takeEnd 4 url /= ".gif" = url
              | otherwise                 = handleGif url
  where
    url = case T.breakOnEnd "?" url' of
      ("", u) -> u
      (p, _)  -> T.dropEnd 1 p
    handleGif ur = case T.breakOn "gfycat.com" ur of
      (u, "")   -> "http://gfycat.com/fetch/" `T.append` u
      (_, post) -> "http://" `T.append` (T.dropEnd 4 post)

isYoutube :: Text -> Bool
isYoutube t = case T.breakOn "youtube.com" t of
  (_, "") -> case T.breakOn "youtu.be" t of
    (_, "") -> False
    (_, _)  -> True
  (_, _)  -> True

after :: UTCTime -> Post a -> Bool
after cutoff p = diffUTCTime (post_created_utc p) cutoff > 0

byMean :: (a, a) -> a
byMean = fst
byMax :: (a, a) -> a
byMax = snd

oneDay :: Num a => a
oneDay = 86400

main :: IO ()
main = do
  (Opts mode count maxAge' open openCmd youtube nogfycat quiet reddits') <- execParser opts
  let reddits = if null reddits' then defaultReddits else reddits'
  res <- runEitherT $ do
    now <- scriptIO $ getCurrentTime
    rawPosts <- mapM (getRedditPosts "day" 50) reddits
    posts <- case mode of
              Normalized -> do
                stats <- mapM (getRedditStats "month" 500) reddits
                return $ concat $ zipWith (normalizeReddit byMean) stats rawPosts
              Absolute   -> return $ concat rawPosts
    let maxAge = (-(fromIntegral maxAge') * oneDay) `addUTCTime` now
        ytf = if youtube then id else filter (not.isYoutube.post_url)
        gfy = if nogfycat then id else map (\p -> p { post_url = toGfycat (post_url p) })
        postList = take count $ sort $ gfy $ ytf $ filter (after maxAge) $ posts
    when open $ scriptIO $ do
      handles <- mapM (spawnProcess openCmd . pure . T.unpack . post_url) postList
      mapM_ waitForProcess handles

    return $ map displayPost $ postList
  case res of
   Left e -> do
     putStrLn $ "Error: " ++ e
     exitFailure
   Right m -> unless quiet $ T.putStrLn $ T.unlines m

data Ranking = Normalized | Absolute

data Opts = Opts { opt_mode         :: Ranking
                 , opt_count        :: Int
                 , opt_max_age      :: Int
                 , opt_open         :: Bool
                 , opt_open_command :: String
                 , opt_youtube      :: Bool
                 , opt_nogfycat     :: Bool
                 , opt_quiet        :: Bool
                 , opt_reddits      :: [String]
                 }

opts :: ParserInfo Opts
opts = info (helper <*> parseOpts)
       (fullDesc
        <> progDesc ("Fetch top links from a bunch of subreddits.  If you specify no subreddits on the command line, these default subreddits will be used: " ++ intercalate ", " defaultReddits)
        <> header "reddit")

parseOpts :: Parser Opts
parseOpts = Opts <$> flag Normalized Absolute
            ( long "absolute"
              <> short 'A'
              <> help "Rank links by absolute post score (the multi-reddit default)")
            <*> option auto
            ( long "count"
              <> short 'c'
              <> metavar "COUNT"
              <> help "Output the top COUNT posts"
              <> showDefault
              <> value 20)
            <*> option auto
            ( long "max-age"
              <> short 'a'
              <> help "Ignore posts older than AGE days"
              <> metavar "AGE"
              <> showDefault
              <> value 1)
            <*> switch
            ( long "open"
              <> short 'o'
              <> help "Open each link with a command (use --command)")
            <*> strOption
            ( long "command"
              <> short 'C'
              <> metavar "COMMAND"
              <> help "Specify the command to call on each URL when '--open' is passed"
              <> showDefault
              <> value "x-www-browser")
            <*> switch
            ( long "youtube"
              <> short 'y'
              <> help "Include links to youtube videos (default is to remove)")
            <*> switch
            ( long "no-gfycat"
              <> short 'G'
              <> help "Don't translate all gifs to gfycat webm videos (default is to translate)")
            <*> switch
            ( long "quiet"
              <> short 'q'
              <> help "Don't print posts and links to the command line (use with '--open')")
            <*> many (argument str (metavar "REDDITS..."))
