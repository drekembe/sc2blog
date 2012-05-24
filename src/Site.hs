{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Arrow hiding (app)
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Monad
import           Data.Lens.Common
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Markdown
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Db (Post, Player, Game, Outcome, Race)
import qualified Db as DB


-- Some convenience functions
showAsText :: Show a => a -> Text
showAsText = T.pack . show

bs2integer :: ByteString -> Integer
bs2integer = read . B.unpack

bs2text :: ByteString -> Text
bs2text = T.decodeUtf8

bs2race :: ByteString -> Race
bs2race x = toEnum . read . B.unpack $ x


------------------------------------------------------------------------------
-- | Current time splice
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return [TextNode $ T.pack $ show $ time]

------------------------------------------------------------------------------
-- | Takes a player and returns a splice with his info
mkPlayerSplice :: Player -> Splice AppHandler
mkPlayerSplice p = runChildrenWithText
    [("name", DB.name p)
    ,("race", showAsText $ DB.race p)
    ,("r", T.take 1 . showAsText $ DB.race p)
    ,("rating", showAsText $ DB.rating p)
    ]

------------------------------------------------------------------------------
-- | A splice that gets a player based on the name attribute
playerByNameSplice :: Splice AppHandler
playerByNameSplice = do
    p <- getParamNode
    let (Just name) = getAttribute "name" p
    conn <- lift $ gets _conn
    maybePlayer <- liftIO $ DB.playerByName conn name
    case maybePlayer of Nothing -> return [TextNode "Not found"]
                        (Just player) -> mkPlayerSplice player

------------------------------------------------------------------------------
-- | Splice to display all players
playersSplice :: Splice AppHandler
playersSplice = do
    conn <- lift $ gets _conn
    players <- liftIO $ DB.allPlayers conn
    mapSplices mkPlayerSplice players

------------------------------------------------------------------------------
-- | Takes a post and returns a splice with its contents
mkPostSplice :: Post -> Splice AppHandler
mkPostSplice p = runChildrenWithText
    [("posttitle", DB.title p)
    ,("postbody", DB.body p)
    ,("timeposted", showAsText $ DB.timePosted p)
    ]

------------------------------------------------------------------------------
-- | A splice to display all posts
postsSplice :: Splice AppHandler
postsSplice = do
    conn <- lift $ gets _conn
    posts <- liftIO $ DB.allPosts conn
    mapSplices mkPostSplice posts

------------------------------------------------------------------------------
-- | A splice that looks up the contents of the snippet specified by the
-- 'name' attribute
snippetSplice :: Splice AppHandler
snippetSplice = do
    node <- getParamNode
    let (Just name) = getAttribute "name" node
    conn <- lift $ gets _conn
    maybebody <- liftIO $ DB.snippet conn name
    case maybebody of
        Nothing -> do
            logError $ T.encodeUtf8 $ "snippet for " `mappend` name `mappend` " not found"
            return []
        (Just body) -> return [TextNode body]

------------------------------------------------------------------------------
-- | Takes a 'Game' and returns a splice with its info
mkGameSplice :: Game -> Splice AppHandler
mkGameSplice game = runChildrenWith
    [("id", textSplice $ maybe "invalid" showAsText $ DB.gameID game)
    ,("player1", mkPlayerSplice $ DB.player1 game)
    ,("player2", mkPlayerSplice $ DB.player2 game)
    ,("winner", mkPlayerSplice $ DB.winner game)
    ,("timeplayed", textSplice $ showAsText $ DB.timePlayed game)
    ,("videourl", textSplice $ bs2text $ DB.videoUrl game)
    ]

------------------------------------------------------------------------------
-- | A splice to display all the games
gamesSplice :: Splice AppHandler
gamesSplice = do
    conn <- lift $ gets _conn
    games <- liftIO $ DB.games conn
    mapSplices mkGameSplice games


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
index :: Handler App App ()
index = ifTop $ render "index"

players :: Handler App App ()
players = render "players"

games :: Handler App App ()
games = render "games"

------------------------------------------------------------------------------
-- | Displays a single player
player :: Handler App App ()
player = do
    (Just name) <- getParam "name"
    conn <- gets _conn
    mplayer <- liftIO $ DB.playerByName conn (bs2text name)
    case mplayer of
        Nothing -> the404
        (Just player) -> do
            games <- liftIO $ DB.gamesByPlayer conn player
            let playerSplice = mkPlayerSplice player
            let gamesSplice = mapSplices mkGameSplice games
            heistLocal (bindSplice "player" playerSplice
                       .bindSplice "games" gamesSplice
                       ) $ render "player"

------------------------------------------------------------------------------
-- | gets the POST params and then stores a player into the db
putPlayer :: Handler App App ()
putPlayer = do
    mname <- getPostParam "name"
    mrace <- getPostParam "race"
    mrating <- getPostParam "rating"
    case sequence [mname, mrace, mrating] of
        Nothing -> writeBS "error"
        (Just [name, race, rating])-> do
            conn <- gets _conn
            let player = DB.Player Nothing
                                   (bs2text name)
                                   (bs2race race)
                                   (bs2integer rating)
            liftIO $ DB.putPlayer conn player
            writeBS "OK"


------------------------------------------------------------------------------
-- | Displays a single game
game :: Handler App App ()
game = do
    (Just gameID) <- getParam "id"
    conn <- gets _conn
    mgame <- liftIO $ DB.game conn (read $ B.unpack gameID)
    case mgame of
        Nothing -> the404
        (Just game) -> do
            let splice = mkGameSplice game
            heistLocal (bindSplice "game" splice) $ render "game"

about :: Handler App App ()
about = render "about"

------------------------------------------------------------------------------
-- | A 404 handler, sets the response status to 404 and then renders 404.tpl
the404 :: Handler App App ()
the404 = do
    modifyResponse $ setResponseStatus 404 "Not found"
    render "404"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", index)
         , ("/players", method GET players <|> method POST putPlayer)
         , ("/players/:name", player)
         , ("/about", about)
         , ("/games", games)
         , ("/games/:id", game)
         , ("/empty", render "empty")
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    conn <- liftIO $ connectSqlite3 "data.db"
    addSplices $ map (second liftHeist)
        [ ("posts", postsSplice)
        , ("players", playersSplice)
        , ("player", playerByNameSplice)
        , ("snippet", snippetSplice)
        , ("games", gamesSplice)
        , ("currenttime", currentTimeSplice)
        ]
    addRoutes routes
    wrapHandlers (<|> the404)
    return $ App h conn
