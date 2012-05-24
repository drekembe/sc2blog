{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Db where

import Control.Applicative
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time.Clock
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import System.IO

data Post = Post
    { postID :: Maybe Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , timePosted :: UTCTime
    } deriving (Eq, Show, Read)

data Player = Player
    { playerID :: Maybe Integer
    , name :: Text
    , race :: Race
    , rating :: Integer
    } deriving (Eq, Show, Read)

data Race = Terran | Protoss | Zerg
    deriving (Eq, Show, Read, Enum, Ord)

data Outcome = NotPlayed | Win1 | Win2 deriving (Eq, Show, Read)

data Game = Game
    { gameID :: Maybe Integer
    , player1 :: Player
    , player2 :: Player
    , winner :: Player
    , timePlayed :: UTCTime
    , videoUrl :: ByteString
    } deriving (Eq, Show, Read)

rowToPlayer :: [SqlValue] -> Player
rowToPlayer (id:name:race:rating:_) =
    Player { playerID = Just $ fromSql id
           , name = fromSql name
           , race = toEnum $ (fromSql race :: Int)
           , rating = fromSql rating
           }

playerByName :: Connection -> Text -> IO (Maybe Player)
playerByName conn name = do
    let query = "SELECT id, name, race, rating FROM" ++
                " player WHERE lower(player.name)=?"
    listToMaybe . map rowToPlayer <$>
        quickQuery' conn query [toSql . T.toLower $ name]

allPlayers :: Connection -> IO [Player]
allPlayers conn = do
    let query = "SELECT id, name, race, rating FROM" ++
                " player ORDER BY rating DESC"
    map rowToPlayer <$> quickQuery' conn query []

putPlayer :: Connection -> Player -> IO ()
putPlayer conn player = do
    let query = "INSERT INTO player(name, race, rating)" ++
                "VALUES(?,?,?)"
    quickQuery' conn query
        [toSql $ name player
        ,toSql $ fromEnum $ race player
        ,toSql $ rating player
        ]
    commit conn
    return ()

rowToPost :: [SqlValue] -> Post
rowToPost (id:title:body:slug:timeposted:_) =
    Post { postID = Just $ fromSql id
         , title = fromSql title
         , body = fromSql body
         , slug = fromSql slug
         , timePosted = fromSql timeposted
         }

post :: Connection -> Text -> IO (Maybe Post)
post conn slug = do
    let query = "SELECT id, title, body, slug, timeposted" ++
                " FROM post WHERE post.slug=?"
    listToMaybe . map rowToPost <$>
        quickQuery' conn query [toSql slug]

allPosts :: Connection -> IO [Post]
allPosts conn = do
    let query = "SELECT id, title, body, slug, timeposted" ++
                " FROM post ORDER BY timeposted DESC"
    map rowToPost <$> quickQuery' conn query []

snippet :: Connection -> Text -> IO (Maybe Text)
snippet conn name = do
    let query = "SELECT snippettext FROM snippet" ++
                " WHERE snippet.snippetname=?"
    listToMaybe . map rowToSnippet <$> quickQuery' conn query [toSql name]
    where rowToSnippet (snippetbody:_) = fromSql snippetbody


rowToGame :: [SqlValue] -> Game
rowToGame row = let
    (rowToPlayer -> p1, row') = splitAt 4 row
    (rowToPlayer -> p2, row'') = splitAt 4 row'
    (id:_:_:outcome:time:url:_) = row''
    in Game { gameID = Just $ fromSql id
            , player1 = p1
            , player2 = p2
            , winner = if fromSql outcome == (1::Int) then p1 else p2
            , timePlayed = fromSql time
            , videoUrl = fromSql url
            }


games :: Connection -> IO [Game]
games conn = do
    let query = "select * from player as player1, player as player2, game" ++
                " where game.player1 = player1.id and game.player2 = player2.id"
    map rowToGame <$> quickQuery' conn query []

game :: Connection -> Integer -> IO (Maybe Game)
game conn id = do
    let query = "select * from player as player1, player as player2, game" ++
                " where game.player1 = player1.id and game.player2 = player2.id" ++
                " and game.id=?"
    listToMaybe . map rowToGame <$> quickQuery' conn query [toSql id]

gamesByPlayer :: Connection -> Player -> IO [Game]
gamesByPlayer conn player = do
    let query = "select * from player as player1, player as player2, game" ++
                " where game.player1 = player1.id and game.player2 = player2.id" ++
                " and (player1.id=? or player2.id=?)"
    case playerID player of
        Nothing -> return []
        (Just theid) -> map rowToGame <$> quickQuery' conn query [toSql theid, toSql theid]
