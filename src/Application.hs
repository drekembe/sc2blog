{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Lens.Template
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.Heist
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _conn :: Connection
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
