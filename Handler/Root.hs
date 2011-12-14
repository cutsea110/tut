{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Import
import Data.Text as T

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "tut homepage"
        $(widgetFile "homepage")

getHomeR :: Text -> Handler RepHtml
getHomeR name = do
  let friends = T.words "Kate Michael James"
  defaultLayout $ do
    setTitle "user home"
    $(widgetFile "userhome")
