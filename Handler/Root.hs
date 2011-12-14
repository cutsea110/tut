{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Import
import Data.Text (pack)

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

getHomeR :: UserId -> Handler RepHtml
getHomeR uid = do
  (self, friends) <- runDB $ do
    me <- get404 uid
    fs <- selectList [] [Asc UserIdent]
    return (me, fs)
  let sexs = [minBound..maxBound]
      sexIs s = s == userSex self
      toInt = (+1).fromEnum
  defaultLayout $ do
    setTitle "user home"
    $(widgetFile "userhome")

postHomeR :: UserId -> Handler RepHtml
postHomeR uid = do
  (sex,age,memo) <- runInputPost $ (,,)
                    <$> ireq (selectField sexs) "sex"
                    <*> ireq intField "age"
                    <*> iopt textField "memo"
  runDB $ do
    update uid [ UserSex =. sex
               , UserAge =. age
               , UserMemo =. maybe "" id memo
               ]
  redirect RedirectTemporary $ HomeR uid
  where
    sexs = [(pack $ show s, s) | s <- [minBound..maxBound]]
