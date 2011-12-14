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
  defaultLayout $ do
    setTitle "user home"
    $(widgetFile "userhome")

postHomeR :: UserId -> Handler RepHtml
postHomeR uid = do
  redirect RedirectTemporary $ HomeR uid

data Prof = Prof
            { profSex :: Sex
            , profAge :: Int
            , profMemo :: Maybe Textarea
            }
          deriving Show

profAForm :: Maybe Prof -> AForm Tut Tut Prof
profAForm mp = Prof
            <$> areq (selectField sexs) "Sex" (profSex <$> mp)
            <*> areq intField "Age" (profAge <$> mp)
            <*> aopt textareaField "Memo" (profMemo <$> mp)
  where
    sexs = [(pack $ show s, s) | s <- [minBound::Sex .. maxBound]]
