module Handler.Shit where

import Data.Time
import Import
import qualified Data.Text as T

title :: String
title = "Shit People Have Posted"

entryForm :: Form Post
entryForm = renderDivs $ Post
    <$> areq   textField "Author" Nothing
    <*> areq   textareaField "Contents" Nothing
    <*> lift (liftIO getCurrentTime)

getShitR :: Handler Html
getShitR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    let handlerName = "getShitR" :: Text
    shits <- runDB $ selectList [] [Desc PostCreation]
    defaultLayout $ do
	setTitle $ toHtml title
        $(widgetFile "shit")


postShitR :: Handler Html
postShitR = do
    ((result, formWidget), formEnctype) <- runFormPost entryForm
    runDB $ do 
      return ()
      case result of
        FormSuccess res -> insert res >> return ()
        _ -> return ()
    redirect ShitR



    {-let handlerName = "postShitR" :: Text

    shits <- queryDB
    defaultLayout $ do
        aDomId <- newIdent
	title
        $(widgetFile "shit")-}


