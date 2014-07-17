module Handler.Ring where

import Data.Time
import Import
import qualified Data.Text as T
import qualified System.Random as RND
import qualified Control.Monad as M
title :: String
title = "Web Ring"

entryForm :: Form Site
entryForm = renderDivs $ Site
    <$> areq textField "Owner" Nothing
    <*> areq textField "Url" Nothing

getRingR :: Handler Html
getRingR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    let handlerName = "getRingR" :: Text
    sites <- runDB $ selectList [] [Asc SiteOwner]
    rnds <- liftIO $ M.replicateM (length sites) (RND.randomRIO (0,3) :: IO Int)
    defaultLayout $ do
	setTitle $ toHtml title
        $(widgetFile "ring")

postRingR :: Handler Html
postRingR = do
    ((result, formWidget), formEnctype) <- runFormPost entryForm
    runDB $ do 
      return ()
      case result of
        FormSuccess res -> insert res >> return ()
        _ -> return ()
    redirect RingR

