module Handler.BlogReply where

import Import
import Data.Time

entryForm :: CommentId -> Form Comment
entryForm commentId = renderDivs $ Comment
    -- <$> lift (return $ Just commentId)
    <$> pure (Just commentId)
    <*> pure Nothing
    <*> areq htmlField (fieldSettingsLabel MsgContents) Nothing
    <*> lift (liftIO getCurrentTime)

getBlogReplyR :: CommentId -> Handler Html
getBlogReplyR commentId = do
    (formWidget, formEnctype) <- generateFormPost $ entryForm commentId
    let ham = [whamlet|
          <form #commentForm method=post action=@{BlogR commentId}#form enctype=#{formEnctype}>
            ^{formWidget}
            <input type="submit" value="Submit">
        |]

    {-defaultLayout $ do
      toWidget ham-}

    pc <- widgetToPageContent ham
    {-giveUrlRenderer [hamlet|
        ^{pageBody pc}
      |]-}
    giveUrlRenderer $ pageBody pc

    {-giveUrlRenderer $ do
      toWidget wid-}

    --giveUrlRenderer formWidget

    {-let handlerName = "getBlogR" :: Text
    comments <- runDB $ selectList [] [Desc CommentCreation]
    defaultLayout $ do
	setTitleI $ MsgBlog
	addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        $(widgetFile "blog")-}

