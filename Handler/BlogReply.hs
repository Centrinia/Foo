module Handler.BlogReply where

import Import
import Data.Time
import Yesod.Auth

entryForm :: Maybe User -> Maybe CommentId -> Form Comment
entryForm user maybeCommentId = renderDivs $ Comment
    <$> pure maybeCommentId -- Parent
    <*> aopt textField (fieldSettingsLabel MsgTitle) Nothing
    <*> pure user -- User
    <*> areq htmlField (fieldSettingsLabel MsgContents) Nothing
    <*> lift (liftIO getCurrentTime)

getBlogReplyR :: Maybe CommentId -> Handler Html
getBlogReplyR maybeCommentId = do
    authId <- maybeAuthId
    user <- case authId of
      Just authId' -> runDB $ get authId'
      Nothing -> return Nothing

    (formWidget, formEnctype) <- generateFormPost $ entryForm user maybeCommentId
    let ham = [whamlet|
          <form #commentForm method=post action=@{BlogR maybeCommentId}#form enctype=#{formEnctype}>
            ^{formWidget}
            <input type="submit" value="Submit">
        |]

    pc <- widgetToPageContent ham

    giveUrlRenderer $ pageBody pc

