
module Handler.Blog where

import Import
import Data.Maybe (maybe)
import Data.Time
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (toStrict)
import Data.List ((\\))
--import Database.Persist.Sql.Types (SqlBackend)
import Data.Function (on)

entryForm :: CommentId -> Form Comment
entryForm commentId = renderDivs $ Comment
    <$> pure (Just commentId)
    <*> pure Nothing
    <*> areq htmlField (fieldSettingsLabel MsgContents) Nothing
    <*> lift (liftIO getCurrentTime)

data Tree a = Node a [Tree a]

instance Show Comment where
  show (Comment parent title contents creation) = unwords ("Comment":map (maybe "" T.unpack) [title,Just $ toStrict $ renderHtml $ contents,Just $ T.pack $ show creation])

instance Show a => Show (Tree a) where
  show (Node a as) = (unwords ["Node",show a]) ++ (showList as "")

--type BackedComment = KeyBackend Database.Persist.Sql.Types.SqlBackend Comment
type EntityComment = Entity Comment
--makeCommentTree :: Maybe Comment -> [Comment] -> [Tree Comment]
makeCommentTree :: Maybe EntityComment -> [EntityComment] -> [Tree EntityComment]
makeCommentTree Nothing comments =
  concat [makeCommentTree (Just comment) comments | comment <- comments, (commentParent $ entityVal comment) == Nothing]
makeCommentTree (Just parent) comments =
  let
    children = filter (\comment -> (commentParent $ entityVal comment) == (Just $ entityKey parent)) comments
    subchildren = comments --  \\ children
  in [Node parent $ concat [makeCommentTree (Just child) subchildren | child <- children]]

renderCommentForest commentTrees =
  [whamlet|
  $forall commentTree <- commentTrees
    ^{renderCommentTree commentTree}
  |]

renderCommentTree (Node a []) =
  renderComment a

renderCommentTree (Node root commentTrees) =
  [whamlet|
  <div>
    ^{renderComment root}
    <ul>
      $forall tree <- commentTrees
        <li>
          ^{renderCommentTree tree}
  |]

renderComment (Entity commentId comment) =
  [whamlet|
      <div .comment reply-url=@{BlogReplyR commentId}> 
        <span>
          _{MsgPostedOn} #{show $ commentCreation comment}:
            <div .commentArea>
              ^{commentContents comment}
        <a href=# .reply>Reply
  |]

getBlogR :: CommentId -> Handler Html
getBlogR commentId = do
    (formWidget, formEnctype) <- generateFormPost $ entryForm commentId
    let handlerName = "getBlogR" :: Text
    --comments <- runDB $ selectList [] [Desc CommentCreation]
    comments <- runDB $ selectList [] []
    --lift $ mapM_ (\(Entity i e) -> print $ i) comments
    --lift $ mapM_ print comments
    --lift $ print $ makeCommentTree Nothing comments
    let commentTrees = makeCommentTree Nothing comments
    defaultLayout $ do
	setTitleI $ MsgBlog
	addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        $(widgetFile "blog")


postBlogR :: CommentId -> Handler Html
postBlogR commentId = do
    ((result, _), _) <- runFormPost $ entryForm commentId
    runDB $ do 
      return ()
      case result of
        FormSuccess res -> insertUnique res >> return ()
        _ -> return ()
    redirect (BlogR commentId)



