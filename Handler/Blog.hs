
module Handler.Blog where

import Import
import Data.Maybe (maybe,isJust)
import Data.Time
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.List ((\\))
--import Database.Persist.Sql.Types (SqlBackend)
import Data.Function (on)
import Yesod.Auth
import System.Locale (defaultTimeLocale)

entryForm :: Maybe User -> Maybe CommentId -> Form Comment
entryForm user maybeCommentId = renderDivs $ Comment
    <$> pure maybeCommentId
    <*> aopt textField (fieldSettingsLabel MsgTitle) Nothing
    <*> pure user
    <*> areq htmlField (fieldSettingsLabel MsgContents) Nothing
    <*> lift (liftIO getCurrentTime)

data Tree a = Node a [Tree a]

instance Show Comment where
  show (Comment parent title user contents creation) = unwords ("Comment":map (maybe "" T.unpack) [Just $ T.pack $ show $ user,title {-,Just $ toStrict $ renderHtml $ contents -} ,Just $ T.pack $ show creation])

{-instance Show a => Show (Tree a) where
  show (Node a as) = (unwords ["Node",show a]) ++ (showList as "")
-}

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

renderCommentForest :: [Tree EntityComment] -> Widget
renderCommentForest commentTrees =
  [whamlet|
  <ul>
    $forall commentTree <- commentTrees
      <li .comment>
        ^{renderCommentTree commentTree}
  |]

renderCommentTree :: Tree EntityComment -> Widget
renderCommentTree (Node a []) =
  renderComment a

renderCommentTree (Node root commentTrees) =
  [whamlet|
  ^{renderComment root}
  <ul>
    $forall tree <- commentTrees
      <li .comment>
        ^{renderCommentTree tree}
  |]

renderComment :: EntityComment -> Widget
renderComment (Entity commentId comment) =
  $(widgetFile "blog-comment")

getBlogR :: Maybe CommentId -> Handler Html
getBlogR maybeCommentId = do
    authId <- maybeAuthId
    user <- case authId of
      Just authId' -> runDB $ get authId'
      Nothing -> return Nothing
    --(formWidget, formEnctype)
    formItems <- case user of
      Just user' -> (generateFormPost $ entryForm (Just user') Nothing) >>= (return . Just)
      Nothing -> return $ Nothing

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

showTime = formatTime defaultTimeLocale "%B %e, %Y %r %Z"

postBlogR :: Maybe CommentId -> Handler Html
postBlogR maybeCommentId = do
    authId <- maybeAuthId
    user <- case authId of
      Just authId' -> runDB $ get authId'
      Nothing -> return Nothing

    ((result,_),_) <- runFormPost $ entryForm user maybeCommentId

    case result of
      FormSuccess res -> runDB $ insert res  >> return ()
      _ -> return ()

    redirect (BlogR maybeCommentId)



