module Handler.Hypercube where

import Import

canvasSize :: Int
canvasSize = 1000

getHypercubeR :: Handler Html
getHypercubeR = 
  let
    choices = [3..11] :: [Int]
  in defaultLayout $ do
	setTitleI MsgHypercube
	addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        $(widgetFile "hypercube")
