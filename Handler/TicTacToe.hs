module Handler.TicTacToe where

import Text.Julius
import qualified Data.Text as T
import Import

title :: String
title = "Tic Tac Toe"

canvasSize :: Int
canvasSize = 600

getTicTacToeR :: Handler Html
getTicTacToeR = 
    defaultLayout $ do
	setTitle $ toHtml title
	addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        $(widgetFile "tictactoe")

