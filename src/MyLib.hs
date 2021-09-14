{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Text.HTML.Scalpel
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
allComments = scrapeURL "http://example.com/article.html" comments -- TODO: Actual URL: https://www.game.es/ps5-playstation5-reserva
   where
       comments :: Scraper String [Comment]
       comments = chroots ("div" @: [hasClass "container"]) comment

       comment :: Scraper String Comment
       comment = textComment <|> imageComment

       textComment :: Scraper String Comment
       textComment = do
           author      <- text $ "span" @: [hasClass "author"]
           commentText <- text $ "div"  @: [hasClass "text"]
           return $ TextComment author commentText

       imageComment :: Scraper String Comment
       imageComment = do
           author   <- text       $ "span" @: [hasClass "author"]
           imageURL <- attr "src" $ "img"  @: [hasClass "image"]
           return $ ImageComment author imageURL

exampleHtml :: String
exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>Sally</span>\
\                <div class='comment text'>Woo hoo!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"