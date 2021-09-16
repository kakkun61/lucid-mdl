{-# LANGUAGE OverloadedStrings #-}

import Lucid.Mdl.Base
import Lucid.Mdl.Toggle
import qualified Lucid as L
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn $ L.renderText html

html :: L.Html ()
html =
  L.html_ $ do
    L.head_ $ do
      L.script_ [L.src_ "https://code.getmdl.io/1.3.0/material.min.js"] ("" :: L.Html ())
      L.link_ [L.rel_ "stylesheet", L.href_ "https://code.getmdl.io/1.3.0/material.pink-indigo.min.css"]
      L.link_ [L.rel_ "stylesheet", L.href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]
      L.style_ "body { padding: 20px; box-sizing: border-box; }"
    L.body_ $ do
      L.h1_ "lucid-mdl"
      L.h2_ "Lucid.Mdl.Toggle"
      L.h3_ "Checkbox"
      L.div_ $ do
        checkbox_ "checkbox1" (Just Ripple) [L.style_ "display: block;"] [] [] "Checkbox 1"
        checkbox_ "checkbox2" (Just Ripple) [L.style_ "display: block;"] [] [] "Checkbox 2"
      L.h3_ "Radio button"
      L.div_ $ do
        radio_ "radio1" "radio-group" (Just Ripple) [L.style_ "display: block;"] [L.checked_] [] "Radio button 1"
        radio_ "radio2" "radio-group" (Just Ripple) [L.style_ "display: block;"] [] [] "Radio button 2"
      L.h3_ "Icon toggle"
      L.div_ $ do
        iconToggle_ "thumb-up" (Just Ripple) [] [] [] "thumb_up"
        iconToggle_ "thumb-down" (Just Ripple) [] [] [] "thumb_down"
      L.h3_ "Switch"
      L.div_ $ do
        switch_ "switch1" (Just Ripple) [] [] []
