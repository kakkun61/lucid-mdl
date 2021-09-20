{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Lucid             as L
import           Lucid.Mdl.Base    (Ripple (Ripple))
import qualified Lucid.Mdl.Button as Button
import qualified Lucid.Mdl.Head as Head
import qualified Lucid.Mdl.Toggle as Toggle
import qualified Lucid.Mdl.Slider as Slider
import qualified Lucid.Mdl.Chip as Chip

main :: IO ()
main = T.putStrLn $ L.renderText html

html :: L.Html ()
html =
  L.html_ $ do
    L.head_ $ do
      Head.script_
      Head.css_ Head.Pink Head.Indigo
      Head.font_
      L.style_ "body { padding: 20px; box-sizing: border-box; }"
    L.body_ $ do
      L.h1_ "lucid-mdl"
      L.h2_ "Lucid.Mdl.Toggle"
      L.h3_ "Button"
      L.div_ $ do
        Button.button_ (Just Button.Raised) (Just Button.Colored) (Just Ripple) [] "Button"
      L.h3_ "Checkbox"
      L.div_ $ do
        Toggle.checkbox_ "checkbox1" (Just Ripple) [L.style_ "display: block;"] [] [] "Checkbox 1"
        Toggle.checkbox_ "checkbox2" (Just Ripple) [L.style_ "display: block;"] [] [] "Checkbox 2"
      L.h3_ "Radio button"
      L.div_ $ do
        Toggle.radio_ "radio1" "radio-group" (Just Ripple) [L.style_ "display: block;"] [L.checked_] [] "Radio button 1"
        Toggle.radio_ "radio2" "radio-group" (Just Ripple) [L.style_ "display: block;"] [] [] "Radio button 2"
      L.h3_ "Icon toggle"
      L.div_ $ do
        Toggle.iconToggle_ "thumb-up" (Just Ripple) [] [] [] "thumb_up"
        Toggle.iconToggle_ "thumb-down" (Just Ripple) [] [] [] "thumb_down"
      L.h3_ "Switch"
      L.div_ $ do
        Toggle.switch_ "switch1" (Just Ripple) [] [] []
      L.h3_ "Slider"
      L.div_ $ do
        L.div_ [L.style_ "display: block; width: 400px;"] $ do
          Slider.slider_ [L.min_ "0", L.max_ "100", L.value_ "0", L.tabindex_ "0"]
      L.h3_ "Chip"
      L.div_ $ do
        Chip.chip_ Nothing [L.style_ "margin-right: 8px;"] [] [] [] "Chip"
        Chip.chip_ (Just Chip.Deletable) [L.style_ "margin-right: 8px;"] [] [] [] "Deletable"
        Chip.button_ [L.style_ "margin-right: 8px;"] [] "Button"
        Chip.contact_ (Just Chip.Deletable) [] [] [] [] (L.span_ [L.classes_ ["mdl-color--teal", "mdl-color-text--white"]] "A") "Contact"
