{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Lucid.MaterialDesign.Icon                    as Icon
import qualified Lucid.MaterialDesign.Lite.Base               as Base
import qualified Lucid.MaterialDesign.Lite.Button             as Button
import qualified Lucid.MaterialDesign.Lite.Card               as Card
import qualified Lucid.MaterialDesign.Lite.Chip               as Chip
import qualified Lucid.MaterialDesign.Lite.Dialog             as Dialog
import qualified Lucid.MaterialDesign.Lite.Head               as Head
import qualified Lucid.MaterialDesign.Lite.Layout.Footer      as Footer
import qualified Lucid.MaterialDesign.Lite.Layout.Footer.Mega as MegaFooter
import qualified Lucid.MaterialDesign.Lite.Layout.Footer.Mini as MiniFooter
import qualified Lucid.MaterialDesign.Lite.Layout.Grid        as Grid
import qualified Lucid.MaterialDesign.Lite.Layout.Navigation  as Navigation
import qualified Lucid.MaterialDesign.Lite.Layout.Tab         as Tab
import qualified Lucid.MaterialDesign.Lite.Slider             as Slider
import qualified Lucid.MaterialDesign.Lite.TextField          as TextField
import qualified Lucid.MaterialDesign.Lite.Toggle             as Toggle

import           Data.Default.Class (Default (def))
import           Data.String        (IsString)
import qualified Data.Text.Lazy.IO  as T
import qualified Lucid              as L

main :: IO ()
main = T.putStrLn $ L.renderText html

html :: L.Html ()
html =
  L.html_ $ do
    L.head_ $ do
      Head.script_
      Head.css_ Head.Pink Head.Indigo
      Icon.head_
    L.body_ [L.style_ "padding: 20px; box-sizing: border-box;"] $ do
      L.h1_ "lucid-mdl"

      L.h3_ "Button"
      L.div_ $ do
        Button.button_ def "Button"
        horizontalSpace
        Button.button_ def { Button.style = Just Button.Raised, Button.color = Just Button.Colored, Button.ripple = Base.Ripple True } "Button"

      L.h3_ "Checkbox"
      L.div_ $ do
        Toggle.checkbox_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "checkbox1" "Checkbox 1"
        Toggle.checkbox_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "checkbox2" "Checkbox 2"

      L.h3_ "Radio button"
      L.div_ $ do
        Toggle.radio_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"], Toggle.inputAttributes = [L.checked_] } "radio1" "radio-group" "Radio button 1"
        Toggle.radio_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "radio2" "radio-group" "Radio button 2"

      L.h3_ "Icon toggle"
      L.div_ $ do
        Toggle.iconToggle_ def { Toggle.ripple = Base.Ripple True } "thumb-up" $ Icon.icon_ "thumb_up"
        Toggle.iconToggle_ def { Toggle.ripple = Base.Ripple True } "thumb-down" $ Icon.icon_ "thumb_down"

      L.h3_ "Switch"
      L.div_ $ do
        Toggle.switch_ def { Toggle.ripple = Base.Ripple True } "switch1" ""

      L.h3_ "Slider"
      L.div_ $ do
        L.div_ [L.style_ "display: block; width: 400px;"] $ do
          Slider.slider_ def { Slider.attributes = [L.min_ "0", L.max_ "100", L.value_ "0", L.tabindex_ "0"] }

      L.h3_ "Chip"
      L.div_ $ do
        Chip.chip_ def "Chip"
        horizontalSpace
        Chip.chip_ def { Chip.deletable = Just def } "Deletable"
        horizontalSpace
        Chip.button_ def "Button"
        horizontalSpace
        Chip.textContact_ def { Chip.contactAttributes = [L.classes_ [Base.toHtmlClass Base.PrimaryColor, Base.toHtmlClass $ Base.TextColor Base.White]] } "A" "Contact"
        horizontalSpace
        Chip.imageContact_ def "https://getmdl.io/templates/dashboard/images/user.jpg" "Contact"

      L.h3_ "Text field"
      L.div_ $
        L.form_ $ do
          L.div_ $ TextField.textField_ def "text-field-1" "Text field"
          L.div_ $ TextField.textField_ def { TextField.error = Just (TextField.ErrorConfig "-?[0-9]*(\\.[0-9]+)?" [] "Input must be a number") } "text-field-2" "Text field"
          L.div_ $ TextField.textField_ def { TextField.floatingLabel = TextField.FloatingLabel True } "text-field-3" "Text field"
          L.div_ $ TextField.textArea_ def "text-area-1" "Text area" ""

      L.h3_ "Card"
      L.div_ $ do
        Card.card_ def { Card.attributes = [L.class_ "mdl-shadow--2dp"] } $ do
          Card.title_ def $ do
            Card.titleText_ $ L.h4_ "Card"
            Card.subtitleText_ $ L.h5_ "Material"
          Card.supportingText_ def loremIpsum
          Card.actions_ def { Card.border = Card.Border True } $ Button.button_ def { Button.color = Just Button.Colored } "Button"
          Card.menu_ def $ Button.button_ def { Button.style = Just Button.Icon } $ Icon.icon_ "share"

      L.h3_ "Dialog"
      L.div_ $ do
        Button.button_ def { Button.attributes = [L.id_ "button-open-dialog"] } "Open dialog"
        Dialog.dialog_ def { Dialog.attributes = [L.id_ "dialog"] } $ do
          Dialog.title_ $ L.h4_ "Dialog"
          Dialog.content_ def $ L.p_ loremIpsum
          Dialog.actions_ def $ do
            Button.button_ def { Button.attributes = [L.id_ "button-close-dialog"] } "Close dialog"
        L.script_
          "var dialog = document.getElementById('dialog');\
          \var showDialogButton = document.getElementById('button-open-dialog');\
          \if (! dialog.showModal) {\
          \  dialogPolyfill.registerDialog(dialog);\
          \}\
          \showDialogButton.addEventListener('click', function() {\
          \  dialog.showModal();\
          \});\
          \document.getElementById('button-close-dialog').addEventListener('click', function() {\
          \  dialog.close();\
          \});"

      L.h3_ "Navigation"
      L.div_ $ do
        L.div_ [boxStyle] $ do
          Navigation.layout_ def { Navigation.attributes = [L.style_ "background: url('https://getmdl.io//assets/demos/transparent.jpg') center / cover;"] } $ do
            let colorWhite = L.style_ "color: white;"
            L.style_ ".mdl-layout__drawer-button { color: white; }"
            Navigation.header_ def { Navigation.attributes = [colorWhite], Navigation.transparent = Navigation.HeaderTransparent True } $ do
              Navigation.headerRow_ def $ do
                Navigation.title_ def "Title"
                Navigation.spacer_
                Navigation.navigation_ def $ do
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 1"
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 3"
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 2"
            Navigation.drawer_ def $ do
              Navigation.title_ def "Title"
              Navigation.navigation_ def $ do
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 1"
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 3"
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 2"
            Navigation.content_ def ""

        verticalSpace

        L.div_ [boxStyle] $ do
          Navigation.layout_ def { Navigation.fixedDrawer = Navigation.FixedDrawer True, Navigation.fixedHeader = Navigation.FixedHeader True } $ do
            Navigation.header_ def $ do
              Navigation.headerRow_ def $ do
                Navigation.title_ def "Title"
                Navigation.spacer_
                Navigation.navigation_ def $ do
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 1"
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 3"
                  Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 2"
            Navigation.drawer_ def $ do
              Navigation.title_ def "Title"
              Navigation.navigation_ def $ do
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 1"
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 3"
                Navigation.navigationLink_ def { Navigation.attributes = [L.href_ ""] } "Link 2"
            Navigation.content_ def ""

        verticalSpace

        L.div_ [boxStyle] $ do
          Navigation.layout_ def $ do
            Navigation.header_ def $ do
              Navigation.headerRow_ def $ do
                Navigation.title_ def "Title"
              Navigation.tabBar_ def { Navigation.ripple = Base.Ripple True } $ do
                Navigation.tab_ def { Navigation.attributes = [L.href_ "#layout-tab-1"], Navigation.active = Base.Active True } "Tab 1"
                Navigation.tab_ def { Navigation.attributes = [L.href_ "#layout-tab-2"] } "Tab 2"
            Navigation.content_ def $ do
              Navigation.tabPanel_ def { Navigation.attributes = [L.id_ "layout-tab-1"], Navigation.active = Base.Active True } $ do
                L.p_ "This is a content of the tab 1"
              Navigation.tabPanel_ def { Navigation.attributes = [L.id_ "layout-tab-2"] } $ do
                L.p_ "This is a content of the tab 2"

      L.h3_ "Grid"
      L.div_ $ do
        L.style_
          " .demo-grid .mdl-cell {\
          \   box-sizing: border-box;\
          \   background-color: #BDBDBD;\
          \   height: 200px;\
          \   padding-left: 8px;\
          \   padding-top: 4px;\
          \   color: white;\
          \ }\
          \ .demo-grid .mdl-grid:first-of-type .mdl-cell {\
          \   height: 50px;\
          \ }"
        L.div_ [L.class_ "demo-grid"] $ do
          Grid.grid_ def $ do
            let config = def { Grid.column = Just 1 }
            Grid.cell_ config "a"
            Grid.cell_ config "b"
            Grid.cell_ config "c"
            Grid.cell_ config "d"
          Grid.grid_ def $ do
            Grid.cell_ def "a"
            Grid.cell_ def "b"

      L.h3_ "Tab"
      L.div_ $ do
        L.div_ [boxStyle] $ do
          Tab.tabs_ def $ do
            Tab.tabBar_ def $ do
              Tab.tab_ def { Tab.attributes = [L.href_ "#tab-1"], Tab.active = Base.Active True } "Tab 1"
              Tab.tab_ def { Tab.attributes = [L.href_ "#tab-2"] } "Tab 1"
              Tab.tab_ def { Tab.attributes = [L.href_ "#tab-3"] } "Tab 1"
            Tab.panel_ def { Tab.attributes = [L.id_ "tab-1"], Tab.active = Base.Active True } $
              L.p_ "This is a content of the tab 1"
            Tab.panel_ def { Tab.attributes = [L.id_ "tab-2"] } $
              L.p_ "This is a content of the tab 2"
            Tab.panel_ def { Tab.attributes = [L.id_ "tab-3"] } $
              L.p_ "This is a content of the tab 3"

      L.h3_ "Mega footer"
      L.div_ $ do
        MegaFooter.footer_ def $ do
          MegaFooter.topSection_ def $ do
            MegaFooter.leftSection_ def $ do
              MegaFooter.socialButton_ def ""
              MegaFooter.socialButton_ def ""
              MegaFooter.socialButton_ def ""
            MegaFooter.rightSection_ def $ do
              L.a_ [L.href_ ""] "Introduction"
              L.a_ [L.href_ ""] "App Status Dashboard"
              L.a_ [L.href_ ""] "Terms of Service"
          MegaFooter.middleSection_ def $ do
            MegaFooter.dropDownSection_ def $ do
              MegaFooter.heading_ def "Learning and Support"
              MegaFooter.linkList_ def $ do
                L.li_ $ L.a_ [L.href_ ""] "Resource Center"
                L.li_ $ L.a_ [L.href_ ""] "Help Center"
                L.li_ $ L.a_ [L.href_ ""] "Community"
                L.li_ $ L.a_ [L.href_ ""] "Learn with Google"
                L.li_ $ L.a_ [L.href_ ""] "Small Business Community"
                L.li_ $ L.a_ [L.href_ ""] "Think Insights"
            MegaFooter.dropDownSection_ def $ do
              MegaFooter.heading_ def "Just for Developers"
              MegaFooter.linkList_ def $ do
                L.li_ $ L.a_ [L.href_ ""] "Google Developers"
                L.li_ $ L.a_ [L.href_ ""] "AdWords API"
                L.li_ $ L.a_ [L.href_ ""] "AdWords Scripts"
                L.li_ $ L.a_ [L.href_ ""] "AdWords Remarketing Tag"
          MegaFooter.bottomSection_ def $ do
            Footer.logo_ def "More Information"
            MegaFooter.linkList_ def $ do
              L.li_ $ L.a_ [L.href_ ""] "Help"
              L.li_ $ L.a_ [L.href_ ""] "Privacy and Terms"

      L.h3_ "Mini footer"
      L.div_ $ do
        MiniFooter.footer_ def $ do
          MiniFooter.leftSection_ def $ do
            Footer.logo_ def "More Information"
            MiniFooter.linkList_ def $ do
              L.li_ $ L.a_ [L.href_ ""] "Help"
              L.li_ $ L.a_ [L.href_ ""] "Privacy and Terms"
              L.li_ $ L.a_ [L.href_ ""] "User Agreement"
          MiniFooter.rightSection_ def $ do
            MiniFooter.socialButton_ def ""
            MiniFooter.socialButton_ def ""
            MiniFooter.socialButton_ def ""

horizontalSpace :: L.Html ()
horizontalSpace = L.span_ [L.style_ "display: inline-block; width: 8px;"] ""

verticalSpace :: L.Html ()
verticalSpace = L.div_ [L.style_ "height: 8px;"] ""

boxStyle :: L.Attribute
boxStyle = L.style_ "width: 100%; max-width: 900px; height: 300px; position: relative;"

loremIpsum :: IsString a => a
loremIpsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation \
  \ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
  \occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
