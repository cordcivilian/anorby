{-# LANGUAGE OverloadedStrings #-}

module Web.Styles where

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

combineCSS :: [T.Text] -> T.Text
combineCSS = T.concat

cssEntry :: T.Text -> [T.Text] -> T.Text
cssEntry selector properties = T.unlines
  [ selector <> " {"
  , T.intercalate "\n" (map (\p -> "    " <> p <> ";") properties)
  , "}"
  ]

inlineCSSEntry :: [T.Text] -> T.Text
inlineCSSEntry properties = T.intercalate "; " properties

cssProperty :: T.Text -> T.Text -> T.Text
cssProperty property value = T.intercalate ": " [property, value]

aorbDynamicCSS :: [(String, Int)] -> H.AttributeValue
aorbDynamicCSS orderPairs =
  H.preEscapedTextValue $ inlineCSSEntry
    [ "--order-" <> T.pack name <> ": " <> T.pack (show order)
    | (name, order) <- orderPairs ]

byDiceTargetCSS :: T.Text
byDiceTargetCSS = cssEntry "#by-dice:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-dice) !important"
  ]

byPolarTargetCSS :: T.Text
byPolarTargetCSS = cssEntry "#by-polar:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-polar) !important"
  ]

bySidedTargetCSS :: T.Text
bySidedTargetCSS = cssEntry "#by-sided:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-sided) !important"
  ]

aorbDisplayCSS :: T.Text
aorbDisplayCSS = combineCSS
  [ cssEntry ".aorb"
    [ cssProperty "order" "var(--order-dice)"
    ]
  ]

byBasicTargetCSS :: T.Text
byBasicTargetCSS =
  cssEntry "#by-basic:target ~ #aorbs-container .aorb-clickable"
  [ cssProperty "order" "var(--order-basic) !important"
  ]

byFlakeTargetCSS :: T.Text
byFlakeTargetCSS =
  cssEntry "#by-flake:target ~ #aorbs-container .aorb-clickable"
  [ cssProperty "order" "var(--order-flake) !important"
  ]

clickableAorbCSS :: T.Text
clickableAorbCSS = combineCSS
  [ cssEntry ".aorb-clickable"
    [ cssProperty "order" "var(--order-flake)"
    ]
  ]
