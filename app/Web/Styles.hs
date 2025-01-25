{-# LANGUAGE OverloadedStrings #-}

module Web.Styles where

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

-- | CSS Helper Functions

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

cssMediaQuery :: T.Text -> [T.Text] -> T.Text
cssMediaQuery query rules = T.unlines
  [ "@media " <> query <> " {"
  , T.unlines $ map ("  " <>) rules
  , "}"
  ]

cssMaxWidth :: Int -> [T.Text] -> T.Text
cssMaxWidth width = cssMediaQuery
  ("only screen and (max-width: " <> T.pack (show width) <> "px)")

-- | Core CSS

baseCSS :: T.Text
baseCSS = combineCSS
  [ rootCSS
  , bodyHtmlCSS
  , frameCSS
  , linkCSS
  , hrCSS
  , scrollbarCSS
  ]

rootCSS :: T.Text
rootCSS = cssEntry ":root"
  [ cssProperty "color-scheme" "light dark"
  ]

bodyHtmlCSS :: T.Text
bodyHtmlCSS = cssEntry "body, html"
  [ cssProperty "margin" "0 auto"
  , cssProperty "font-family" "'Lucida Console', monospace"
  , cssProperty "font-size" "20px"
  , cssProperty "max-width" "1280px"
  , cssProperty "width" "95vw"
  ]

frameCSS :: T.Text
frameCSS = cssEntry ".frame"
  [ cssProperty "min-height" "100dvh"
  , cssProperty "text-align" "center"
  , cssProperty "align-content" "center"
  , cssProperty "border" "3px"
  , cssProperty "box-sizing" "border-box"
  , cssProperty "place-items" "center"
  , cssProperty "width" "100%"
  ]

linkCSS :: T.Text
linkCSS = cssEntry "a"
  [ cssProperty "text-decoration" "none"
  , cssProperty "color" "#4169e1"
  ]

hrCSS :: T.Text
hrCSS = cssEntry "hr"
  [ cssProperty "border" "none"
  , cssProperty "height" "2px"
  , cssProperty "background-color" "lightgrey"
  , cssProperty "margin" "30px auto"
  ]

scrollbarCSS :: T.Text
scrollbarCSS = cssEntry "html"
  [ cssProperty "scrollbar-gutter" "stable"
  ]

-- | Navigation CSS

navBarCSS :: T.Text
navBarCSS = T.unlines
  [ cssEntry ".nav-bar"
    [ cssProperty "display" "flex"
    , cssProperty "justify-content" "center"
    , cssProperty "gap" "0.75rem"
    , cssProperty "margin-bottom" "1rem"
    , cssProperty "flex-wrap" "wrap"
    , cssProperty "align-items" "center"
    ]
  , cssEntry ".nav-bar-row"
    [ cssProperty "display" "flex"
    , cssProperty "gap" "0.75rem"
    , cssProperty "align-items" "center"
    ]
  , cssEntry ".nav-separator"
    [ cssProperty "color" "gray"
    ]
  , cssEntry ".nav-link.active"
    [ cssProperty "color" "orange"
    ]
  , cssMaxWidth 350
    [ cssEntry ".nav-bar"
        [ cssProperty "flex-direction" "column"
        ]
    , cssEntry ".nav-bar-row"
        [ cssProperty "flex-direction" "column"
        ]
    , cssEntry ".nav-separator"
        [ cssProperty "display" "none"
        ]
    ]
  ]

-- | Page Specific CSS

rootPageCSS :: T.Text
rootPageCSS = combineCSS
  [ baseCSS
  , underlineCSS
  , navBarCSS
  , sorterCSS
  , sortByCSS
  , byDiceTargetCSS
  , byPolarTargetCSS
  , bySidedTargetCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , notchCSS
  ]

underlineCSS :: T.Text
underlineCSS = cssEntry "span.underline"
  [ cssProperty "border-bottom" "6px solid black"
  , cssProperty "display" "inline-block"
  , cssProperty "line-height" "0.85"
  , cssProperty "padding" "0 1px"
  , cssProperty "margin" "0 1px -3px"
  ]

roadmapCSS :: T.Text
roadmapCSS = combineCSS
  [ cssEntry ".milestone-incomplete"
    [ cssProperty "position" "relative"
    , cssProperty "height" "60px"
    , cssProperty "background-color" "#f5f5f5"
    , cssProperty "border-radius" "30px"
    , cssProperty "margin" "2rem auto"
    , cssProperty "max-width" "800px"
    , cssProperty "overflow" "hidden"
    ]
  , cssEntry ".milestone-complete"
    [ cssProperty "position" "relative"
    , cssProperty "height" "60px"
    , cssProperty "background-color" "#f5f5f5"
    , cssProperty "border-radius" "30px"
    , cssProperty "margin" "2rem auto"
    , cssProperty "max-width" "800px"
    , cssProperty "overflow" "hidden"
    , cssProperty "border" "2px solid #4169e1"
    ]
  , cssEntry ".progress-bar"
    [ cssProperty "position" "absolute"
    , cssProperty "top" "0"
    , cssProperty "left" "0"
    , cssProperty "height" "100%"
    , cssProperty "background-color" "rgba(65, 105, 225, 0.2)"
    , cssProperty "transition" "width 0.5s ease-in-out"
    ]
  , cssEntry ".milestone-marker"
    [ cssProperty "position" "absolute"
    , cssProperty "top" "50%"
    , cssProperty "left" "50%"
    , cssProperty "transform" "translate(-50%, -50%)"
    , cssProperty "font-size" "1.5rem"
    , cssProperty "font-weight" "bold"
    , cssProperty "color" "#4169e1"
    ]
  , cssEntry ".next-milestone"
    [ cssProperty "text-align" "center"
    , cssProperty "margin-top" "2rem"
    ]
  , cssMediaQuery "(prefers-color-scheme: dark)"
    [ cssEntry ".milestone-incomplete"
      [ cssProperty "background-color" "#2a2a2a"
      ]
    ]
  ]

sorterCSS :: T.Text
sorterCSS = cssEntry "#sorter"
  [ cssProperty "display" "grid"
  , cssProperty "grid-template-columns" "1fr"
  , cssProperty "width" "80vw"
  , cssProperty "max-width" "400px"
  , cssProperty "margin" "0 auto"
  ]

sortByCSS :: T.Text
sortByCSS = cssEntry ".sort-by"
  [ cssProperty "padding" ".5rem"
  , cssProperty "text-align" "left"
  ]

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

aorbsContainerCSS :: T.Text
aorbsContainerCSS = cssEntry "#aorbs-container, .aorbs-container"
  [ cssProperty "margin-top" "4rem"
  , cssProperty "display" "grid"
  , cssProperty "place-items" "center"
  , cssProperty "justify-content" "stretch"
  , cssProperty "width" "80vw"
  , cssProperty "gap" "2rem"
  ]

aorbDisplayCSS :: T.Text
aorbDisplayCSS = combineCSS
  [ cssEntry ".aorb"
    [ cssProperty "border" "1px solid #ddd"
    , cssProperty "max-width" "800px"
    , cssProperty "order" "var(--order-dice)"
    , cssProperty "padding" "1rem 1rem 0.8rem 1rem"
    , cssProperty "width" "100%"
    , cssProperty "text-align" "left"
    , cssProperty "border-radius" "0.5rem"
    , cssProperty "outline" "2px solid #ddd"
    , cssProperty "outline-offset" "2px"
    , cssProperty "transition" "background-color 0.2s, outline-color 0.2s"
    ]
  , cssEntry ".context"
    [ cssProperty "font-style" "italic"
    , cssProperty "color" "gray"
    , cssProperty "margin-bottom" "1rem"
    ]
  , cssEntry ".choice"
    [ cssProperty "margin" "0.5rem 0"
    ]
  , cssEntry ".choice.preferred"
    [ cssProperty "font-weight" "bold"
    , cssProperty "font-size" "1.1rem"
    ]
  , cssEntry ".choice.alternative"
    [ cssProperty "font-size" "0.9rem"
    , cssProperty "color" "gray"
    ]
  , cssEntry ".choice.selected"
    [ cssProperty "color" "#4169e1"
    ]
  , cssEntry ".percentage"
    [ cssProperty "color" "#4169e1"
    , cssProperty "margin-left" "0.5rem"
    ]
  , cssEntry ".delta"
    [ cssProperty "color" "orange"
    , cssProperty "margin-left" "0.5rem"
    ]
  , cssEntry ".neutral"
    [ cssProperty "color" "gray"
    ]
  ]

noMainInstructionsCSS :: T.Text
noMainInstructionsCSS = cssEntry ".no-main-instructions"
  [ cssProperty "text-align" "center"
  , cssProperty "padding" "2rem"
  , cssProperty "background-color" "#f5f5f5"
  , cssProperty "border-radius" "0.5rem"
  , cssProperty "margin" "1rem auto"
  , cssProperty "max-width" "600px"
  ]

notchCSS :: T.Text
notchCSS = cssEntry ".notch"
  [ cssProperty "position" "sticky"
  , cssProperty "top" "95dvh"
  , cssProperty "transform" "translateX(50%)"
  , cssProperty "z-index" "1000"
  , cssProperty "padding" "0.2rem"
  , cssProperty "border" "2px solid #4169e1"
  , cssProperty "background" "#4169e1"
  , cssProperty "border-radius" "10px"
  ]

-- | Profile CSS

profilePageCSS :: Maybe T.Text -> T.Text
profilePageCSS maybeUuid = combineCSS
  [ baseCSS
  , navBarCSS
  , sorterCSS
  , sortByCSS
  , byBasicTargetCSS
  , byFlakeTargetCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , noMainInstructionsCSS
  , clickableAorbCSS
  , notchCSS
  , case maybeUuid of
      Just _ -> sharedViewOverrides
      Nothing -> ""
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

sharedViewOverrides :: T.Text
sharedViewOverrides = combineCSS
  [ cssEntry ".percentage"
    [ cssProperty "color" "orange"
    ]
  , cssEntry ".delta"
    [ cssProperty "color" "orange"
    ]
  , cssEntry ".choice.selected"
    [ cssProperty "color" "orange"
    ]
  , cssEntry ".aorb-clickable"
    [ cssProperty "pointer-events" "none"
    , cssProperty "cursor" "default"
    ]
  , cssEntry ".aorb-clickable:hover"
    [ cssProperty "transform" "none"
    ]
  , cssEntry ".aorb:hover"
    [ cssProperty "background-color" "inherit"
    ]
  ]

-- | Answer CSS

ansPageCSS :: T.Text
ansPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , ansComponentsCSS
  ]

ansComponentsCSS :: T.Text
ansComponentsCSS = combineCSS
  [ cssEntry ".ans-context"
    [ cssProperty "text-align" "center"
    , cssProperty "padding" "2rem 1rem"
    , cssProperty "font-style" "italic"
    , cssProperty "color" "gray"
    ]
  , cssEntry ".ans-choices"
    [ cssProperty "display" "grid"
    , cssProperty "grid-template-columns" "1fr"
    , cssProperty "grid-auto-rows" "1fr"
    , cssProperty "gap" "5rem"
    , cssProperty "padding" "1rem"
    , cssProperty "max-width" "1200px"
    , cssProperty "margin" "0 auto"
    , cssProperty "width" "80vw"
    ]
  , cssEntry ".ans-choice"
    [ cssProperty "border" "1px solid #ddd"
    , cssProperty "padding" "2rem"
    , cssProperty "border-radius" "0.5rem"
    , cssProperty "min-height" "160px"
    , cssProperty "text-align" "center"
    , cssProperty "cursor" "pointer"
    , cssProperty "outline" "2px solid #ddd"
    , cssProperty "outline-offset" "2px"
    , cssProperty "transition" "background-color 0.2s, outline-color 0.2s"
    ]
  , cssEntry ".ans-choice:hover"
    [ cssProperty "background-color" "#f5f5f5"
    , cssProperty "outline-color" "#aaa"
    ]
  , cssEntry ".ans-choice.selected"
    [ cssProperty "border-color" "#4169e1"
    , cssProperty "outline-color" "#4169e1"
    , cssProperty "color" "#4169e1"
    , cssProperty "background-color" "rgba(65, 105, 225, 0.05)"
    ]
  , cssEntry ".ans-choice.selected:hover"
    [ cssProperty "background-color" "rgba(65, 105, 225, 0.1)"
    ]
  , cssEntry ".ans-choice.selected.favorite"
    [ cssProperty "border-color" "orange"
    , cssProperty "outline-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "background-color" "rgba(255, 165, 0, 0.05)"
    ]
  , cssEntry ".ans-choice.selected.favorite:hover"
    [ cssProperty "background-color" "rgba(255, 165, 0, 0.1)"
    ]
  , cssEntry "button.ans-choice"
    [ cssProperty "width" "100%"
    , cssProperty "height" "100%"
    , cssProperty "border" "none"
    , cssProperty "font" "inherit"
    ]
  , cssEntry ".favorite-section"
    [ cssProperty "margin-top" "3rem"
    , cssProperty "text-align" "center"
    ]
  , cssEntry ".favorite-button"
    [ cssProperty "padding" "1rem 2rem"
    , cssProperty "border" "2px solid #ddd"
    , cssProperty "background" "transparent"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
    , cssProperty "transition" "all 0.2s"
    , cssProperty "border-radius" "0.5rem"
    ]
  , cssEntry ".favorite-button:hover"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    ]
  , cssEntry ".favorite-button.active"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "background-color" "rgba(255, 165, 0, 0.1)"
    ]
  , cssMediaQuery "(min-width: 1200px)"
    [ cssEntry ".ans-choices"
      [ cssProperty "grid-template-columns" "1fr 1fr"
      ]
    ]
  , cssMediaQuery "(prefers-color-scheme: dark)"
    [ cssEntry ".ans-choice"
      [ cssProperty "border-color" "#333"
      , cssProperty "outline-color" "#333"
      ]
    , cssEntry ".ans-choice:hover"
      [ cssProperty "background-color" "#2a2a2a"
      , cssProperty "outline-color" "#444"
      ]
    ]
  ]

clickableAorbCSS :: T.Text
clickableAorbCSS = combineCSS
  [ cssEntry ".aorb-clickable"
    [ cssProperty "color" "inherit"
    , cssProperty "cursor" "pointer"
    , cssProperty "transition" "all 0.2s ease-in-out"
    , cssProperty "display" "block"
    , cssProperty "text-decoration" "none"
    , cssProperty "width" "95%"
    , cssProperty "place-items" "center"
    , cssProperty "order" "var(--order-flake)"
    ]
  , cssEntry ".aorb-clickable:hover"
    [ cssProperty "transform" "translateY(-2px)"
    , cssProperty "outline-color" "#aaa"
    ]
  , cssEntry ".aorb:hover"
    [ cssProperty "background-color" "#f5f5f5"
    ]
  , cssEntry ".aorb-favorite"
    [ cssProperty "border-color" "orange"
    , cssProperty "outline" "2px solid orange"
    ]
  , cssEntry ".favorite-button"
    [ cssProperty "margin-top" "1rem"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border" "2px solid #ddd"
    , cssProperty "background" "transparent"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "transition" "all 0.2s"
    ]
  ]

-- | Match CSS

matchPageCSS :: T.Text
matchPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , matchTypeCSS
  ]

matchTypeCSS :: T.Text
matchTypeCSS = combineCSS
  [ cssEntry ".scheme-grid"
    [ cssProperty "display" "grid"
    , cssProperty "grid-template-columns" "1fr"
    , cssProperty "gap" "2rem"
    , cssProperty "margin" "2rem auto"
    , cssProperty "max-width" "1200px"
    ]
  , cssMediaQuery "(min-width: 900px)"
    [ cssEntry ".scheme-grid"
      [ cssProperty "grid-template-columns" "repeat(3, 1fr)"
      ]
    ]
  , cssEntry ".scheme-card"
    [ cssProperty "border" "1px solid #ddd"
    , cssProperty "padding" "2rem"
    , cssProperty "border-radius" "0.5rem"
    , cssProperty "cursor" "pointer"
    , cssProperty "outline" "2px solid #ddd"
    , cssProperty "outline-offset" "2px"
    , cssProperty "transition" "all 0.2s"
    , cssProperty "display" "block"
    , cssProperty "width" "100%"
    , cssProperty "text-decoration" "none"
    , cssProperty "color" "inherit"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "inherit"
    , cssProperty "background" "none"
    ]
  , cssEntry ".scheme-card:hover"
    [ cssProperty "background-color" "#f5f5f5"
    , cssProperty "outline-color" "#aaa"
    ]
  , cssEntry ".scheme-card.selected"
    [ cssProperty "border-color" "orange"
    , cssProperty "outline-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "font-weight" "bold"
    ]
  , cssEntry ".back-link"
    [ cssProperty "display" "block"
    , cssProperty "margin" "1rem 0"
    ]
  , cssEntry ".description-frame"
    [ cssProperty "margin-top" "4rem"
    , cssProperty "padding" "2rem"
    , cssProperty "background-color" "#f5f5f5"
    , cssProperty "border-radius" "0.5rem"
    ]
  , cssEntry ".scheme-name.PPPod"
    [ cssProperty "font-family" "'Times New Roman', serif"
    , cssProperty "font-size" "1.5rem"
    ]
  , cssEntry ".scheme-name.Fencer"
    [ cssProperty "font-family" "'Courier New', monospace"
    , cssProperty "font-size" "1.5rem"
    ]
  , cssEntry ".scheme-name.Bipolar"
    [ cssProperty "font-family" "'Arial Black', sans-serif"
    , cssProperty "font-size" "1.5rem"
    ]
  , cssMediaQuery "(prefers-color-scheme: dark)"
    [ cssEntry ".scheme-card:hover"
      [ cssProperty "background-color" "#2a2a2a"
      ]
    , cssEntry ".description-frame"
      [ cssProperty "background-color" "#2a2a2a"
      ]
    ]
  ]

-- | Auth CSS

authPageCSS :: T.Text
authPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , cssEntry "div.auth-form"
    [ cssProperty "max-width" "400px"
    , cssProperty "margin" "0 auto"
    ]
  , cssEntry "form.auth-form"
    [ cssProperty "display" "flex"
    , cssProperty "flex-direction" "column"
    ]
  , cssEntry ".auth-input"
    [ cssProperty "padding" "0.5rem"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "inherit"
    ]
  , cssEntry ".auth-button"
    [ cssProperty "width" "100%"
    , cssProperty "padding" "0.5rem"
    , cssProperty "margin" "1rem 0"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "inherit"
    , cssProperty "cursor" "pointer"
    ]
  ]

-- | Account CSS

accountPageCSS :: T.Text
accountPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , cssEntry ".account-section"
    [ cssProperty "margin" "2rem auto"
    , cssProperty "max-width" "600px"
    , cssProperty "text-align" "left"
    ]
  , cssEntry ".account-heading"
    [ cssProperty "border-bottom" "2px solid #ddd"
    , cssProperty "padding-bottom" "0.5rem"
    , cssProperty "margin-bottom" "1rem"
    ]
  , cssEntry ".danger-zone"
    [ cssProperty "margin-top" "3rem"
    , cssProperty "padding" "1rem"
    , cssProperty "border" "2px solid #ff6b6b"
    , cssProperty "border-radius" "4px"
    ]
  , cssEntry ".danger-heading"
    [ cssProperty "color" "#ff6b6b"
    , cssProperty "margin-top" "0"
    ]
  , cssEntry ".confirm-button"
    [ cssProperty "background-color" "#ff6b6b"
    , cssProperty "color" "white"
    , cssProperty "border" "none"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border-radius" "4px"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
    , cssProperty "margin-top" "1rem"
    ]
  , cssEntry ".cancel-button"
    [ cssProperty "display" "block"
    , cssProperty "background-color" "transparent"
    , cssProperty "color" "inherit"
    , cssProperty "border" "1px solid #ddd"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border-radius" "4px"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
    ]
  ]

-- | Helper Functions for Aorb Dynamic CSS

aorbDynamicCSS :: [(String, Int)] -> H.AttributeValue
aorbDynamicCSS orderPairs =
  H.preEscapedTextValue $ inlineCSSEntry
    [ "--order-" <> T.pack name <> ": " <> T.pack (show order)
    | (name, order) <- orderPairs ]
