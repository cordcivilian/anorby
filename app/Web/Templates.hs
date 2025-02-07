{-# LANGUAGE OverloadedStrings #-}

module Web.Templates where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Word as Word
import qualified Text.Printf as Text

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as I

import Types
import Web.Styles
import Web.Types
import Utils.Time
import Utils.Config
import Utils.MatchState

-- | Navigation Components

navBar :: [NavLink] -> H.Html
navBar links = H.div H.! A.class_ "navbar bg-base-100 flex justify-center flex-wrap gap-4" $ do
  let separator = H.span H.! A.class_ "text-base-content/50" $ "///"
      withSeparators [] = return ()
      withSeparators [x] = navLink x
      withSeparators (x:xs) = H.div H.! A.class_ "flex items-center gap-4" $ do
        navLink x
        separator
        withSeparators xs
      navLink link =
        if linkActive link
          then H.span
            H.! A.class_ "text-primary font-medium" $
            H.toHtml $ linkText link
          else H.a
            H.! A.class_ "link hover:text-primary transition-colors"
            H.! A.href (H.textValue $ linkPath link) $
            H.toHtml $ linkText link
  withSeparators links

frame :: H.Html -> H.Html
frame content = H.div
  H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $
  content

-- | Core Templates

rootTemplate :: Int -> [Aorb] -> H.Html
rootTemplate userCount' aorbs = H.docTypeHtml $
  H.html H.! H.dataAttribute "theme" "light" $ do
    H.head $ do
      H.link H.! A.rel "icon" H.! A.href "data:,"
      H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
      H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
      H.title "anorby"
    H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
      H.div H.! A.class_ "container mx-auto px-4" $ do
        navBar [ NavLink "/" "home" True
               , NavLink "/whoami" "whoami" False
               , NavLink "/ans" "answer" False
               , NavLink "/match" "match" False
               ]
        H.div H.! A.class_ "prose prose-lg max-w-none" $ do
          H.h1 H.! A.class_ "text-center" $ do
            H.span H.! A.class_ "border-b-4 border-primary" $ "a"
            H.text "n"
            H.span H.! A.class_ "border-b-4 border-primary" $ "or"
            H.span H.! A.class_ "border-b-4 border-primary" $ "b"
            H.text "y"
            H.div $ do
              H.a H.! A.href "#baseline" $ "the underground census"

      H.span H.! A.id "baseline" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "baseline_100"
        H.h4 H.! A.class_ "text-lg mb-6" $ "what is this... blah blah blah"
        H.a H.! A.href "/roadmap" H.! A.class_ "mb-8" $ do
          H.h4 H.! A.class_ "text-lg" $
            H.text $ T.pack $ "# of responses: " ++ show userCount'

        H.div H.! A.id "sorter" H.! A.class_ "w-full max-w-md mx-auto space-y-2" $ do
          H.div H.! A.class_ "text-left py-2" $ "sort by:"
          H.a H.! A.class_ "block text-left py-2 hover:text-primary transition-colors"
             H.! A.href "#by-sided" $ "> most one-sided"
          H.a H.! A.class_ "block text-left py-2 hover:text-primary transition-colors"
             H.! A.href "#by-polar" $ "> most polarizing"
          H.a H.! A.class_ "block text-left py-2 hover:text-primary transition-colors"
             H.! A.href "#by-dice" $ "> random"

      H.span H.! A.id "aorbs" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4 pt-[10vh]" $ do
        H.div H.! A.id "by-sided" $ mempty
        H.div H.! A.id "by-dice" $ mempty
        H.div H.! A.id "by-polar" $ mempty
        H.div H.! A.class_ "fixed bottom-[5vh] right-1/2 translate-x-1/2 border-2 border-primary bg-primary text-primary-content px-4 py-1 rounded-lg z-50" $ do
          H.a H.! A.href "#baseline" H.! A.class_ "hover:opacity-80" $
            "backtobasebasebase"
        publicAorbs aorbs

roadmapTemplate :: Int -> H.Html
roadmapTemplate _ = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
    H.title "roadmap"
  H.body $ do
    navBar [ NavLink "/#baseline" "back" False ]
    H.h1 "roadmap"
    H.div H.! A.class_ "flex items-center justify-center" $ do
      H.ul H.! A.class_ "steps steps-vertical" $ do
        H.li H.! A.class_ "step step-primary" H.! H.dataAttribute "content" "+" $ "100"
        H.li H.! A.class_ "step step-primary" H.! H.dataAttribute "content" "+" $ "500"
        H.li H.! A.class_ "step" H.! H.dataAttribute "content" "?" $ "1000"
        H.li H.! A.class_ "step" H.! H.dataAttribute "content" "?" $ "5000"
        H.li H.! A.class_ "step" H.! H.dataAttribute "content" "?" $ "10000"
        H.li H.! A.class_ "step" H.! H.dataAttribute "content" "?" $ "20000"
        H.li H.! A.class_ "step" H.! H.dataAttribute "content" "?" $ "30000"

publicAorbs :: [Aorb] -> H.Html
publicAorbs aorbs = do
  H.div H.! A.id "aorbs-container" H.!
    A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
    Monad.forM_ (aorbWithOrders aorbs) $
      \(_, (aorb, orders)) -> do
        H.div H.! A.class_ "w-full max-w-2xl border border-base-300 rounded-lg p-4 transition-all hover:bg-base-200"
              H.! A.style (aorbDynamicCSS (zip ["dice", "polar", "sided"] orders)) $ do
          H.div H.! A.class_ "text-base-content/60 italic mb-4" $
            H.toHtml (aorbCtx aorb)
          let mean = aorbMean aorb
              delta = (abs (mean - 0.5)) * 100
              formatDelta = T.concat ["(+", T.pack (Text.printf "%.2f" delta), ")"]
          if delta < 0.01
            then do
              H.div H.! A.class_ "my-2" $ do
                H.toHtml (aorbA aorb)
                H.span H.! A.class_ "text-base-content/50" $
                  H.toHtml $ T.pack " ...?"
              H.div H.! A.class_ "my-2" $ do
                H.toHtml (aorbB aorb)
                H.span H.! A.class_ "text-base-content/50" $
                  H.toHtml $ T.pack " ...?"
            else if mean > 0.5
            then do
              H.div H.! A.class_ "text-lg font-medium my-2" $ do
                H.toHtml (aorbB aorb)
                H.span H.! A.class_ "text-warning ml-2" $
                  H.toHtml formatDelta
              H.div H.! A.class_ "text-base-content/70 text-sm my-2" $
                H.toHtml (aorbA aorb)
            else do
              H.div H.! A.class_ "text-lg font-medium my-2" $ do
                H.toHtml (aorbA aorb)
                H.span H.! A.class_ "text-warning ml-2" $
                  H.toHtml formatDelta
              H.div H.! A.class_ "text-base-content/70 text-sm my-2" $
                H.toHtml (aorbB aorb)

profileTemplate :: [AorbWithAnswer] -> Maybe AorbID -> Maybe T.Text -> Maybe T.Text -> H.Html
profileTemplate aorbs mAid maybeUuid shareUrl = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title $ case maybeUuid of
                Just uuid -> H.text $ "share/" <> uuid
                Nothing -> "whoami"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
    H.style $ case maybeUuid of
                Just _ -> "/* Shared view overrides */"
                Nothing -> ""
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    profileHeadline maybeUuid $
      H.div H.! A.class_ "text-center" $ do
        H.a H.! A.href "#main" H.! A.class_ "link hover:text-primary transition-colors" $ "begin"
    profileFullView mAid aorbs maybeUuid shareUrl

profileHead :: Maybe T.Text -> H.Html
profileHead maybeUuid = H.head $ do
  H.title $ case maybeUuid of
    Just uuid -> H.text $ "share/" <> uuid
    Nothing -> "whoami"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
  H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  case maybeUuid of
    Just _ -> H.style $ I.preEscapedText $ T.unlines
      [ "/* Shared view color overrides */"
      , ".percentage { @apply text-warning; }"
      , ".delta { @apply text-warning; }"
      , ".choice.selected { @apply text-warning; }"
      , ".text-primary { color: orange; }"
      , ".aorb-clickable { pointer-events: none; cursor: default; }"
      , ".aorb-clickable:hover { transform: none; }"
      , ".aorb:hover { background-color: inherit; }"
      ]
    Nothing -> mempty

profileHeadline :: Maybe T.Text -> H.Html -> H.Html
profileHeadline maybeUuid children = do
  H.span H.! A.id "top" $ ""
  H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    navBar [ NavLink "/" "home" False
      , NavLink "/whoami" "whoami" (Maybe.isNothing maybeUuid)
      , NavLink "/ans" "answer" False
      , NavLink "/match" "match" False
      ]
    H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ case maybeUuid of
      Just uuid -> H.text $ "#" <> uuid
      Nothing -> "whoami"
    children

profileMainAorb :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> H.Html
profileMainAorb mAid aorbs maybeUuid =
  case (mAid, maybeUuid) of
    (Just aid, Just _) -> do
      H.span H.! A.id "main" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "main"
        H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
          mapM_ (\awa -> profileAorb awa mAid Nothing maybeUuid) $
            filter (\awa -> aorbId (aorbData awa) == aid) aorbs
    (Nothing, Just _) -> mempty
    (Just aid, Nothing) -> do
      H.span H.! A.id "main" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "main"
        H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
          mapM_ (\awa -> profileAorb awa mAid Nothing maybeUuid) $
            filter (\awa -> aorbId (aorbData awa) == aid) aorbs
    (Nothing, Nothing) -> do
      H.span H.! A.id "main" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "main"
        H.div H.! A.class_ "prose max-w-lg mx-auto text-center" $ do
          H.p "you haven't selected your main question yet"
          H.p "pick from the answers below"

profileCommonplaceAorbs :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> H.Html
profileCommonplaceAorbs mAid aorbs maybeUuid = do
  H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "most commonplace"
    H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
      mapM_ (\awa -> profileAorb awa mAid Nothing maybeUuid) (take 3 $ reverse aorbs)

profileControversialAorbs :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> H.Html
profileControversialAorbs mAid aorbs maybeUuid = do
  H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "most controversial"
    H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
      mapM_ (\awa -> profileAorb awa mAid Nothing maybeUuid) (take 3 aorbs)

profileAllAnswers :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> H.Html
profileAllAnswers mAid aorbs maybeUuid = do
  H.span H.! A.id "all-answers" $ ""
  H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "all answers"
    H.div H.! A.class_ "w-full max-w-md mx-auto space-y-2" $ do
      H.div H.! A.class_ "text-left py-2" $ "sort by:"
      H.a H.! A.class_ "block text-left py-2 hover:text-primary transition-colors"
         H.! A.href "#by-flake" $ "> most controversial"
      H.a H.! A.class_ "block text-left py-2 hover:text-primary transition-colors"
         H.! A.href "#by-basic" $ "> most commonplace"
  H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4 pt-[10vh]" $ do
    H.div H.! A.id "by-basic" $ mempty
    H.div H.! A.id "by-flake" $ mempty
    H.div H.! A.class_ "fixed bottom-[5vh] right-1/2 translate-x-1/2 border-2 border-primary bg-primary text-primary-content px-4 py-1 rounded-lg z-50" $ do
      H.a H.! A.href "#all-answers" H.! A.class_ "hover:opacity-80" $ "backtoallanswersss"
    profileOrdinaryAorbs mAid aorbs maybeUuid

profileAorb :: AorbWithAnswer -> Maybe AorbID -> Maybe [Int] -> Maybe T.Text -> H.Html
profileAorb awa mFavoriteId mOrders maybeUuid = do
  let aorb = aorbData awa
      ans = userAnswer awa
      aid = aorbId aorb
      isFavorite = maybe False (== aid) mFavoriteId
      percentage = case ans of
        AorbAnswer 0 -> 100 * (1 - aorbMean aorb)
        _ -> 100 * aorbMean aorb
      dynamicStyle = case mOrders of
        Just orders -> (H.! A.style (aorbDynamicCSS (zip ["basic", "flake"] orders)))
        Nothing -> id
      baseWrapper contents = case maybeUuid of
        Just _ -> H.div H.! wrapperClass $ contents
        Nothing -> H.a H.! A.href (H.toValue $ "/ans/" ++ show aid) H.! wrapperClass $ contents
      wrapperClass = A.class_ $ "w-full hover:-translate-y-1 transition-transform " <>
        if maybeUuid == Nothing then "cursor-pointer" else "cursor-default"

  dynamicStyle $ baseWrapper $ do
    H.div H.! A.class_ ("w-full border border-base-300 rounded-lg p-4 transition-colors hover:bg-base-200" <>
      if isFavorite then " border-warning" else "") $ do
      H.div H.! A.class_ "text-base-content/60 italic mb-4" $ H.toHtml $ aorbCtx aorb
      H.div H.! A.class_ (getChoiceClasses (ans == AorbAnswer 0) maybeUuid) $ do
        H.toHtml $ aorbA aorb
        Monad.when (ans == AorbAnswer 0) $
          H.span H.! A.class_ (getPercentageClasses maybeUuid) $
            H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage
      H.div H.! A.class_ (getChoiceClasses (ans == AorbAnswer 1) maybeUuid) $ do
        H.toHtml $ aorbB aorb
        Monad.when (ans == AorbAnswer 1) $
          H.span H.! A.class_ (getPercentageClasses maybeUuid) $
            H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage

  where
    getChoiceClasses :: Bool -> Maybe T.Text -> H.AttributeValue
    getChoiceClasses isSelected maybeUuid' =
      if isSelected
        then case maybeUuid' of
          Just _ -> "text-lg font-medium my-2 text-warning"
          Nothing -> "text-lg font-medium my-2 text-primary"
        else "text-base-content/70 text-sm my-2"

    getPercentageClasses :: Maybe T.Text -> H.AttributeValue
    getPercentageClasses pct = case pct of
      Just _ -> "text-warning ml-2"
      Nothing -> "text-primary ml-2"

profileFullView :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> Maybe T.Text -> H.Html
profileFullView mAid aorbs maybeUuid shareUrl = do
  profileMainAorb mAid aorbs maybeUuid
  Monad.when (Maybe.isNothing mAid) $ H.span H.! A.id "main" $ ""
  profileCommonplaceAorbs mAid aorbs maybeUuid
  profileControversialAorbs mAid aorbs maybeUuid
  profileAllAnswers mAid aorbs maybeUuid
  profileSharer maybeUuid shareUrl
  accountManager maybeUuid

profileSharer :: Maybe T.Text -> Maybe T.Text -> H.Html
profileSharer maybeUuid shareUrl = case (maybeUuid, shareUrl) of
  (Nothing, Just url) -> H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "share"
    H.div $ H.text url
  _ -> mempty

accountManager :: Maybe T.Text -> H.Html
accountManager maybeUuid = case maybeUuid of
  Nothing -> H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
    H.div $ do
      H.a H.! A.href "/account" H.! A.class_ "link hover:text-primary transition-colors" $
        "manage my account"
  _ -> mempty

profileOrdinaryAorbs :: Maybe AorbID -> [AorbWithAnswer] -> Maybe T.Text -> H.Html
profileOrdinaryAorbs mAid aorbs maybeUuid = do
  H.div H.! A.id "aorbs-container" H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
    Monad.forM_ (aorbWithAnswerWithOrders aorbs) $
      \(_, (awa, orders)) -> profileAorb awa mAid (Just orders) maybeUuid

aorbsWithOrders :: (Eq a) => [a] -> [OrderingFunction a] -> [(Int, (a, [Int]))]
aorbsWithOrders as orderingFuncs = zip [(1::Int)..] $
  let orderedLists = map (\f -> f as) orderingFuncs
      lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
  in [ (a, map (\orderedList -> lookupOrder orderedList a) orderedLists)
     | a <- as ]

aorbWithOrders :: [Aorb] -> [(Int, (Aorb, [Int]))]
aorbWithOrders as = aorbsWithOrders as aorbOrderings

aorbWithAnswerWithOrders :: [AorbWithAnswer]
                         -> [(Int, (AorbWithAnswer, [Int]))]
aorbWithAnswerWithOrders awas = aorbsWithOrders awas aorbWithAnswerOrderings

aorbOrderings :: [OrderingFunction Aorb]
aorbOrderings =
  [ id                                                     -- byDice
  , List.sortOn (\a -> abs (aorbMean a - 0.5))             -- byPolar
  , List.sortOn (Ord.Down . \a -> abs (aorbMean a - 0.5))  -- bySided
  ]

aorbWithAnswerOrderings :: [OrderingFunction AorbWithAnswer]
aorbWithAnswerOrderings =
  [ reverse   -- byBasic
  , id        -- byFlake
  ]

ansTemplate :: Aorb -> Bool -> T.Text -> H.Html
ansTemplate aorb shouldSwap token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
            , NavLink "/whoami" "whoami" False
            , NavLink "/ans" "answer" True
            , NavLink "/match" "match" False
            ]
      H.div H.! A.class_ "text-center p-8 text-base-content/60 italic" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "grid gap-20 p-4 max-w-4xl mx-auto w-4/5" $
        let (firstChoice, firstValue, secondChoice, secondValue) =
              if shouldSwap
                then (aorbB aorb, 1, aorbA aorb, 0)
                else (aorbA aorb, 0, aorbB aorb, 1)
        in do
          makeChoice aorb token firstChoice firstValue
          makeChoice aorb token secondChoice secondValue
  where
    makeChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> H.Html
    makeChoice a t choice value = do
      H.form H.! A.method "POST" H.! A.action "/ans/submit" $ do
        H.input H.! A.type_ "hidden" H.!
          A.name "aorb_id" H.!
          A.value (H.toValue $ show $ aorbId a)
        H.input H.! A.type_ "hidden" H.!
          A.name "token" H.!
          A.value (H.textValue t)
        H.input H.! A.type_ "hidden" H.!
          A.name "choice" H.!
          A.value (H.toValue $ show value)
        H.button H.!
          A.type_ "submit" H.!
          A.class_ "w-full min-h-[160px] p-8 text-center border border-base-300 rounded-lg transition-all hover:bg-base-200 hover:border-base-300 cursor-pointer font-inherit" $
          H.toHtml choice

existingAnswerTemplate :: Aorb -> Maybe AorbAnswer -> Bool -> T.Text -> H.Html
existingAnswerTemplate aorb mCurrentAnswer isFavorite token =
  H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
            , NavLink "/whoami" "whoami" False
            , NavLink "/ans" "answer" True
            , NavLink "/match" "match" False
            ]
      H.div H.! A.class_ "text-center p-8 text-base-content/60 italic" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "grid gap-20 p-4 max-w-4xl mx-auto w-4/5" $ do
        makeExistingChoice aorb token (aorbA aorb) 0
          (mCurrentAnswer == Just (AorbAnswer 0)) isFavorite
        makeExistingChoice aorb token (aorbB aorb) 1
          (mCurrentAnswer == Just (AorbAnswer 1)) isFavorite
      if isFavorite
        then mempty
        else
          H.div H.! A.class_ "mt-12 text-center" $ do
            H.form H.! A.method "POST"
                  H.! A.action (H.toValue $ "/aorb/favorite/" ++ show (aorbId aorb)) $ do
              H.button H.! A.type_ "submit"
                      H.! A.class_ "px-8 py-4 border-2 border-base-300 bg-transparent cursor-pointer font-inherit text-base transition-all hover:border-warning hover:text-warning rounded-lg" $
                "set as favorite question"
  where
    makeExistingChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> Bool -> Bool -> H.Html
    makeExistingChoice a t choice value isSelected favorite = do
      let baseClasses = "w-full min-h-[160px] p-8 text-center border rounded-lg transition-all cursor-pointer font-inherit"
          selectedClasses = if isSelected
            then if favorite
              then " border-warning text-warning bg-warning/5 hover:bg-warning/10"
              else " border-primary text-primary bg-primary/5 hover:bg-primary/10"
            else " border-base-300 hover:bg-base-200"
      H.form H.! A.method "POST" H.! A.action "/ans/edit" $ do
        H.input H.! A.type_ "hidden" H.!
          A.name "aorb_id" H.!
          A.value (H.toValue $ show $ aorbId a)
        H.input H.! A.type_ "hidden" H.!
          A.name "token" H.!
          A.value (H.textValue t)
        H.input H.! A.type_ "hidden" H.!
          A.name "choice" H.!
          A.value (H.toValue $ show value)
        H.button H.! A.type_ "submit"
                H.! A.class_ (H.textValue $ baseClasses <> selectedClasses) $
          H.toHtml choice

matchTemplate :: Config
              -> Maybe POSIXTime.POSIXTime
              -> Maybe POSIXTime.POSIXTime
              -> POSIXTime.POSIXTime
              -> Bool
              -> Int
              -> Maybe (Match, Double)
              -> MatchStatus
              -> H.Html
matchTemplate config maybeCutoffTime maybeReleaseTime now isEnrolled enrolledCount maybeMatchScore matchStatus =
  H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
    H.title "match"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.span H.! A.id "top" $ ""
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" False
             , NavLink "/match" "match" True
             ]
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "match"
      H.div H.! A.class_ "flex justify-center gap-4 flex-wrap" $ do
        H.a H.! A.href "/match/found" H.! A.class_ "link hover:text-primary transition-colors" $ "past"
        H.span H.! A.class_ "text-base-content/50" $ "|"
        H.a H.! A.href "#today" H.! A.class_ "link hover:text-primary transition-colors" $ "present"
        H.span H.! A.class_ "text-base-content/50" $ "|"
        H.a H.! A.href "/match/type" H.! A.class_ "link hover:text-primary transition-colors" $ "future"
    H.span H.! A.id "today" $ ""
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "today"
      matchTodaySection config maybeCutoffTime maybeReleaseTime now isEnrolled enrolledCount maybeMatchScore matchStatus

matchTodaySection :: Config
                  -> Maybe POSIXTime.POSIXTime
                  -> Maybe POSIXTime.POSIXTime
                  -> POSIXTime.POSIXTime
                  -> Bool
                  -> Int
                  -> Maybe (Match, Double)
                  -> MatchStatus
                  -> H.Html
matchTodaySection config maybeCutoffTime maybeReleaseTime now isEnrolled enrolledCount maybeMatchScore matchStatus =
  let otherUsersCount = max 0 (enrolledCount - 1)
      enrolledText =
        if otherUsersCount == 1
        then "1 other user enrolled"
        else T.pack (show otherUsersCount) <> " other users enrolled"
      isBeforeCutoff = case maybeCutoffTime of
        Just ct -> now < ct
        Nothing -> True
      isBeforeRelease = case maybeReleaseTime of
        Just rt -> now < rt
        Nothing -> True
      timeUntilCutoff = case maybeCutoffTime of
        Just ct -> formatTimeUntil now ct
        Nothing -> "soon"
      timeUntilRelease = case maybeReleaseTime of
        Just rt -> formatTimeUntil now rt
        Nothing -> "soon"
      currentTimestamp = floor now

  in H.div H.! A.class_ "flex flex-col gap-6 max-w-xl mx-auto text-center p-8" $ do
      case matchStatus of
        InProgress ->
          H.div H.! A.class_ "p-6 rounded-lg bg-base-200" $
            H.text "matching in progress..."

        Failed err ->
          H.div H.! A.class_ "p-6 rounded-lg bg-base-200" $ do
            H.text "matching failed: "
            H.text (T.pack err)

        Completed ->
          Monad.when isBeforeRelease $
            H.div H.! A.class_ "p-6 rounded-lg bg-base-200" $ do
              H.text "matching completed"

        _ -> do
          Monad.when isBeforeRelease $
            H.div H.! A.class_ "p-6 rounded-lg bg-base-200 text-sm text-base-content/70" $
              H.text enrolledText

      Monad.when isBeforeRelease $
        H.div H.! A.class_ (if isEnrolled
                            then "p-6 rounded-lg bg-primary/10 border-2 border-primary text-primary"
                            else "p-6 rounded-lg bg-base-200") $ do
          if isBeforeCutoff
            then if isEnrolled
              then H.text "you are enrolled for today's matching"
              else do
                H.text "answer more questions to join today's matching pool"
                H.br
                H.a H.! A.href "/ans" H.! A.class_ "link text-primary" $ "answer more questions"
            else if isEnrolled
              then
                case matchStatus of
                  Completed -> H.text "you've been matched"
                  _ -> H.text "you are enrolled for today's matching"
              else H.text "you've missed today's matching cutoff"

      Monad.when isBeforeRelease $
        H.div H.! A.class_ "p-6 rounded-lg bg-base-200 text-sm" $ do
          let displayTime = if isBeforeCutoff && not isEnrolled
                            then (timeUntilCutoff, matchCutoffTime config)
                            else (timeUntilRelease, matchReleaseTime config)
          case displayTime of
            (timeLeft, timeStr) -> do
              if isBeforeCutoff || isEnrolled
                then do
                  H.span H.! A.class_ "font-bold" $
                    if isEnrolled
                      then H.text $ "reveal in " <> timeLeft
                      else H.text $ "cutoff in " <> timeLeft
                  H.span H.! A.class_ "text-base-content/70" $
                    H.text $ " (" <> timeStr <> " UTC)"
                else
                  H.text "answer your questions tomorrow before the cutoff"

      Monad.unless isBeforeRelease $
        case maybeMatchScore of
          Just (match, score) -> matchCard currentTimestamp match score 0
          Nothing -> H.div H.! A.class_ "p-6 rounded-lg bg-base-200" $
            H.text "no match found for today"

matchTypeTemplate :: User -> H.Html
matchTypeTemplate user = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "match type"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/match" "back" False ]
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "match type"

      H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 m-8 max-w-4xl" $ do
        schemeCard PPPod (userAssoc user)
        schemeCard Swing (userAssoc user)
        schemeCard Bipolar (userAssoc user)

      H.div $ do H.a H.! A.href "#explained" H.! A.class_ "link hover:text-primary transition-colors" $ "explain"

    H.span H.! A.id "explained" $ ""
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "bg-base-200 p-8 rounded-lg max-w-2xl" $ do
        H.h2 H.! A.class_ "text-xl font-bold mb-4" $ "how it works"
        H.p H.! A.class_ "mb-4" $ "each type determines how you'll be matched with other users:"
        schemeDetailedDescription PPPod
        schemeDetailedDescription Swing
        schemeDetailedDescription Bipolar

schemeCard :: AssociationScheme -> Maybe AssociationScheme -> H.Html
schemeCard scheme currentScheme =
  let isSelected = currentScheme == Just scheme
      baseClasses = "w-full p-8 rounded-lg cursor-pointer transition-all font-inherit text-inherit bg-transparent"
      selectedClasses = if isSelected
        then " border-warning text-warning border-2"
        else " border border-base-300 hover:bg-base-200"
      schemeName = show scheme
      schemeNameClass = case scheme of
        PPPod -> "font-serif text-2xl"
        Swing -> "font-mono text-2xl"
        Bipolar -> "font-black text-2xl"
  in H.form H.! A.method "POST" H.! A.action "/match/type" $ do
    H.button
      H.! A.type_ "submit"
      H.! A.name "assoc"
      H.! A.value (H.toValue schemeName)
      H.! A.class_ (H.textValue $ baseClasses <> selectedClasses) $ do
        H.div H.! A.class_ (H.textValue schemeNameClass) $
          H.toHtml schemeName

schemeDetailedDescription :: AssociationScheme -> H.Html
schemeDetailedDescription scheme =
  H.div H.! A.class_ "my-8" $ do
    H.h3 H.! A.class_ schemeNameClasses $
      H.toHtml $ schemeName scheme
    H.p H.! A.class_ "mt-2 text-base-content/70" $
      H.toHtml $ schemeDetail scheme
  where
    schemeNameClasses = case scheme of
      PPPod -> "font-serif text-xl"
      Swing -> "font-mono text-xl"
      Bipolar -> "font-black text-xl"

    schemeName :: AssociationScheme -> T.Text
    schemeName PPPod = "PPPod"
    schemeName Swing = "Swing"
    schemeName Bipolar = "Bipolar"

    schemeDetail :: AssociationScheme -> T.Text
    schemeDetail PPPod = "..."
    schemeDetail Swing = "..."
    schemeDetail Bipolar = "..."

matchFoundTemplate :: Integer -> Integer -> [((Match, Double), Int)] -> H.Html
matchFoundTemplate currentTimestamp expiryDays matchData = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "matches"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-[80vh] mt-[10vh]" $ do
      navBar [ NavLink "/match" "back" False ]
      H.h1 H.! A.class_ "text-2xl font-bold mb-4 text-center" $ "matches"
      if null matchData
        then H.div H.! A.class_ "text-center p-8 bg-base-200 rounded-lg mx-auto max-w-xl" $ do
          H.p "no matches found"
        else do
          H.h4 H.! A.class_ "text-center mb-8 text-base-content/70" $ "(agreement rate)"
          H.div H.! A.class_ "grid gap-8 max-w-2xl mx-auto p-4" $ do
            let startOfToday = (div currentTimestamp 86400) * 86400
                expiryAgo = currentTimestamp - (expiryDays * 24 * 60 * 60)
                pastMatches = filter (\((m, _), _) ->
                  let ts = matchTimestamp m
                  in ts < startOfToday && ts >= expiryAgo) matchData
            mapM_ (\((m, s), u) ->
              matchCard currentTimestamp m s u) pastMatches

matchCard :: Integer -> Match -> Double -> Int -> H.Html
matchCard currentTimestamp match score unreadCount =
  H.a H.! A.class_ "block w-full border border-base-300 p-6 rounded-lg cursor-pointer transition-all hover:bg-base-200 hover:-translate-y-1 text-inherit"
      H.! A.href (H.textValue $ "/match/found/t-" <> formatMatchDelta currentTimestamp (matchTimestamp match)) $ do
    H.div H.! A.class_ "text-sm text-base-content/70 mb-2" $
      H.toHtml $ formatMatchDate currentTimestamp (matchTimestamp match) unreadCount
    H.div H.! A.class_ "text-primary font-bold" $
      H.toHtml $ formatSimilarityScore score
  where
    formatSimilarityScore :: Double -> T.Text
    formatSimilarityScore s =
      T.pack $ Text.printf "%.0f%%" ((s + 1) * 50)

    formatMatchDate :: Integer -> Integer -> Int -> T.Text
    formatMatchDate currentTimestamp' matchTimestamp' unread =
      let dayDelta = getDayDelta currentTimestamp' matchTimestamp'
          baseText = case dayDelta of
            0 -> "today"
            1 -> "yesterday"
            _ -> T.pack $
              DateTimeFormat.formatTime
              DateTimeFormat.defaultTimeLocale
              "%A, %Y-%m-%d"
              (POSIXTime.posixSecondsToUTCTime $ fromIntegral matchTimestamp')
      in if unread > 0 then "[" <> T.pack (show unread) <> "] " <> baseText
                       else baseText

    formatMatchDelta :: Integer -> Integer -> T.Text
    formatMatchDelta currentTimestamp' matchTimestamp' =
      T.pack $ show $ getDayDelta currentTimestamp' matchTimestamp'

    getDayDelta :: Integer -> Integer -> Integer
    getDayDelta currentTimestamp' matchTimestamp' =
      let secondsPerDay = 86400 :: Double
          currentDay = floor (( fromIntegral currentTimestamp' :: Double)
                              / secondsPerDay
                             ) :: Integer
          matchDay = floor (( fromIntegral matchTimestamp' :: Double)
                            / secondsPerDay
                           ) :: Integer
      in currentDay - matchDay

matchProfileTemplate :: Integer -> UserID -> UserID -> MatchView -> [Message] -> H.Html
matchProfileTemplate days mainUserId _ view messages =
  H.docTypeHtml $ H.html $ do
    H.head $ do
      H.title "match profile"
      H.link H.! A.rel "icon" H.! A.href "data:,"
      H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
      H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
    H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
      H.span H.! A.id "top" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        navBar [ NavLink (if days == 0 then "/match#today" else "/match/found") "back" False ]
        H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "stats"
        H.div H.! A.class_ "grid grid-cols-2 gap-4 max-w-xl mx-auto w-full" $ do
          statsBox "matched on" (formatMatchDate (viewTimestamp view))
          statsBox "agreement rate" (formatPercent (viewAgreementRate view))
          statsBox "you answered" (T.pack $ show $ viewYourTotalAnswers view)
          statsBox "they answered" (T.pack $ show $ viewTargetTotalAnswers view)
        H.div $ do
          H.a H.! A.href "#agreement" H.! A.class_ "link hover:text-primary transition-colors" $ "begin"

      H.span H.! A.id "agreement" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "two and the truth is a majority"
        case viewTopAgreement view of
          Just mawa -> matchProfileAgreement mawa
          Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no agreements found"
        H.div $ do
          H.a H.! A.href "#spotlight" H.! A.class_ "link hover:text-primary transition-colors" $ "next"

      H.span H.! A.id "spotlight" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "their roman empire"
        case viewMainAorbs view of
          Just (_, theirMain) -> spotlightAorbSection mainUserId theirMain
          Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no main question found"
        H.div $ do
          H.a H.! A.href "#disagreement" H.! A.class_ "link hover:text-primary transition-colors" $ "next"

      H.span H.! A.id "disagreement" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "plz mend the rift"
        case viewTopDisagreement view of
          Just mawa -> matchProfileDisagreement mainUserId mawa
          Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no disagreements found"
        H.div $ do
          H.a H.! A.href "#messages" H.! A.class_ "link hover:text-primary transition-colors" $ "next"

      H.span H.! A.id "messages" $ ""
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-8" $ "head-to-head"
        renderMessages days mainUserId messages

      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.h1 H.! A.class_ "text-2xl font-bold mb-8" $ "fin"
        H.div $ do
          H.a H.! A.href "#top" H.! A.class_ "link hover:text-primary transition-colors" $ "back to top"
  where
    statsBox :: T.Text -> T.Text -> H.Html
    statsBox label value =
      H.div H.! A.class_ "text-center p-6 bg-base-200 rounded-lg" $ do
        H.div H.! A.class_ "text-sm text-base-content/70 mb-2" $ H.toHtml label
        H.div H.! A.class_ "text-lg font-bold" $ H.toHtml value

    formatMatchDate :: Integer -> T.Text
    formatMatchDate timestamp =
      T.pack $ DateTimeFormat.formatTime DateTimeFormat.defaultTimeLocale "%Y-%m-%d"
        (POSIXTime.posixSecondsToUTCTime $ fromIntegral timestamp)

    formatPercent :: Double -> T.Text
    formatPercent n = T.pack $ Text.printf "%.0f%%" n

spotlightAorbSection :: UserID -> MatchingAorbWithAnswer -> H.Html
spotlightAorbSection _ mawa = do
  let aorb = matchingAorbData mawa
      myAns = mainUserAnswer mawa
      theirAns = otherUserAnswer mawa
      agreement = myAns == theirAns
      theirChoice = if theirAns == AorbAnswer 0 then aorbA aorb else aorbB aorb
      myChoice = if myAns == AorbAnswer 0 then aorbA aorb else aorbB aorb
      otherChoice = if theirAns == AorbAnswer 0 then aorbB aorb else aorbA aorb

  H.div H.! A.class_ "rounded-2xl p-8 min-h-[30vh] w-[60vw] max-w-2xl bg-primary/10 border-2 border-primary" $ do
    H.div H.! A.class_ "text-base-content/60 italic mb-8 leading-relaxed" $
      H.toHtml $ aorbCtx aorb
    H.div H.! A.class_ "grid gap-6" $
      if agreement
        then do
          H.div H.! A.class_ "text-primary text-2xl font-bold" $
            H.toHtml theirChoice
          H.div H.! A.class_ "text-base-content/70 text-sm" $ do
            H.div H.! A.class_ "text-base-content/60 mb-2" $
              "every other idiot"
            H.toHtml otherChoice
        else do
          H.div H.! A.class_ "text-primary text-2xl font-bold" $
            H.toHtml theirChoice
          H.div H.! A.class_ "text-base-content/70 text-sm" $ do
            H.div H.! A.class_ "text-base-content/60 mb-2" $
              "you (proceed cautiously)"
            H.toHtml myChoice

matchProfileAgreement :: MatchingAorbWithAnswer -> H.Html
matchProfileAgreement mawa = do
  let aorb = matchingAorbData mawa
      sharedAns = mainUserAnswer mawa
      mean = aorbMean aorb
      (sharedChoice, otherChoice, agreePct) = case sharedAns of
        AorbAnswer 0 -> (aorbA aorb, aorbB aorb, 100 * (1 - mean))
        _ -> (aorbB aorb, aorbA aorb, 100 * mean)
      otherPct = 100 - agreePct

  H.div H.! A.class_ "rounded-2xl p-8 min-h-[30vh] w-[60vw] max-w-2xl bg-success/10 border-2 border-success" $ do
    H.div H.! A.class_ "text-base-content/60 italic mb-8 leading-relaxed" $
      H.toHtml $ aorbCtx aorb
    H.div H.! A.class_ "grid gap-6" $ do
      H.div H.! A.class_ "text-success text-2xl font-bold" $ do
        H.toHtml sharedChoice
      H.div H.! A.class_ "text-base-content/70" $ do
        H.toHtml $ T.pack $ Text.printf "against %.0f%% of the world" otherPct
      H.div H.! A.class_ "text-base-content/70 text-sm" $
        H.toHtml otherChoice

matchProfileDisagreement :: UserID -> MatchingAorbWithAnswer -> H.Html
matchProfileDisagreement _ mawa = do
  let aorb = matchingAorbData mawa
      myAns = mainUserAnswer mawa
      theirAns = otherUserAnswer mawa
      mean = aorbMean aorb
      (myChoice, myPct) = if myAns == AorbAnswer 0
                          then (aorbA aorb, 100 * (1 - mean))
                          else (aorbB aorb, 100 * mean)
      (theirChoice, theirPct) = if theirAns == AorbAnswer 0
                                then (aorbA aorb, 100 * (1 - mean))
                                else (aorbB aorb, 100 * mean)
  H.div H.! A.class_ "rounded-2xl p-8 min-h-[30vh] w-[60vw] max-w-2xl bg-warning/10 border-2 border-warning" $ do
    H.div H.! A.class_ "text-base-content/60 italic mb-8 leading-relaxed" $
      H.toHtml $ aorbCtx aorb
    H.div H.! A.class_ "grid gap-6" $ do
      H.div H.! A.class_ "text-warning" $ do
        H.div H.! A.class_ "text-base-content/60 mb-2" $
          H.toHtml $ T.pack $ Text.printf "you and your righteous %.0f%%" myPct
        H.div H.! A.class_ "text-xl" $ H.toHtml myChoice
      H.div H.! A.class_ "text-warning" $ do
        H.div H.! A.class_ "text-base-content/60 mb-2" $
          H.toHtml $ T.pack $ Text.printf "them and their precious %.0f%%" theirPct
        H.div H.! A.class_ "text-xl" $ H.toHtml theirChoice

renderMessages :: Integer -> UserID -> [Message] -> H.Html
renderMessages days uid messages = do
  let userMessageCount = length $ filter ((== uid) . messageSenderId) messages
      remainingMessages = 3 - userMessageCount
      hasReachedLimit = userMessageCount >= 3

  H.div H.! A.class_ "w-full max-w-2xl mx-auto space-y-4" $ do
    mapM_ (renderMessage uid) messages

    if hasReachedLimit
      then H.div H.! A.class_ "p-4 text-center rounded-lg bg-base-200" $ do
        "You have reached the limit of 3 messages"
      else H.form H.! A.id "message-form"
           H.! A.method "POST"
           H.! A.action (H.textValue $ "/match/found/t-" <> T.pack (show days) <> "/message")
           H.! A.class_ "flex flex-col gap-4" $ do
        H.div H.! A.class_ "text-sm text-base-content/70 mb-2" $ do
          H.text $ T.pack $ show remainingMessages <> " messages remaining"

        H.textarea H.! A.class_ "w-full p-4 border border-base-300 rounded-lg resize-none font-inherit"
          H.! A.form "message-form"
          H.! A.type_ "text"
          H.! A.id "new-message"
          H.! A.name "new-message"
          H.! A.placeholder "message (max 400 characters)"
          H.! A.required "required"
          H.! A.autocomplete "off"
          H.! A.maxlength "400"
          $ ""

        H.input H.! A.class_ "px-4 py-2 bg-primary text-primary-content rounded-lg cursor-pointer font-inherit hover:bg-primary/90"
          H.! A.type_ "submit"
          H.! A.value "send"

renderMessage :: UserID -> Message -> H.Html
renderMessage uid msg = do
  let baseClasses = "p-4 rounded-lg w-fit max-w-[50%] "
      messageClasses = if messageSenderId msg == uid
        then baseClasses <> "ml-auto border-2 border-primary"
        else baseClasses <> "mr-auto border-2 border-warning"
  H.div H.! A.class_ messageClasses $ do
    H.div H.! A.class_ "message-content" $
      H.toHtml $ messageContent msg

-- | Auth Templates

loginTemplate :: T.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "login"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
            , NavLink "/login" "login" True
            , NavLink "/register" "register" False
            ]
      H.div H.! A.class_ "w-full max-w-sm mx-auto" $ do
        H.form H.! A.class_ "flex flex-col gap-4"
          H.! A.method "POST" H.! A.action "/login" $ do
          H.input H.! A.type_ "email"
                H.! A.name "email"
                H.! A.placeholder "email"
                H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300"
                H.! A.required "required"
          H.input H.! A.type_ "hidden"
                H.! A.name "token"
                H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $
            "login"

registerTemplate :: T.Text -> H.Html
registerTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "register"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
            , NavLink "/login" "login" False
            , NavLink "/register" "register" True
            ]
      H.div H.! A.class_ "w-full max-w-sm mx-auto" $ do
        H.form H.! A.class_ "flex flex-col gap-4"
          H.! A.method "POST" H.! A.action "/register" $ do
          H.input H.! A.type_ "email"
                H.! A.name "email"
                H.! A.placeholder "email"
                H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300"
                H.! A.required "required"
          H.input H.! A.type_ "hidden"
                H.! A.name "token"
                H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $
            "register"

confirmTemplate :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> H.Html
confirmTemplate title warning action token actionText cancelUrl =
  H.docTypeHtml $ H.html $ do
    H.head $ do
      H.title $ H.toHtml title
      H.link H.! A.rel "icon" H.! A.href "data:,"
      H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
      H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
    H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
      H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
        H.form H.! A.method "POST" H.! A.action (H.textValue action) $ do
          H.input H.! A.type_ "hidden"
                H.! A.name "token"
                H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                H.! A.class_ "mb-8 px-8 py-4 bg-error text-error-content rounded-lg hover:bg-error/90" $
            H.toHtml actionText
        H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ H.toHtml title
        H.p H.! A.class_ "mb-8" $ H.toHtml warning
        H.a H.! A.href (H.textValue cancelUrl)
            H.! A.class_ "px-8 py-4 border border-base-300 rounded-lg hover:bg-base-200" $
          "cancel"

accountTemplate :: User -> H.Html
accountTemplate user = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "account"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      navBar [ NavLink "/" "home" False
            , NavLink "/whoami" "whoami" False
            , NavLink "/ans" "answer" False
            , NavLink "/match" "match" False
            ]
      H.div H.! A.class_ "w-full max-w-xl mx-auto" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-4 pb-2 border-b border-base-300" $
          "account information"
        H.p H.! A.class_ "mb-4" $ do
          H.text "Email: "
          H.toHtml $ userEmail user
        H.div H.! A.class_ "mt-12 p-4 border-2 border-error rounded-lg" $ do
          H.h3 H.! A.class_ "text-xl font-bold text-error mb-4" $
            "danger zone"
          H.p H.! A.class_ "mb-2" $ do
            H.text "logout from all devices: "
            H.a H.! A.href "/logout" H.! A.class_ "link text-error hover:opacity-80" $
              "confirm via email"
          H.p H.! A.class_ "mb-2" $ do
            H.text "delete account and all data: "
            H.a H.! A.href "/delete" H.! A.class_ "link text-error hover:opacity-80" $
              "confirm via email"

-- | Message Templates

msgTemplate :: MessageTemplate -> H.Html
msgTemplate template = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title $ H.toHtml $ messageTitle template
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/css/output.css"
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ H.toHtml $ messageHeading template
      H.div $ do
        H.a H.! A.href (H.textValue $ fst $ messageLink template)
            H.! A.class_ "link hover:text-primary transition-colors" $
          H.toHtml $ snd $ messageLink template

emailSentTemplate :: H.Html
emailSentTemplate = msgTemplate MessageTemplate
  { messageTitle = "check your email"
  , messageHeading = "check your email"
  , messageLink = ("/", "home")
  }

dailyLimitTemplate :: T.Text -> H.Html
dailyLimitTemplate timeLeft = msgTemplate MessageTemplate
  { messageTitle = "daily limit"
  , messageHeading = "daily answer limit reached, come back in " <> timeLeft
  , messageLink = ("/whoami", "back to profile")
  }

noMoreQuestionsTemplate :: H.Html
noMoreQuestionsTemplate = msgTemplate MessageTemplate
  { messageTitle = "no more questions"
  , messageHeading = "no more questions"
  , messageLink = ("/whoami", "back to profile")
  }

profileNotYetActive :: Int -> H.Html
profileNotYetActive threshold = msgTemplate MessageTemplate
  { messageTitle = "profile not yet active"
  , messageHeading = T.pack $
      "your profile will be activated after answering "
      ++ show threshold ++ " questions"
  , messageLink = ("/ans", "answer more questions")
  }
matchNotYetActive :: Int -> H.Html
matchNotYetActive threshold = msgTemplate MessageTemplate
  { messageTitle = "match not yet active"
  , messageHeading = T.pack $
      "matching will be activated after answering "
      ++ show threshold ++ " questions"
  , messageLink = ("/ans", "answer more questions")
  }

errorTemplateWithLink :: Int -> T.Text -> (T.Text, T.Text) -> H.Html
errorTemplateWithLink code message link = msgTemplate MessageTemplate
  { messageTitle = message
  , messageHeading = T.pack (show code) <> " - " <> message
  , messageLink = link
  }

errorTemplate :: Int -> T.Text -> H.Html
errorTemplate code message =
  errorTemplateWithLink code message ("/", "go home")

alreadyAnsweredTemplate :: H.Html
alreadyAnsweredTemplate = errorTemplateWithLink
  403 "question already answered" ("/ans", "next question")

invalidTokenTemplate :: H.Html
invalidTokenTemplate = errorTemplateWithLink
  403 "invalid or expired token" ("/ans", "try again")

invalidSubmissionTemplate :: H.Html
invalidSubmissionTemplate = errorTemplateWithLink
  400 "invalid submission format" ("/ans", "try again")

userNotFoundTemplate :: H.Html
userNotFoundTemplate =
  errorTemplateWithLink 404 "user not found" ("/register", "register")

emailExistsTemplate :: H.Html
emailExistsTemplate =
  errorTemplateWithLink 409 "email already registered" ("/login", "login")

notFoundTemplate :: H.Html
notFoundTemplate = errorTemplate 404 "not found"

internalErrorTemplate :: H.Html
internalErrorTemplate = errorTemplate 500 "internal server error"
