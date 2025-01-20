{-# LANGUAGE OverloadedStrings #-}

module Web.Templates
  ( rootTemplate
  , roadmapTemplate
  , profileTemplate
  , ansTemplate
  , existingAnswerTemplate
  , loginTemplate
  , registerTemplate
  , accountTemplate
  , confirmTemplate
  , MessageTemplate(..)
  , msgTemplate
  , emailSentTemplate
  , dailyLimitTemplate
  , noMoreQuestionsTemplate
  , alreadyAnsweredTemplate
  , invalidTokenTemplate
  , invalidSubmissionTemplate
  , notFoundTemplate
  ) where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Word as Word
import qualified Text.Printf as Text

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as I

import Types
import Web.Styles
import Web.Types

-- | Navigation Components

navBar :: [NavLink] -> H.Html
navBar links = H.div H.! A.class_ "nav-bar" $ do
  let separator = H.span H.! A.class_ "nav-separator" $ "///"
      withSeparators [] = return ()
      withSeparators [x] = navLink x
      withSeparators (x:xs) = H.div H.! A.class_ "nav-bar-row" $ do
        navLink x
        separator
        withSeparators xs
      navLink link =
        if linkActive link
          then H.span H.! A.class_ "nav-link active" $
            H.toHtml $ linkText link
          else H.a H.! A.class_ "nav-link"
               H.! A.href (H.textValue $ linkPath link) $
            H.toHtml $ linkText link
  withSeparators links

-- | Core Templates

rootTemplate :: Int -> [Aorb] -> H.Html  -- Add Int parameter for user count
rootTemplate userCount aorbs = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
    H.title "anorby"
    H.style $ H.text rootPageCSS
  H.body $ do
    H.span H.! A.id "top" $ ""
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" True
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" False
             ]
      H.h1 $ do
        H.span H.! A.class_ "underline" $ "a"
        H.text "n"
        H.span H.! A.class_ "underline" $ "or"
        H.span H.! A.class_ "underline" $ "b"
        H.text "y"
      H.div $ do
        H.a H.! A.href "#baseline" $ "the underground census"
    H.span H.! A.id "baseline" $ ""
    H.div H.! A.class_ "frame" $ do
      H.h1 "baseline_100"
      H.h4 "what is this... blah blah blah"
      H.a H.! A.href "/roadmap" $ do
        H.h4 $ H.text $ T.pack $ "# of responses: " ++ show userCount
      H.div H.! A.id "sorter" $ do
        H.div H.! A.class_ "sort-by" $ "sort by:"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-sided" $ "> most one-sided"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-polar" $ "> most polarizing"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-dice" $ "> random"
    H.span H.! A.id "aorbs" $ ""
    H.div H.! A.class_ "frame" H.! A.style "padding-top: 10vh;" $ do
      H.div H.! A.id "by-sided" $ mempty
      H.div H.! A.id "by-dice" $ mempty
      H.div H.! A.id "by-polar" $ mempty
      H.span H.! A.class_ "notch" $ do
        H.a H.! A.href "#baseline" $ "backtobasebasebase"
      publicAorbs aorbs

roadmapTemplate :: Int -> H.Html
roadmapTemplate userCount = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
    H.title "roadmap"
    H.style $ H.text $ combineCSS [baseCSS, navBarCSS, roadmapCSS]
  H.body $ do
    H.span H.! A.id "top" $ ""
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/#baseline" "back" False ]
      H.h1 "roadmap"
      H.div $ do
        H.a H.! A.href "#milestone-100" $ "begin"
    milestoneFrame
      "100" userCount "#milestone-500" False
      (Just "profile sharing")
    milestoneFrame
      "500" userCount "#milestone-1000" False
      (Just "community themes")
    milestoneFrame
      "1000" userCount "#milestone-5000" False
      (Just "friend matching")
    milestoneFrame
      "5000" userCount "#top" True
      (Just "question submissions")

milestoneFrame :: T.Text -> Int -> T.Text -> Bool -> Maybe T.Text -> H.Html
milestoneFrame threshold userCount nextLink bottom maybeProgressText = do
  let users = read (T.unpack threshold) :: Int
      progress :: Int
      progress = min 100 $ round ((fromIntegral userCount /
                                 fromIntegral users) * 100 :: Double)
      progressClass = if userCount >= users
                     then "milestone-complete"
                     else "milestone-incomplete"
      progressText = case maybeProgressText of
        Just txt -> txt
        Nothing -> threshold
  H.span H.! A.id (H.textValue $ "milestone-" <> threshold) $ ""
  H.div H.! A.class_ "frame" $ do
    H.h2 $ do
      H.text $ threshold <> " users"
    H.div H.! A.class_ progressClass $ do
      H.div H.! A.class_ "progress-bar" H.!
        A.style (H.textValue $ "width: " <> T.pack (show progress) <> "%") $ ""
      H.div H.! A.class_ "milestone-marker" $ H.text progressText
    H.div H.! A.class_ "next-milestone" $ do
      H.a H.! A.href (H.textValue nextLink) $
        (if bottom then "top" else  "next")

publicAorbs :: [Aorb] -> H.Html
publicAorbs aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbWithOrders aorbs) $
      \(_, (aorb, orders)) -> do
        H.div H.! A.class_ "aorb" H.!
          A.style (aorbDynamicCSS (zip ["dice", "polar", "sided"] orders)) $ do
            H.div H.! A.class_ "context" $ H.toHtml (aorbCtx aorb)
            let mean = aorbMean aorb
                delta = (abs (mean - 0.5)) * 100
                formatDelta =
                    T.concat ["(+", T.pack (Text.printf "%.2f" delta), ")"]
            if delta < 0.01
              then do
                H.div H.! A.class_ "choice" $ do
                  H.toHtml (aorbA aorb)
                  H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " ...?"
                H.div H.! A.class_ "choice" $ do
                  H.toHtml (aorbB aorb)
                  H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " ...?"
              else if mean > 0.5
              then do
                H.div H.! A.class_ "choice preferred" $ do
                  H.toHtml (aorbB aorb)
                  H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                H.div H.! A.class_ "choice alternative" $
                  H.toHtml (aorbA aorb)
              else do
                H.div H.! A.class_ "choice preferred" $ do
                  H.toHtml (aorbA aorb)
                  H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                H.div H.! A.class_ "choice alternative" $
                  H.toHtml (aorbB aorb)

profileTemplate :: [AorbWithAnswer] -> Maybe AorbID
                -> Maybe T.Text -> Maybe T.Text -> Bool
                -> H.Html
profileTemplate aorbs mAid maybeUuid shareUrl showMinimal =
  H.docTypeHtml $ H.html $ do
    profileHead maybeUuid
    H.body $ do
      profileHeadline maybeUuid $
        if showMinimal
          then profileNotYetActive
          else H.div $ do H.a H.! A.href "#main" $ "begin"
      if showMinimal
         then mempty
         else profileFullView mAid aorbs maybeUuid shareUrl

profileHead :: Maybe T.Text -> H.Html
profileHead maybeUuid = H.head $ do
  H.title $ case maybeUuid of
    Just uuid -> H.text $ "share/" <> uuid
    Nothing -> "whoami"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  H.meta H.! A.name "viewport" H.!
    A.content "width=device-width, initial-scale=1.0"
  H.style $ I.preEscapedText $ profilePageCSS maybeUuid

profileHeadline :: Maybe T.Text -> H.Html -> H.Html
profileHeadline maybeUuid children = do
  H.span H.! A.id "top" $ ""
  H.div H.! A.class_ "frame" $ do
    navBar [ NavLink "/" "home" False
           , NavLink "/whoami" "whoami"
              (case maybeUuid of Just _ -> False ; _ -> True)
           , NavLink "/ans" "answer" False
           ]
    H.h1 $ case maybeUuid of
      Just uuid -> H.text $ "#" <> uuid
      Nothing -> "whoami"
    children

profileNotYetActive :: H.Html
profileNotYetActive = H.div $ do
  H.p "your profile will be activated after answering 10 questions"
  H.div $ do
    H.a H.! A.href "/ans" $ "begin"

profileMainAorb :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileMainAorb mAid aorbs = case mAid of
  Just aid -> do
    H.span H.! A.id "main" $ ""
    H.div H.! A.class_ "frame" $ do
      H.h1 "main"
      H.div H.! A.class_ "aorbs-container" $ do
        mapM_ (\awa -> profileAorb awa mAid Nothing) $
          filter (\awa -> aorbId (aorbData awa) == aid) aorbs
  Nothing -> mempty

profileCommonplaceAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileCommonplaceAorbs mAid aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most commonplace"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_
      (\awa -> profileAorb awa mAid Nothing) (take 3 $ reverse aorbs)

profileControversialAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileControversialAorbs mAid aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most controversial"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_ (\awa -> profileAorb awa mAid Nothing) (take 3 aorbs)

profileAllAnswers :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileAllAnswers mAid aorbs = do
  H.span H.! A.id "all-answers" $ ""
  H.div H.! A.class_ "frame" $ do
    H.h1 "all answers"
    H.div H.! A.id "sorter" $ do
      H.div H.! A.class_ "sort-by" $ "sort by:"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-flake" $ "> most controversial"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-basic" $ "> most commonplace"
  H.div H.! A.class_ "frame" H.! A.style "padding-top: 10vh" $ do
    H.div H.! A.id "by-basic" $ mempty
    H.div H.! A.id "by-flake" $ mempty
    H.span H.! A.class_ "notch" $ do
      H.a H.! A.href "#all-answers" $ "backtoallanswersss"
    profileOrdinaryAorbs mAid aorbs

profileSharer :: Maybe T.Text -> Maybe T.Text -> H.Html
profileSharer maybeUuid shareUrl = case (maybeUuid, shareUrl) of
  (Nothing, Just url) -> H.div H.! A.class_ "frame" $ do
    H.h1 "share"
    H.div $ H.text url
  _ -> mempty

accountManager :: Maybe T.Text -> H.Html
accountManager maybeUuid =
  case maybeUuid of
    Nothing ->
      H.div H.! A.class_ "frame" $ do
        H.div $ do H.a H.! A.href "/account" $ "manage my account"
    _ -> mempty

profileFullView :: Maybe AorbID -> [AorbWithAnswer]
                -> Maybe T.Text -> Maybe T.Text
                -> H.Html
profileFullView mAid aorbs maybeUuid shareUrl = do
  profileMainAorb mAid aorbs
  Monad.when (Maybe.isNothing mAid) $ H.span H.! A.id "main" $ ""
  profileCommonplaceAorbs mAid aorbs
  profileControversialAorbs mAid aorbs
  profileAllAnswers mAid aorbs
  profileSharer maybeUuid shareUrl
  accountManager maybeUuid

profileAorb :: AorbWithAnswer -> Maybe AorbID -> Maybe [Int] -> H.Html
profileAorb awa mFavoriteId mOrders = do
  let aorb = aorbData awa
      ans = userAnswer awa
      aid = aorbId aorb
      isFavorite = maybe False (== aid) mFavoriteId
      percentage =
        case ans of
          AorbAnswer 0 -> 100 * (1 - aorbMean aorb)
          _ -> 100 * aorbMean aorb
      favoriteClass = if isFavorite then " aorb-favorite" else ""
      dynamicStyle = case mOrders of
        Just orders ->
          (H.! A.style (aorbDynamicCSS (zip ["basic", "flake"] orders)))
        Nothing -> id
  dynamicStyle $ H.a H.! A.href (H.toValue $ "/ans/" ++ show aid)
    H.! A.class_ "aorb-clickable" $ do
      H.div H.! A.class_ (H.textValue $ "aorb" <> favoriteClass) $ do
        H.div H.! A.class_ "context" $ H.toHtml $ aorbCtx aorb
        H.div H.! A.class_ (if ans == AorbAnswer 0
                            then "choice selected"
                            else "choice") $ do
          H.toHtml $ aorbA aorb
          Monad.when (ans == AorbAnswer 0) $
            H.span H.! A.class_ "percentage" $
              H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage
        H.div H.! A.class_ (if ans == AorbAnswer 1
                            then "choice selected"
                            else "choice") $ do
          H.toHtml $ aorbB aorb
          Monad.when (ans == AorbAnswer 1) $
            H.span H.! A.class_ "percentage" $
              H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage

profileOrdinaryAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileOrdinaryAorbs mAid aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbWithAnswerWithOrders aorbs) $
      \(_, (awa, orders)) -> profileAorb awa mAid (Just orders)

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
  [ id  -- byDice
  , List.sortOn (\a -> abs (aorbMean a - 0.5))  -- byPolar
  , List.sortOn (Ord.Down . \a -> abs (aorbMean a - 0.5))  -- bySided
  ]

aorbWithAnswerOrderings :: [OrderingFunction AorbWithAnswer]
aorbWithAnswerOrderings =
  [ reverse  -- byBasic
  , id  -- byFlake
  ]

ansTemplate :: Aorb -> Bool -> T.Text -> H.Html
ansTemplate aorb shouldSwap token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText ansPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" True
             ]
      H.div H.! A.class_ "ans-context" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "ans-choices" $
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
          A.class_ "ans-choice" $
          H.toHtml choice

existingAnswerTemplate :: Aorb -> Maybe AorbAnswer -> Bool -> T.Text -> H.Html
existingAnswerTemplate aorb mCurrentAnswer isFavorite token =
  H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText $ combineCSS
      [ baseCSS
      , navBarCSS
      , ansComponentsCSS
      , clickableAorbCSS
      ]
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" True
             ]
      H.div H.! A.class_ "ans-context" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "ans-choices" $ do
        makeExistingChoice aorb token (aorbA aorb) 0
          (mCurrentAnswer == Just (AorbAnswer 0)) isFavorite
        makeExistingChoice aorb token (aorbB aorb) 1
          (mCurrentAnswer == Just (AorbAnswer 1)) isFavorite
      if isFavorite
        then mempty
        else
          H.div H.! A.class_ "favorite-section" $ do
            H.form H.! A.method "POST"
                   H.! A.action
                     (H.toValue $ "/aorb/favorite/" ++ show (aorbId aorb)) $ do
              H.button
                H.! A.type_ "submit"
                H.! A.class_ (H.textValue $ "favorite-button active") $
                  "set as favorite question"
  where
    makeExistingChoice ::
      Aorb -> T.Text -> T.Text -> Word.Word8 -> Bool -> Bool -> H.Html
    makeExistingChoice a t choice value isSelected favorite = do
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
        H.button H.!
          A.type_ "submit" H.!
          A.class_ (H.textValue $ "ans-choice" <>
                    if isSelected
                    then " selected" <> if favorite then " favorite" else ""
                    else "") $
          H.toHtml choice

-- | Auth Templates

loginTemplate :: T.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "login"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ H.text authPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/login" "login" True
             , NavLink "/register" "register" False
             ]
      H.div H.! A.class_ "auth-form" $ do
        H.form H.! A.class_ "auth-form"
          H.! A.method "POST" H.! A.action "/login" $ do
          H.input H.! A.type_ "email"
                 H.! A.name "email"
                 H.! A.placeholder "email"
                 H.! A.class_ "auth-input"
                 H.! A.required "required"
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                  H.! A.class_ "auth-button" $ "login"

registerTemplate :: T.Text -> H.Html
registerTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "register"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ H.text authPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/login" "login" False
             , NavLink "/register" "register" True
             ]
      H.div H.! A.class_ "auth-form" $ do
        H.form H.! A.class_ "auth-form"
          H.! A.method "POST" H.! A.action "/register" $ do
          H.input H.! A.type_ "email"
                 H.! A.name "email"
                 H.! A.placeholder "email"
                 H.! A.class_ "auth-input"
                 H.! A.required "required"
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                  H.! A.class_ "auth-button" $ "register"

accountTemplate :: User -> H.Html
accountTemplate user = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "account"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText accountPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" False
             ]
      H.div H.! A.class_ "account-section" $ do
        H.h2 H.! A.class_ "account-heading" $ "account information"
        H.p $ do
          H.text "Email: "
          H.toHtml $ userEmail user

        H.div H.! A.class_ "danger-zone" $ do
          H.h3 H.! A.class_ "danger-heading" $ "danger zone"
          H.p $ do
            H.text "logout from all devices: "
            H.a H.! A.href "/logout" $ "confirm via email"
          H.p $ do
            H.text "delete account and all data: "
            H.a H.! A.href "/delete" $ "confirm via email"

confirmTemplate :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
                -> H.Html
confirmTemplate title warning action token actionText cancelUrl =
  H.docTypeHtml $ H.html $ do
    H.head $ do
      H.title $ H.toHtml title
      H.link H.! A.rel "icon" H.! A.href "data:,"
      H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
      H.style $ I.preEscapedText accountPageCSS
    H.body $ do
      H.div H.! A.class_ "frame" $ do
        H.form H.! A.method "POST" H.! A.action (H.textValue action) $ do
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "confirm-button" $
            H.toHtml actionText
        H.h1 $ H.toHtml title
        H.p $ H.toHtml warning
        H.a H.! A.href (H.textValue cancelUrl) H.! A.class_ "cancel-button" $
          "cancel"

-- | Message Templates

msgTemplate :: MessageTemplate -> H.Html
msgTemplate template = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title $ H.toHtml $ messageTitle template
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText baseCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      H.h1 $ H.toHtml $ messageHeading template
      H.div $ do
        H.a H.! A.href (H.textValue $ fst $ messageLink template) $
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

alreadyAnsweredTemplate :: H.Html
alreadyAnsweredTemplate = msgTemplate MessageTemplate
  { messageTitle = "already answered"
  , messageHeading = "403 - question already answered"
  , messageLink = ("/ans", "next question")
  }

invalidTokenTemplate :: H.Html
invalidTokenTemplate = msgTemplate MessageTemplate
  { messageTitle = "invalid token"
  , messageHeading = "403 - invalid or expired token"
  , messageLink = ("/ans", "try again")
  }

invalidSubmissionTemplate :: H.Html
invalidSubmissionTemplate = msgTemplate MessageTemplate
  { messageTitle = "invalid submission"
  , messageHeading = "400 - invalid submission format"
  , messageLink = ("/ans", "try again")
  }

notFoundTemplate :: H.Html
notFoundTemplate = msgTemplate MessageTemplate
  { messageTitle = "error"
  , messageHeading = "404 - not found"
  , messageLink = ("/", "go home")
  }
