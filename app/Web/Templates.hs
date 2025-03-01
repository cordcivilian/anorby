{-# LANGUAGE OverloadedStrings #-}

module Web.Templates where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Word as Word
import qualified Text.HTML.SanitizeXSS as XSS
import qualified Text.Printf as Text

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as I

import Types
import Web.Types
import Utils.Time
import Utils.Config
import Utils.MatchState

navBar :: Maybe T.Text -> H.Html
navBar maybeSubtitle = do
  H.div H.! A.class_ "ds-navbar mb-12" $ do
    H.div H.! A.class_ "ds-navbar-start" $ do
      H.a H.! A.href "/clash" $ H.div H.! A.class_ "ds-btn" $ "clash"
    case maybeSubtitle of
      Just subtitle -> do
        H.div H.! A.class_ "ds-tooltip ds-tooltip-open ds-tooltip-bottom ds-tooltip-secondary" $ do
          H.div H.! A.class_ "ds-tooltip-content" $ H.text subtitle
          H.div $ H.a H.! A.class_ "ds-navbar-center md:text-3xl text-xl" H.! A.href "/" $ anorbyTitle
      Nothing -> do
        H.div $ H.a H.! A.class_ "ds-navbar-center md:text-3xl text-xl" H.! A.href "/" $ anorbyTitle
    H.div H.! A.class_ "ds-navbar-end ds-dropdown ds-dropdown-end ds-dropdown-bottom" $ do
      H.div H.! A.tabindex "0" H.! A.role "button" H.! A.class_ "ds-btn transition-all" $ "you"
      H.ul H.! A.tabindex "0" H.! A.class_ "ds-menu ds-dropdown-content" $ do
        H.li H.! A.class_ "hover:text-primary transition-all" $ H.a H.! A.href "/whoami" $ "whoami"
        H.li H.! A.class_ "hover:text-primary transition-all" $ H.a H.! A.href "/account" $ "account"

anorbyTitle :: H.Html
anorbyTitle = do
  H.span H.! A.class_ "border-b-3 border-primary inline-block leading-[0.85] mx-[3px]" $ "a"
  H.span H.! A.class_ "border-b-3 border-transparent inline-block" $ "n"
  H.span H.! A.class_ "border-b-3 border-secondary inline-block leading-[0.85] mx-[3px]" $ "or"
  H.span H.! A.class_ "border-b-3 border-primary inline-block leading-[0.85] mx-[3px]" $ "b"
  H.span H.! A.class_ "border-b-3 border-transparent inline-block" $ "y"

pageHead :: T.Text -> H.Html -> H.Html
pageHead title more = H.head $ do
  H.title $ H.toHtml title
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
  H.meta H.! A.charset "utf-8"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  H.link H.! A.rel "stylesheet" H.! A.href "/styles/output.css"
  more

adminTemplate :: [Aorb] -> H.Html
adminTemplate aorbs = H.docTypeHtml $ H.html $ do
  pageHead "admin" mempty
  H.body $ do
    H.div $ do
      navBar (Just "admin")
      H.div H.! A.class_ "ds-card max-w-4xl mx-auto mb-8" $ do
        H.form H.! A.method "POST" H.! A.action "/admin/aorb/add" H.! A.class_ "ds-card-body space-y-4" $ do
          H.h2 H.! A.class_ "ds-card-title" $ "new question"
          H.textarea H.! A.placeholder "context" H.! A.id "context" H.! A.name "context" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ ""
          H.textarea H.! A.placeholder "subtext" H.! A.id "subtext" H.! A.name "subtext" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ ""
          H.textarea H.! A.placeholder "aaaaa" H.! A.id "option_a" H.! A.name "option_a" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ ""
          H.textarea H.! A.placeholder "bbbbb" H.! A.id "option_b" H.! A.name "option_b" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ ""
          H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-primary ds-btn-soft ds-btn-block" $ "add"

      H.div H.! A.class_ "ds-collapse ds-collapse-arrow border border-base-400 max-w-4xl mx-auto" $ do
        H.input H.! A.type_ "checkbox"
        H.div H.! A.class_ "ds-collapse-title font-black" $ "edit questions"
        H.div H.! A.class_ "ds-collapse-content" $ mapM_ renderAorbAdmin aorbs

renderAorbAdmin :: Aorb -> H.Html
renderAorbAdmin aorb =
  H.div H.! A.class_ "ds-card" $ do
    H.div H.! A.class_ "ds-card-body" $ do
      H.div H.! A.class_ "ds-card-title" $ do
        H.div $ H.toHtml $ "[" <> T.pack (show $ aorbId aorb) <> "]"
        H.div $ H.toHtml $ aorbCtx aorb
      H.div H.! A.class_ "italic" $ H.toHtml $ aorbStx aorb
      H.div H.! A.class_ "text-sm text-base-content/60" $ H.toHtml $ "mean: " <> T.pack (Text.printf "%.4f" $ aorbMean aorb)
      H.div H.! A.class_ "p-4 mb-4 mt-4" $ do
        H.div $ H.toHtml $ aorbA aorb
        H.div H.! A.class_ "ds-divider" $ "OR"
        H.div $ H.toHtml $ aorbB aorb

      H.div H.! A.class_ "grid gap-4" $ do
        H.form H.! A.method "GET" H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/edit") $ do
          H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-soft ds-btn-warning ds-btn-block" $ "edit"
        let dialogId = "delete-dialog-" <> T.pack (show $ aorbId aorb)
        H.button H.! A.type_ "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-error ds-btn-block" H.! A.onclick (H.textValue $ "document.getElementById('" <> dialogId <> "').showModal()") $ "delete"

        H.dialog H.! A.id (H.textValue dialogId) H.! A.class_ "ds-modal" $ do
          H.div H.! A.class_ "ds-modal-box" $ do
            H.h3 H.! A.class_ "text-2xl font-black mb-4" $ "confirm"
            H.div H.! A.class_ "grid gap-2" $ do
              H.form H.! A.method "POST" H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/delete") H.! A.class_ "" $ do
                H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-error ds-btn-soft ds-btn-block" $ "delete"
              H.button H.! A.type_ "button" H.! A.class_ "ds-btn ds-btn-neutral ds-btn-soft ds-btn-block" H.! A.onclick (H.textValue $ "document.getElementById('" <> dialogId <> "').close()") $ "cancel"

editAorbTemplate :: Aorb -> H.Html
editAorbTemplate aorb = H.docTypeHtml $ H.html $ do
  pageHead "edit question" mempty
  H.body $ H.div $ do
      navBar (Just "admin: edit")
      H.div H.! A.class_ "ds-card max-w-4xl mx-auto mb-8" $ do
        H.form H.! A.method "POST" H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/edit") H.! A.class_ "ds-card-body space-y-4" $ do
          H.h2 H.! A.class_ "ds-card-title" $ H.toHtml $ "edit question #" <> T.pack (show $ aorbId aorb)
          H.textarea H.! A.placeholder "context" H.! A.id "context" H.! A.name "context" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ H.toHtml (aorbCtx aorb)
          H.textarea H.! A.placeholder "subtext" H.! A.id "subtext" H.! A.name "subtext" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ H.toHtml (aorbStx aorb)
          H.textarea H.! A.placeholder "aaaaa" H.! A.id "option_a" H.! A.name "option_a" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ H.toHtml (aorbA aorb)
          H.textarea H.! A.placeholder "bbbbb" H.! A.id "option_b" H.! A.name "option_b" H.! A.autocomplete "off" H.! A.class_ "w-full pt-4 pb-4 border border-base-400 resize-none field-sizing-content" H.! A.required "required" $ H.toHtml (aorbB aorb)
          H.div H.! A.class_ "grid gap-4" $ do
            H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-primary ds-btn-soft ds-btn-block" $ "save"
            H.a H.! A.href "/admin" H.! A.class_ "ds-btn ds-btn-neutral ds-btn-soft ds-btn-block" $ "cancel"

data ShowAorbMode
  = Population Aorb [Int]
  | Individual AorbWithAnswer [Int] (Maybe AorbID) (Maybe T.Text)

aorbDynamicCSS :: [(String, Int)] -> H.AttributeValue
aorbDynamicCSS orderPairs =
  H.preEscapedTextValue $ T.intercalate "; " [ "--order-" <> T.pack name <> ": " <> T.pack (show order) | (name, order) <- orderPairs ]

orderAorbs :: (Eq a) => [a] -> [OrderingFunction a] -> [(a, [Int])]
orderAorbs as orderingFuncs =
  let orderedLists = map (\f -> f as) orderingFuncs
      lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
  in [ (a, map (\orderedList -> lookupOrder orderedList a) orderedLists) | a <- as ]

rootTemplate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> MatchStatus -> [Aorb] -> H.Html
rootTemplate totalQuestions totalAnswers todayAnswers activeUsers newUsers newQuestions matchingEnrolled matchStatus aorbs =
  H.docTypeHtml $ H.html $ do
  pageHead "anorby"
    ( H.style $ H.preEscapedText $ T.unlines
      [ ":root:has(#remember-diced:checked) .aorb { order: var(--order-diced) !important; }"
      , ":root:has(#remember-sided:checked) .aorb { order: var(--order-sided) !important; }"
      , ":root:has(#remember-split:checked) .aorb { order: var(--order-split) !important; }"
      ]
    )
  H.body $ do
    H.div $ do
      H.div H.! A.id "top" $ mempty
      navBar Nothing
      H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
        rootStats totalQuestions totalAnswers todayAnswers activeUsers newUsers newQuestions matchingEnrolled matchStatus
        H.fieldset H.! A.class_ "flex flex-col mb-2 items-center gap-4" $ do
          H.div "sorter:"
          H.div H.! A.class_ "flex gap-2 md:gap-8" $ do
            H.input H.! A.id "remember-diced" H.! A.name "root-sort" H.! A.type_ "radio" H.! A.checked "checked" H.! A.class_ "ds-btn ds-btn-neutral" H.! I.customAttribute "aria-label" "random"
            H.input H.! A.id "remember-sided" H.! A.name "root-sort" H.! A.type_ "radio" H.! A.class_ "ds-btn ds-btn-neutral" H.! I.customAttribute "aria-label" "unanimity"
            H.input H.! A.id "remember-split" H.! A.name "root-sort" H.! A.type_ "radio" H.! A.class_ "ds-btn ds-btn-neutral" H.! I.customAttribute "aria-label" "deadlocks"
        H.div H.! A.class_ "grid gap-8 justify-items-center px-4 py-4 pt-0 pb-0 w-full" $ do
          H.div H.! A.class_ "ds-toast ds-toast-end" H.! A.style "z-index: 10000" $ do
            H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-primary" H.! A.href "#top" $ "back to top"
          mapM_ (showAorb . uncurry Population) (orderAorbs aorbs orderFuncs)
  where
    orderFuncs =
      [ id
      , List.sortOn (Ord.Down . \a -> abs (aorbMean a - 0.5))
      , List.sortOn (\a -> abs (aorbMean a - 0.5))
      ]

rootStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> MatchStatus -> H.Html
rootStats totalQuestions totalAnswers todayAnswers activeUsers newUsers newQuestions matchingEnrolled matchStatus =
  H.div H.! A.class_ "ds-stats ds-stats-vertical md:ds-stats-horizontal grid grid-cols-2 md:grid-cols-4 shadow" $ do
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "population"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show activeUsers
      H.div H.! A.class_ "ds-stat-desc" $ H.toHtml $ ("+" <> show newUsers <> " this week")
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "questions"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show totalQuestions
      H.div H.! A.class_ "ds-stat-desc" $ H.toHtml $ ("+" <> show newQuestions <> " this week")
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "answers"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show totalAnswers
      H.div H.! A.class_ "ds-stat-desc" $ H.toHtml $ ("+" <> show todayAnswers <> " today")
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "clashes"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show matchingEnrolled
      H.div H.! A.class_ "ds-stat-desc" $ case matchStatus of
        NotStarted -> "enrolled for matching"
        InProgress -> "matching in progress"
        Completed -> "matched today"
        Failed _ -> "matching failed"

showAorb :: ShowAorbMode -> H.Html
showAorb mode =
  case clickable of
    True -> do
        H.a H.! aorbClass H.! aorbStyle H.! A.href (H.toValue $ "/ans/" ++ show aid) $ do
          H.div H.! A.class_ "ds-card-body" $ do
            H.div H.! A.class_ "ds-card-title italic mb-4" $ H.toHtml $ context
            mkChoice True mode
            H.div H.! A.class_ "ds-divider" $ "OR"
            mkChoice False mode
    False -> do
      H.div H.! aorbClass H.! aorbStyle $ do
        H.div H.! A.class_ "ds-card-body" $ do
          H.div H.! A.class_ "ds-card-title italic mb-4" $ H.toHtml $ context
          mkChoice True mode
          H.div H.! A.class_ "ds-divider" $ "OR"
          mkChoice False mode
  where
    aid = case mode of
      Population a _ -> aorbId a
      Individual awa _ _ _ -> aorbId $ aorbData awa
    clickable = case mode of
      Population _ _ -> False
      Individual _ _ _ maybeUuid -> case maybeUuid of
        Just _ -> False
        Nothing -> True
    main = case mode of
      Population _ _ -> False
      Individual awa _ maybeMain _ -> case maybeMain of
        Just mainAorbId -> aorbId (aorbData awa) == mainAorbId
        Nothing -> False
    aorbClass = case (main, clickable) of
      (True, True) -> A.class_ "aorb w-full max-w-3xl ds-card ds-card-border border-3 rounded-4xl border-warning hover:-translate-y-1 hover:bg-base-200 transition-all"
      (True, False) -> A.class_ "aorb w-full max-w-3xl ds-card ds-card-border border-3 rounded-4xl border-primary"
      (False, True) -> A.class_ "aorb w-full max-w-3xl ds-card ds-card-border border-3 rounded-4xl hover:-translate-y-1 hover:bg-base-200 transition-all"
      (False, False) -> A.class_ "aorb w-full max-w-3xl ds-card ds-card-border border-3 rounded-4xl"
    aorbStyle = case mode of
      Population _ orders -> A.style (aorbDynamicCSS (zip ["diced", "sided", "split"] orders))
      Individual _ orders _ _ -> A.style (aorbDynamicCSS (zip ["basic", "flake"] orders))
    context = case mode of
      Population a _ -> aorbCtx a
      Individual awa _ _ _ -> aorbCtx $ aorbData awa

mkChoice :: Bool -> ShowAorbMode -> H.Html
mkChoice isTop mode =
  case mode of
    Population aorb _ ->
      let
        mean = aorbMean aorb
        isClear = mean > 0.51 || mean < 0.49
        (choice, popularity) = if isTop == (mean > 0.5)
          then (aorbA aorb, mean)
          else (aorbB aorb, 1 - mean)
        showFn = case (isTop, isClear) of
          (True, True)   -> showChoice (A.class_ "text-lg") (Just $ A.class_ "ml-2 text-warning")
          (False, True)  -> showChoice (A.class_ "text-sm") (Nothing)
          (_, False)     -> showChoice (mempty) (Just $ A.class_ "ml-2")
      in
        showFn choice popularity
    Individual awa _ _ maybeUuid ->
      let
        ans = userAnswer awa
        isShared = maybeUuid /= Nothing
        (choice, popularity) = if isTop == (ans == AorbAnswer 0)
          then (aorbA $ aorbData awa, aorbMean $ aorbData awa)
          else (aorbB $ aorbData awa, 1 - aorbMean (aorbData awa))
        showFn = case (isTop, isShared) of
          (True, False)  -> showChoice (A.class_ "text-lg text-primary") (Just $ A.class_ "ml-2 text-primary")
          (True, True)   -> showChoice (A.class_ "text-lg text-warning") (Just $ A.class_ "ml-2 text-warning")
          (False, _)     -> showChoice (A.class_ "text-sm") (Nothing)
      in
        showFn choice popularity

showChoice :: H.Attribute -> Maybe H.Attribute -> T.Text -> Double -> H.Html
showChoice choiceClass popularityClass choice popularity =
  H.div H.! choiceClass $ do
    H.toHtml choice
    case popularityClass of
      Just pc -> H.span H.! pc $ H.toHtml $
        T.concat ["(", T.pack (Text.printf "%.2f" $ popularity * 100), "%)"]
      Nothing -> mempty

profileTemplate :: [AorbWithAnswer] -> Maybe AorbID -> Maybe T.Text -> Maybe T.Text -> H.Html
profileTemplate awas maybeMain maybeUuid shareUrl = H.docTypeHtml $ H.html $ do
  pageHead
    ( case maybeUuid of
        Just uuid -> "share/" <> uuid
        Nothing -> "whoami"
    )
    ( H.style $ H.preEscapedText $ T.unlines
      [ ":root:has(#remember-flake:checked) .aorb { order: var(--order-flake) !important; }"
      , ":root:has(#remember-basic:checked) .aorb { order: var(--order-basic) !important; }"
      ]
    )
  H.body $ do
    H.div H.! A.id "top" $ mempty
    navBar $ case maybeUuid of
      Just uuid -> Just $ "#" <> uuid
      Nothing -> Just "whoami"
    H.div H.! A.class_ "grid gap-8 place-items-center w-full p-4" $ do
      case (maybeUuid, shareUrl) of
        (Nothing, Just url) -> H.div $ H.div $ H.text url
        _ -> mempty
      H.div H.! A.class_ "ds-tabs ds-tabs-lift justify-center" $ do
        H.input H.! A.class_ "ds-tab mb-4" H.! A.type_ "radio" H.! A.name "profile-tabs" H.! I.customAttribute "aria-label" "main" H.! A.checked "checked"
        H.div H.! A.class_ "ds-tab-content" $ do
          H.div H.! A.class_ "grid gap-8 justify-items-center" $ do
            case (maybeMain, maybeUuid) of
              (Just aid, _) -> do
                Monad.forM_ (orderAorbs (filter ((== aid) . aorbId . aorbData) awas) orderFuncs) $ \(aorb, orders) ->
                  let aorbMode = Individual aorb orders maybeMain maybeUuid
                  in showAorb aorbMode
              (Nothing, Just _) -> mempty
              (Nothing, Nothing) -> do
                H.p "you haven't selected your main question yet"
                H.p "pick from the answers below"

        H.input H.! A.class_ "ds-tab mb-4" H.! A.type_ "radio" H.! A.name "profile-tabs" H.! I.customAttribute "aria-label" "commonplace"
        H.div H.! A.class_ "ds-tab-content" $ do
          H.div H.! A.class_ "grid gap-8 justify-items-center" $ do
            Monad.forM_ (orderAorbs (take 3 $ reverse awas) orderFuncs) $ \(aorb, orders) ->
              let aorbMode = Individual aorb orders maybeMain maybeUuid
              in showAorb aorbMode

        H.input H.! A.class_ "ds-tab mb-4" H.! A.type_ "radio" H.! A.name "profile-tabs" H.! I.customAttribute "aria-label" "controversial"
        H.div H.! A.class_ "ds-tab-content" $ do
          H.div H.! A.class_ "grid gap-8 justify-items-center" $ do
            Monad.forM_ (orderAorbs (take 3 awas) orderFuncs) $ \(aorb, orders) ->
              let aorbMode = Individual aorb orders maybeMain maybeUuid
              in showAorb aorbMode

        H.input H.! A.class_ "ds-tab mb-4 " H.! A.type_ "radio" H.! A.name "profile-tabs" H.! I.customAttribute "aria-label" "all"
        H.div H.! A.class_ "ds-tab-content" $ do
          H.div H.! A.class_ "ds-toast ds-toast-end" H.! A.style "z-index: 10000" $ do
            H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-primary" H.! A.href "#top" $ "back to top"
          H.fieldset H.! A.class_ "flex flex-col mb-8 items-center gap-4" $ do
            H.div "sorter:"
            H.div H.! A.class_ "flex gap-8" $ do
              H.input H.! A.id "remember-flake" H.! A.name "whoami-sort" H.! A.type_ "radio" H.! A.checked "checked" H.! A.class_ "ds-btn ds-btn-neutral" H.! I.customAttribute "aria-label" "contrarian"
              H.input H.! A.id "remember-basic" H.! A.name "whoami-sort" H.! A.type_ "radio" H.! A.class_ "ds-btn ds-btn-neutral" H.! I.customAttribute "aria-label" "conformist"
          H.div H.! A.class_ "grid gap-8 justify-items-center" $ do
            Monad.forM_ (orderAorbs awas orderFuncs) $ \(aorb, orders) ->
              let aorbMode = Individual aorb orders maybeMain maybeUuid
              in showAorb aorbMode
  where orderFuncs = [id, reverse]

answerTemplate :: Aorb -> Bool -> T.Text -> H.Html
answerTemplate aorb shouldSwap token = H.docTypeHtml $ H.html $ do
  pageHead "answer" mempty
  H.body $ do
    H.div H.! A.class_ "flex flex-col h-screen" $ do
      navBar Nothing
      H.div H.! A.class_ "flex-1 h-auto justify-items-center" $ do
        H.div H.! A.class_ "ds-card w-screen max-w-4xl" $ do
          H.div H.! A.class_ "flex-1 ds-card-body" $ do
            H.div H.! A.class_ "ds-card-title italic mb-8 justify-center" $ H.toHtml (aorbCtx aorb)
            H.div $ do
              let (firstChoice, firstValue, secondChoice, secondValue) =
                    if shouldSwap
                      then (aorbB aorb, 1, aorbA aorb, 0)
                      else (aorbA aorb, 0, aorbB aorb, 1)
              makeChoice aorb token firstChoice firstValue
              H.div H.! A.class_ "ds-divider" $ "OR"
              makeChoice aorb token secondChoice secondValue
  where
    makeChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> H.Html
    makeChoice a t choice value = do
      H.form  H.! A.method "POST" H.! A.action "/ans/submit" $ do
        H.input H.! A.type_ "hidden" H.! A.name "aorb_id" H.! A.value (H.toValue $ show $ aorbId a)
        H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue t)
        H.input H.! A.type_ "hidden" H.! A.name "choice" H.! A.value (H.toValue $ show value)
        H.button H.! A.type_ "submit" H.! A.class_ "w-full min-h-[160px] p-8 text-center border rounded-lg transition-all cursor-pointer font-inherit border-base-300 hover:bg-base-200 hover:border-base-300" $ H.toHtml choice

existingAnswerTemplate :: Aorb -> Maybe AorbAnswer -> Bool -> T.Text -> H.Html
existingAnswerTemplate aorb mCurrentAnswer isFavorite token = H.docTypeHtml $ H.html $ do
  pageHead "answer (edit)" mempty
  H.body $ do
    H.div H.! A.class_ "flex flex-col h-screen" $ do
      navBar Nothing
      H.div H.! A.class_ "flex-1 h-auto justify-items-center" $ do
        H.div H.! A.class_ "ds-card w-screen max-w-4xl" $ do
          H.div H.! A.class_ "flex-1 ds-card-body" $ do
            H.div H.! A.class_ "ds-card-title italic mb-8 justify-center" $ H.toHtml (aorbCtx aorb)
            H.div H.! A.class_ "" $ do
              makeExistingChoice aorb token (aorbA aorb) 0 (mCurrentAnswer == Just (AorbAnswer 0)) isFavorite
              H.div H.! A.class_ "ds-divider" $ "OR"
              makeExistingChoice aorb token (aorbB aorb) 1 (mCurrentAnswer == Just (AorbAnswer 1)) isFavorite
          H.div H.! A.class_ "ds-card-actions justify-center grid gap-6" $ do
            if isFavorite
              then mempty
              else
                H.form H.! A.method "POST" H.! A.action (H.toValue $ "/aorb/favorite/" ++ show (aorbId aorb)) $ do
                  H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-warning ds-btn-soft" $ "set as favorite question"
            H.a H.! A.href "/whoami" H.! A.class_ "ds-btn ds-btn-primary ds-btn-soft" $ "back"
  where
    makeExistingChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> Bool -> Bool -> H.Html
    makeExistingChoice a t choice value isSelected favorite = do
      let choiceClass= if isSelected
            then if favorite
              then A.class_ "w-full min-h-[160px] p-8 text-center border rounded-lg transition-all cursor-pointer font-inherit border-warning text-warning bg-warning/5 hover:bg-warning/10"
              else A.class_ "w-full min-h-[160px] p-8 text-center border rounded-lg transition-all cursor-pointer font-inherit border-primary text-primary bg-primary/5 hover:bg-primary/10"
            else A.class_ "w-full min-h-[160px] p-8 text-center border rounded-lg transition-all cursor-pointer font-inherit border-base-300 hover:bg-base-200"
      H.form H.! A.method "POST" H.! A.action "/ans/edit" $ do
        H.input H.! A.type_ "hidden" H.! A.name "aorb_id" H.! A.value (H.toValue $ show $ aorbId a)
        H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue t)
        H.input H.! A.type_ "hidden" H.! A.name "choice" H.! A.value (H.toValue $ show value)
        H.button H.! A.type_ "submit" H.! choiceClass $ H.toHtml choice

data TimeState = BeforeCutoff | BeforeRelease | AfterRelease
data EnrolState = Enrolled | NotEnrolled
data MatchPhase = ShowingStatus | ShowingEnrollment | ShowingTiming | ShowingResults

matchTemplate ::
  Config -> User -> Bool -> Int
  -> Maybe POSIXTime.POSIXTime -> Maybe POSIXTime.POSIXTime -> POSIXTime.POSIXTime
  -> Maybe (Match, Double) -> MatchStatus -> [(Match, Double, Int)]
  -> H.Html
matchTemplate
  config user isEnrolled enrolledCount
  maybeCutoffTime maybeReleaseTime now
  maybeMatchScore matchStatus pastMatches
  =
  let
    timeState = case (maybeCutoffTime, maybeReleaseTime) of
      (Just ct, Just rt) | now < ct -> BeforeCutoff | now < rt -> BeforeRelease | otherwise -> AfterRelease
      _ -> BeforeCutoff
    enrolState = if isEnrolled then Enrolled else NotEnrolled
    timeUntilCutoff = maybe "soon" (formatTimeUntil now) maybeCutoffTime
    timeUntilRelease = maybe "soon" (formatTimeUntil now) maybeReleaseTime
  in H.docTypeHtml $ H.html $ do
    pageHead "clash" mempty
    H.body $
      H.div $ do
        navBar $ Just "clash"

        H.div H.! A.class_ "p-4" $ do
          H.div H.! A.class_ "ds-collapse ds-collapse-arrow mb-4 grid max-w-xl mx-auto bg-base-100 border-2 border-base-300" $ do
            H.input H.! A.type_ "checkbox"
            H.div H.! A.class_ "ds-collapse-title text-center font-black" $ case userAssoc user of
              Just scheme -> styleScheme scheme False
              Nothing -> "choose your clashes"
            H.div H.! A.class_ "ds-collapse-content grid gap-2 w-full" $ do
              schemeCard PPPod (userAssoc user)
              schemeCard Swing (userAssoc user)
              schemeCard Bipolar (userAssoc user)

        case timeState of
          AfterRelease -> mempty
          _ -> do
            H.div H.! A.class_ "w-full max-w-2xl mx-auto p-4 grid justify-stretch align-center" $ do
              H.div H.! A.class_ "ds-stats ds-stats-vertical md:ds-stats-horizontal shadow" $ do
                H.div H.! A.class_ "ds-stat" $ do
                  H.div H.! A.class_ "ds-stat-title" $ "today's clash pool"
                  H.div H.! A.class_ "ds-stat-value" $ H.text $ T.pack $ show enrolledCount
                  H.div H.! A.class_ "ds-stat-actions" $ do
                    case (timeState, enrolState, matchStatus) of
                      (BeforeRelease, Enrolled, Completed) -> H.div H.! A.class_ "ds-badge ds-badge-success" $ H.text "status: matched"
                      (BeforeRelease, Enrolled, _) -> H.div H.! A.class_ "ds-badge ds-badge-success" $ H.text "status: enrolled"
                      (BeforeCutoff, Enrolled, _) -> H.div H.! A.class_ "ds-badge ds-badge-info" $ H.text "status: enrolled"
                      (BeforeCutoff, NotEnrolled, _) -> H.a H.! A.class_ "ds-badge ds-badge-warning" H.! A.href "/ans" $ "join"
                      (BeforeRelease, NotEnrolled, _) -> H.div H.! A.class_ "ds-badge ds-badge-error" $ H.text "status: missed"
                case (timeState, enrolState) of
                  (BeforeRelease, NotEnrolled) -> mempty
                  (BeforeCutoff, NotEnrolled) -> renderTimeDisplay "cutoff in ..." timeUntilCutoff (matchCutoffTime config)
                  (_, Enrolled) -> renderTimeDisplay "reveal in ..." timeUntilRelease (matchReleaseTime config)

        H.div H.! A.class_ "grid gap-8 max-w-2xl mx-auto p-4" $ do
          if null pastMatches
            then H.h3 H.! A.class_ "w-full border-2 border-base-300 p-6 rounded-lg" $ do
              H.div "1. join the clash pool before the daily cutoff"
              H.div "2. check in on your clash-of-the-day at the daily reveal"
              H.div "3. repeat"
            else do
              case (timeState, maybeMatchScore) of
                (AfterRelease, Nothing) -> H.div H.! A.class_ "w-full border-2 border-base-300 p-6 rounded-lg text-center" $ H.text "you were not in the clash pool today"
                (AfterRelease, Just (match, score)) -> matchCard (floor now) match score 0
                _ -> mempty
              mapM_ (\(m, s, u) -> matchCard (floor now) m s u) pastMatches

schemeCard :: AssociationScheme -> Maybe AssociationScheme -> H.Html
schemeCard scheme currentScheme =
  let isSelected = currentScheme == Just scheme
      schemeClass = if isSelected
        then A.class_ "ds-btn w-full p-4 rounded-lg cursor-pointer transition-all bg-transparent border-warning text-warning border-2"
        else A.class_ "ds-btn w-full p-4 rounded-lg cursor-pointer transition-all bg-transparent border border-base-300 hover:bg-base-200"
  in H.form H.! A.method "POST" H.! A.action "/clash/type" $ do
    H.button H.! A.type_ "submit" H.! A.name "assoc" H.! A.value (H.toValue $ show scheme) H.! schemeClass $ do
      styleScheme scheme True

styleScheme :: AssociationScheme -> Bool -> H.Html
styleScheme scheme showDescription =
  let (schemeNameClass, schemeDesc, schemeDescClass)  = case scheme of
        PPPod -> (A.class_ "font-sans italic", "validate me", A.class_ "ds-tooltip ds-tooltip-open ds-tooltip-right")
        Swing -> (A.class_ "font-serif", "lol who cares", A.class_ "ds-tooltip ds-tooltip-open ds-tooltip-left")
        Bipolar -> (A.class_ "font-mono", "they're wrong", A.class_ "ds-tooltip ds-tooltip-open ds-tooltip-right")
  in if showDescription
        then H.div H.! schemeDescClass H.! H.dataAttribute "tip" schemeDesc $
          H.div H.! schemeNameClass $ H.toHtml $ show scheme
        else H.div H.! schemeNameClass $ H.toHtml $ show scheme

renderTimeDisplay :: T.Text -> T.Text -> T.Text -> H.Html
renderTimeDisplay label timeLeft timeStr =
  H.div H.! A.class_ "ds-stat" $ do
    H.div H.! A.class_ "ds-stat-title" $ H.text label
    H.span H.! A.class_ "ds-stat-value" $ H.text $ timeLeft
    H.span H.! A.class_ "ds-stat-actions ds-badge ds-badge-secondary" $ H.text $ timeStr <> " UTC"

matchCard :: Integer -> Match -> Double -> Int -> H.Html
matchCard currentTimestamp match score unreadCount =
  H.a H.! A.class_ "w-full border-2 border-base-300 p-6 rounded-lg cursor-pointer transition-all hover:bg-base-200 hover:-translate-y-1 text-inherit"
    H.! A.href (H.textValue $ "/clash/t-" <> formatRelativeMatchDate currentTimestamp (matchTimestamp match)) $ do
    H.div H.! A.class_ "text-sm text-base-content/70 mb-2" $ H.toHtml $ formatSemiAbsoluteMatchDate currentTimestamp (matchTimestamp match) unreadCount
    H.div H.! A.class_ "text-info font-bold" $ H.toHtml $ formatSimilarityScore score

formatSimilarityScore :: Double -> T.Text
formatSimilarityScore s = T.pack $ Text.printf "🤝 %.0f%%" ((s + 1) * 50)

getRelativeMatchDate :: Integer -> Integer -> Integer
getRelativeMatchDate currentTimestamp' matchTimestamp' =
  let secondsPerDay = 86400 :: Double
      currentDay = floor ((fromIntegral currentTimestamp' :: Double) / secondsPerDay) :: Integer
      matchDay = floor ((fromIntegral matchTimestamp' :: Double) / secondsPerDay) :: Integer
  in currentDay - matchDay

formatRelativeMatchDate :: Integer -> Integer -> T.Text
formatRelativeMatchDate currentTimestamp' matchTimestamp' =
  T.pack $ show $ getRelativeMatchDate currentTimestamp' matchTimestamp'

formatAbsoluteMatchDate :: Integer -> T.Text
formatAbsoluteMatchDate timestamp =
  T.pack $ DateTimeFormat.formatTime DateTimeFormat.defaultTimeLocale "%Y-%m-%d"
    (POSIXTime.posixSecondsToUTCTime $ fromIntegral timestamp)

formatSemiAbsoluteMatchDate :: Integer -> Integer -> Int -> T.Text
formatSemiAbsoluteMatchDate currentTimestamp' matchTimestamp' unread =
  let dayDelta = getRelativeMatchDate currentTimestamp' matchTimestamp'
      baseText = case dayDelta of
        0 -> "today"
        1 -> "yesterday"
        _ -> T.toLower . T.pack $ DateTimeFormat.formatTime DateTimeFormat.defaultTimeLocale "%A, %Y-%m-%d"
          (POSIXTime.posixSecondsToUTCTime $ fromIntegral matchTimestamp')
  in if unread > 0
        then "[" <> T.pack (show unread) <> "] " <> baseText
        else baseText

matchProfileTemplate :: Config -> Integer -> UserID -> UserID -> Int -> MatchView -> [Message] -> H.Html
matchProfileTemplate config days mainUserId _ matchId view messages = H.docTypeHtml $ H.html $ do
  pageHead (T.pack $ "clash #" ++ show days) mempty
  H.body $ do
    H.div $ do
      navBar (Just (formatAbsoluteMatchDate (viewTimestamp view)))

      H.div H.! A.class_ "w-full max-w-2xl mx-auto p-4 grid justify-stretch align-center" $ do
        H.div H.! A.class_ "ds-stats ds-stats-vertical md:ds-stats-horizontal shadow" $ do
          H.div H.! A.class_ "ds-stat" $ do
            H.div H.! A.class_ "ds-stat-title" $ "they answered"
            H.div H.! A.class_ "ds-stat-value text-warning" $ H.toHtml $ show (viewTargetTotalAnswers view)
          H.div H.! A.class_ "ds-stat" $ do
            H.div H.! A.class_ "ds-stat-title" $ "common ground"
            H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ T.pack $ Text.printf "%.0f%%" (viewAgreementRate view)
          H.div H.! A.class_ "ds-stat" $ do
            H.div H.! A.class_ "ds-stat-title" $ "you answered"
            H.div H.! A.class_ "ds-stat-value text-primary" $ H.toHtml $ show (viewYourTotalAnswers view)

      H.div H.! A.class_ "grid gap-4 mt-4 p-4" $ do
        case viewTopAgreement view of
          Just mawa -> showClashAorb "two and the truth is a majority" mawa
          Nothing -> mempty
        case viewMainAorbs view of
          Just (_, mawa) -> showClashAorb "their roman empire" mawa
          Nothing -> mempty
        case viewTopDisagreement view of
          Just mawa -> showClashAorb "who's the idiot" mawa
          Nothing -> mempty

      H.div H.! A.id "guesses" H.! A.class_ "grid gap-4 p-4" $ do
        Monad.forM_ (viewGuessResults view) $ \result -> H.div $ showGuessResult result
        if length (viewGuessResults view) < 3
          then
            case viewGuessAorbs view of
              [] -> H.div H.! A.class_ "text-center p-4 bg-base-200 rounded-lg" $ "no more questions available for guessing"
              (nextGuess:_) -> showGuessForm days nextGuess matchId
          else mempty

      let chatEnabled =
            case (viewGuessResults view, viewGuessAorbs view) of
              (results, []) | null results -> True
              (results, _)  | length results >= 3 -> any guessResultCorrect results
              _ -> False

          allGuessesCorrect =
            case viewGuessResults view of
              results | length results == 3 -> all guessResultCorrect results
              _ -> False

      H.div H.! A.class_ "p-4" $ do
        if chatEnabled
          then renderMessages config days mainUserId messages allGuessesCorrect
          else H.div H.! A.class_ "w-full max-w-2xl mx-auto text-center p-4 bg-base-200 rounded-lg" $ "complete all 3 guesses with at least one correct to unlock chat"

      H.span H.! A.id "bottom" $ mempty

showGuessForm :: Integer -> Aorb -> Int -> H.Html
showGuessForm days aorb matchId = do
  H.div H.! A.class_ "ds-card ds-card-border border-2 w-full max-w-2xl mx-auto grid" $ do
    H.div H.! A.class_ "ds-card-body p-6" $ do
      H.div H.! A.class_ "ds-card-title text-light text-sm" $ "guess"
      H.div H.! A.class_ "ds-card-title" $ H.toHtml $ aorbCtx aorb

      H.div H.! A.class_ "grid gap-4 mt-4" $ do
        H.form H.! A.method "POST" H.! A.action (H.textValue $ "/clash/t-" <> T.pack (show days) <> "/guess") $ do
          H.input H.! A.type_ "hidden" H.! A.name "aorb_id" H.! A.value (H.toValue $ show $ aorbId aorb)
          H.input H.! A.type_ "hidden" H.! A.name "choice" H.! A.value "0"
          H.input H.! A.type_ "hidden" H.! A.name "match_id" H.! A.value (H.toValue $ show matchId)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-4 text-left border rounded-lg hover:bg-base-200" $
            H.toHtml $ aorbA aorb

        H.form H.! A.method "POST" H.! A.action (H.textValue $ "/clash/t-" <> T.pack (show days) <> "/guess") $ do
          H.input H.! A.type_ "hidden" H.! A.name "aorb_id" H.! A.value (H.toValue $ show $ aorbId aorb)
          H.input H.! A.type_ "hidden" H.! A.name "choice" H.! A.value "1"
          H.input H.! A.type_ "hidden" H.! A.name "match_id" H.! A.value (H.toValue $ show matchId)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-4 text-left border rounded-lg hover:bg-base-200" $
            H.toHtml $ aorbB aorb

showGuessResult :: GuessResult -> H.Html
showGuessResult result = do
  let aorb = guessResultAorb result
      isCorrect = guessResultCorrect result
      resultClass = if isCorrect
                    then A.class_ "ds-card ds-card-border border-2 border-success w-full max-w-2xl mx-auto grid"
                    else A.class_ "ds-card ds-card-border border-2 border-error w-full max-w-2xl mx-auto grid"

  H.div H.! resultClass $ do
    H.div H.! A.class_ "ds-card-body p-6" $ do
      H.div H.! A.class_ "ds-card-title text-light text-sm flex justify-between" $ do
        H.span "your guess"
        H.span H.! A.class_ (if isCorrect then "text-success" else "text-error") $
          if isCorrect then "correct!" else "incorrect"

      H.div H.! A.class_ "ds-card-title" $ H.toHtml $ aorbCtx aorb

      H.div H.! A.class_ "grid gap-4 mt-4" $ do
        let guessClassA = if guessResultGuess result == AorbAnswer 0
                         then A.class_ "w-full p-4 text-left border-2 rounded-lg border-primary"
                         else A.class_ "w-full p-4 text-left border rounded-lg"

            actualClassA = if guessResultActual result == AorbAnswer 0
                          then A.class_ "ds-indicator-item ds-indicator-middle ds-badge ds-badge-warning"
                          else A.class_ "hidden"

            guessClassB = if guessResultGuess result == AorbAnswer 1
                         then A.class_ "w-full p-4 text-left border-2 rounded-lg border-primary"
                         else A.class_ "w-full p-4 text-left border rounded-lg"

            actualClassB = if guessResultActual result == AorbAnswer 1
                          then A.class_ "ds-indicator-item ds-indicator-middle ds-badge ds-badge-warning"
                          else A.class_ "hidden"

        H.div H.! A.class_ "ds-indicator w-full" $ do
          H.div H.! guessClassA $ H.toHtml $ aorbA aorb
          H.div H.! actualClassA $ ""

        H.div H.! A.class_ "ds-indicator w-full" $ do
          H.div H.! guessClassB $ H.toHtml $ aorbB aorb
          H.div H.! actualClassB $ ""

showClashAorb :: T.Text -> MatchingAorbWithAnswer -> H.Html
showClashAorb title mawa = do
  let aorb = matchingAorbData mawa
      mean = round (aorbMean aorb * 100)
      aorbAs = let (d, m) = divMod mean 5 in if m < 3 then d else d + 1
      aorbBs = 20 - aorbAs
      mainAns = mainUserAnswer mawa
      matchAns = otherUserAnswer mawa
      (adjustedAorbAs, adjustedAorbBs) =
        case (mainAns, matchAns) of
          (AorbAnswer 0, AorbAnswer 0) -> (aorbAs - 2, aorbBs)
          (AorbAnswer 0, AorbAnswer 1) -> (aorbAs - 1, aorbBs - 1)
          (AorbAnswer 1, AorbAnswer 0) -> (aorbAs - 1, aorbBs - 1)
          (AorbAnswer 1, AorbAnswer 1) -> (aorbAs, aorbBs - 2)
          _ -> (aorbAs, aorbBs) -- impossible
  H.div H.! A.class_ "ds-card ds-card-border border-2 w-full max-w-2xl mx-auto grid" $ do
    H.div H.! A.class_ "ds-card-body p-6" $ do
      H.div H.! A.class_ "ds-card-title text-light text-sm" $ H.toHtml $ title
      H.div H.! A.class_ "ds-card-title" $ H.toHtml $ aorbCtx aorb
      H.div H.! A.class_ "ds-stats w-full ds-stats-vertical" $ do
        H.div H.! A.class_ "ds-stat w-full p-2" $ do
          H.div H.! A.class_ "ds-stat-title mb-2" $ H.toHtml $ aorbA aorb
          H.div H.! A.class_ "flex gap-1 flex-wrap" $ do
            Monad.when (matchAns == AorbAnswer 0) (H.div H.! A.class_ "ds-badge ds-badge-warning" $ "")
            Monad.when (mainAns == AorbAnswer 0) (H.div H.! A.class_ "ds-badge ds-badge-primary" $ "")
            Monad.replicateM_ adjustedAorbAs (H.div H.! A.class_ "ds-badge ds-badge-secondary" $ "")
        H.div H.! A.class_ "ds-stat w-full p-2" $ do
          H.div H.! A.class_ "flex gap-1 flex-wrap" $ do
            Monad.when (matchAns == AorbAnswer 1) (H.div H.! A.class_ "ds-badge ds-badge-warning" $ "")
            Monad.when (mainAns == AorbAnswer 1) (H.div H.! A.class_ "ds-badge ds-badge-primary" $ "")
            Monad.replicateM_ adjustedAorbBs (H.div H.! A.class_ "ds-badge ds-badge-secondary" $ "")
          H.div H.! A.class_ "ds-stat-title mt-2" $ H.toHtml $ aorbB aorb

renderMessages :: Config -> Integer -> UserID -> [Message] -> Bool -> H.Html
renderMessages config days uid messages unlimitedMessages = do
  let userMessageCount = length $ filter ((== uid) . messageSenderId) messages
      remainingMessages = if unlimitedMessages
                          then "unlimited"
                          else T.pack $ show $ matchMessageLimit config - userMessageCount
      hasReachedLimit = not unlimitedMessages && userMessageCount >= matchMessageLimit config

  H.div H.! A.class_ "w-full max-w-2xl mx-auto mb-4" $ do
    mapM_ (renderMessage uid) messages

  H.div H.! A.class_ "w-full max-w-2xl mx-auto mb-8" $ do
    if hasReachedLimit
      then
        H.div H.! A.class_ "p-4 text-center rounded-lg bg-base-200" $
          H.toHtml ("you have reached the limit of " ++ (show $ matchMessageLimit config) ++ " messages")
      else
        H.form H.! A.id "message-form" H.! A.class_ "flex flex-col" H.! A.method "POST"
               H.! A.action (H.textValue $ "/clash/t-" <> T.pack (show days) <> "/message") $ do
          H.div H.! A.class_ "text-sm text-base-content/70 mb-1" $
            H.text $ if unlimitedMessages
                     then "unlimited messages (all guesses correct!)"
                     else remainingMessages <> " messages remaining"
          H.textarea H.! A.class_ "w-full p-4 mb-4 border border-base-400 resize-none field-sizing-content target:border-primary"
            H.! A.form "message-form" H.! A.type_ "text" H.! A.id "new-message" H.! A.name "new-message"
            H.! A.placeholder ( "message (max " <> (H.toValue $ show $ matchMessageMaxLength config) <> " characters)")
            H.! A.required "required"
            H.! A.autocomplete "off"
            H.! A.maxlength (H.toValue $ show $ matchMessageMaxLength config)
            $ ""
          H.input H.! A.class_ "px-4 py-2 bg-primary text-primary-content rounded-lg cursor-pointer font-inherit hover:bg-primary/90"
                 H.! A.type_ "submit" H.! A.value "send"

renderMessage :: UserID -> Message -> H.Html
renderMessage uid msg = do
  let (messageContainerClass, messageBubbleClass) =
        if messageSenderId msg == uid
          then (A.class_ "ds-chat ds-chat-end", A.class_ "ds-chat-bubble ds-chat-bubble-primary")
          else (A.class_ "ds-chat ds-chat-start", A.class_ "ds-chat-bubble ds-chat-bubble-warning")
      decodedContent = H.preEscapedToHtml $ XSS.sanitize $ TE.decodeUtf8With TEE.lenientDecode $ TE.encodeUtf8 $ messageContent msg
  H.div H.! messageContainerClass $ H.div H.! messageBubbleClass $ decodedContent

-- | Auth Templates

accountTemplate :: User -> H.Html
accountTemplate user = H.docTypeHtml $ H.html $ do
  pageHead "account" mempty
  H.body $ H.div $ do
    navBar (Just $ userEmail user)
    H.div H.! A.class_ "w-full m-8 max-w-xl mx-auto p-4" $ do
      H.div H.! A.class_ "ds-collapse ds-collapse-arrow bg-base-100 border border-base-400 mt-12 p-4 border-2 rounded-lg" $ do
        H.input H.! A.type_ "checkbox"
        H.h3 H.! A.class_ "ds-collapse-title text-xl font-bold text-base-400" $ "danger zone"
        H.div H.! A.class_ "ds-collapse-content" $ do
          H.a H.! A.href "/logout" H.! A.class_ "ds-btn ds-btn-block mt-4 mb-4 ds-btn-warning" $ "logout from all devices"
          H.a H.! A.href "/delete" H.! A.class_ "ds-btn ds-btn-block ds-btn-error" $ "delete account and data"

loginTemplate :: T.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  pageHead "login" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "md:text-3xl text-xl mb-8" $ H.a H.! A.href "/" $ anorbyTitle
      H.div H.! A.class_ "w-full max-w-sm mx-auto mb-8" $ do
        H.form H.! A.class_ "flex flex-col gap-2" H.! A.method "POST" H.! A.action "/login" $ do
          H.input H.! A.type_ "email" H.! A.name "email" H.! A.placeholder "email" H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300" H.! A.required "required"
          H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $ "login"
      H.a H.! A.class_ "ds-link ds-link-primary ds-link-hover" H.! A.href "/register" $ "register"

registerTemplate :: T.Text -> H.Html
registerTemplate token = H.docTypeHtml $ H.html $ do
  pageHead "register" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "md:text-3xl text-xl mb-8" $ H.a H.! A.href "/" $ anorbyTitle
      H.div H.! A.class_ "w-full max-w-sm mx-auto mb-8" $ do
        H.form H.! A.class_ "flex flex-col gap-2" H.! A.method "POST" H.! A.action "/register" $ do
          H.input H.! A.type_ "email" H.! A.name "email" H.! A.placeholder "email" H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300" H.! A.required "required"
          H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $ "register"
      H.a H.! A.class_ "ds-link ds-link-primary ds-link-hover" H.! A.href "/login" $ "login"

confirmTemplate :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> H.Html
confirmTemplate title warning action token actionText cancelUrl = H.docTypeHtml $ H.html $ do
  pageHead title mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.form H.! A.method "POST" H.! A.action (H.textValue action) $ do
        H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
        H.button H.! A.type_ "submit" H.! A.class_ "mb-8 px-8 py-4 ds-btn ds-btn-error" $ H.toHtml actionText
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ H.toHtml title
      H.p H.! A.class_ "mb-8" $ H.toHtml warning
      H.a H.! A.href (H.textValue cancelUrl) H.! A.class_ "px-8 py-4 ds-btn ds-btn-secondary" $ "cancel"

-- | Message Templates

msgTemplate :: MessageTemplate -> H.Html
msgTemplate template = H.docTypeHtml $ H.html $ do
  pageHead (messageTitle template) mempty
  H.body $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center" $ do
      H.div H.! A.class_ "md:text-3xl text-xl" $ H.a H.! A.href "/" $ anorbyTitle
      H.h1 H.! A.class_ "text-2xl font-bold mt-4 mb-4" $ H.toHtml $ messageHeading template
      H.div H.! A.class_ "mb-32" $ do
        H.a H.! A.href (H.textValue $ fst $ messageLink template)
            H.! A.class_ "ds-link ds-link-primary ds-link-hover" $
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
  , messageLink = ("/clash", "clash")
  }

noMoreQuestionsTemplate :: H.Html
noMoreQuestionsTemplate = msgTemplate MessageTemplate
  { messageTitle = "no more questions"
  , messageHeading = "no more questions"
  , messageLink = ("/clash", "clash")
  }

profileNotYetActive :: Int -> H.Html
profileNotYetActive threshold = msgTemplate MessageTemplate
  { messageTitle = "profile not yet active"
  , messageHeading = T.pack $ "whoami will be activated after answering " ++ show threshold ++ " questions"
  , messageLink = ("/ans", "answer more questions")
  }

matchNotYetActive :: Int -> H.Html
matchNotYetActive threshold = msgTemplate MessageTemplate
  { messageTitle = "match not yet active"
  , messageHeading = T.pack $ "clash will be activated after answering " ++ show threshold ++ " questions"
  , messageLink = ("/ans", "answer more questions")
  }

errorTemplateWithLink :: Int -> T.Text -> (T.Text, T.Text) -> H.Html
errorTemplateWithLink code message link = msgTemplate MessageTemplate
  { messageTitle = message
  , messageHeading = T.pack (show code) <> " - " <> message
  , messageLink = link
  }

errorTemplate :: Int -> T.Text -> H.Html
errorTemplate code message = errorTemplateWithLink code message ("/", "home")

alreadyAnsweredTemplate :: H.Html
alreadyAnsweredTemplate = errorTemplateWithLink 403 "question already answered" ("/ans", "next question")

invalidTokenTemplate :: H.Html
invalidTokenTemplate = errorTemplateWithLink 403 "invalid or expired token" ("/ans", "try again")

invalidSubmissionTemplate :: H.Html
invalidSubmissionTemplate = errorTemplateWithLink 400 "invalid submission format" ("/ans", "try again")

userNotFoundTemplate :: H.Html
userNotFoundTemplate = errorTemplateWithLink 404 "user not found" ("/register", "register")

emailExistsTemplate :: H.Html
emailExistsTemplate = errorTemplateWithLink 409 "email already registered" ("/login", "login")

notFoundTemplate :: H.Html
notFoundTemplate = errorTemplate 404 "not found"

internalErrorTemplate :: H.Html
internalErrorTemplate = errorTemplate 500 "internal server error"
