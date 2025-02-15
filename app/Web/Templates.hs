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
      H.a H.! A.href "/match" $ H.div H.! A.class_ "ds-btn hover:text-primary transition-all" $ "clash"
    case maybeSubtitle of
      Just subtitle -> do
        H.div H.! A.class_ "ds-tooltip ds-tooltip-open ds-tooltip-bottom ds-tooltip-secondary" $ do
          H.div H.! A.class_ "ds-tooltip-content" $ H.text subtitle
          H.div $ H.a H.! A.class_ "ds-navbar-center text-3xl" H.! A.href "/" $ anorbyTitle
      Nothing -> do
        H.div $ H.a H.! A.class_ "ds-navbar-center text-3xl" H.! A.href "/" $ anorbyTitle
    H.div H.! A.class_ "ds-navbar-end ds-dropdown ds-dropdown-end ds-dropdown-bottom" $ do
      H.div H.! A.tabindex "0" H.! A.role "button" H.! A.class_ "ds-btn hover:text-primary transition-all" $ "you"
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
      navBar Nothing
      H.div H.! A.class_ "p-4 max-w-4xl mx-auto" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-4" $ "Add New Question"
        H.form H.! A.method "POST"
               H.! A.action "/admin/aorb/add"
               H.! A.class_ "space-y-4" $ do
          H.div $ do
            H.label H.! A.for "context" $ "Context"
            H.textarea H.! A.id "context"
                      H.! A.name "context"
                      H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                      H.! A.required "required"
                      $ ""
          H.div $ do
            H.label H.! A.for "subtext" $ "Subtext"
            H.textarea H.! A.id "subtext"
                      H.! A.name "subtext"
                      H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                      $ ""
          H.div $ do
            H.label H.! A.for "option_a" $ "Option A"
            H.input H.! A.type_ "text"
                   H.! A.id "option_a"
                   H.! A.name "option_a"
                   H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                   H.! A.required "required"
          H.div $ do
            H.label H.! A.for "option_b" $ "Option B"
            H.input H.! A.type_ "text"
                   H.! A.id "option_b"
                   H.! A.name "option_b"
                   H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                   H.! A.required "required"
          H.button H.! A.type_ "submit"
                  H.! A.class_ "px-4 py-2 bg-primary text-primary-content rounded-lg"
                  $ "Add Question"

      H.div H.! A.class_ "p-4 max-w-4xl mx-auto" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-4" $ "All Questions"
        H.div H.! A.class_ "space-y-4" $ do
          mapM_ renderAorbAdmin aorbs

renderAorbAdmin :: Aorb -> H.Html
renderAorbAdmin aorb =
  H.div H.! A.class_ "border border-base-300 rounded-lg p-4" $ do
    H.div H.! A.class_ "flex justify-between items-start" $ do
      H.div H.! A.class_ "space-y-2" $ do
        H.div H.! A.class_ "font-bold" $
          H.toHtml $ "ID: " <> T.pack (show $ aorbId aorb)
        H.div H.! A.class_ "text-base-content/60 italic" $
          H.toHtml $ aorbCtx aorb
        H.div $ H.toHtml $ aorbStx aorb
        H.div H.! A.class_ "mt-2" $ do
          H.div $ H.toHtml $ "A: " <> aorbA aorb
          H.div $ H.toHtml $ "B: " <> aorbB aorb
          H.div H.! A.class_ "text-sm text-base-content/60" $
            H.toHtml $ "Mean: " <> T.pack (show $ aorbMean aorb)

      H.div H.! A.class_ "space-x-2" $ do
        H.form H.! A.method "GET"
               H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/edit")
               H.! A.class_ "inline-block" $ do
          H.button H.! A.type_ "submit"
                  H.! A.class_ "px-4 py-2 bg-warning text-warning-content rounded-lg"
                  $ "Edit"

        let dialogId = "delete-dialog-" <> T.pack (show $ aorbId aorb)
        H.button H.! A.type_ "button"
                H.! A.class_ "px-4 py-2 bg-error text-error-content rounded-lg"
                H.! A.onclick (H.textValue $ "document.getElementById('" <> dialogId <> "').showModal()")
                $ "Delete"

        H.dialog H.! A.id (H.textValue dialogId)
                H.! A.class_ "p-6 rounded-lg backdrop:bg-black/50" $ do
          H.h3 H.! A.class_ "text-lg font-bold mb-4" $
            "Confirm Delete"
          H.p H.! A.class_ "mb-6" $
            "Are you sure you want to delete this question? This action cannot be undone."
          H.div H.! A.class_ "flex justify-end gap-4" $ do
            H.button H.! A.type_ "button"
                    H.! A.class_ "px-4 py-2 border border-base-300 rounded-lg"
                    H.! A.onclick (H.textValue $ "document.getElementById('" <> dialogId <> "').close()")
                    $ "Cancel"
            H.form H.! A.method "POST"
                  H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/delete")
                  H.! A.class_ "inline-block" $ do
              H.button H.! A.type_ "submit"
                      H.! A.class_ "px-4 py-2 bg-error text-error-content rounded-lg"
                      $ "Delete"

editAorbTemplate :: Aorb -> H.Html
editAorbTemplate aorb = H.docTypeHtml $ H.html $ do
  pageHead "edit question" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.div H.! A.class_ "p-4 max-w-4xl mx-auto" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-4" $
          H.toHtml $ "Edit Question #" <> T.pack (show $ aorbId aorb)

        H.form H.! A.method "POST"
               H.! A.action (H.textValue $ "/admin/aorb/" <> T.pack (show $ aorbId aorb) <> "/edit")
               H.! A.class_ "space-y-4" $ do
          H.div $ do
            H.label H.! A.for "context" $ "Context"
            H.textarea H.! A.id "context"
                      H.! A.name "context"
                      H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                      H.! A.required "required"
                      $ H.toHtml (aorbCtx aorb)
          H.div $ do
            H.label H.! A.for "subtext" $ "Subtext"
            H.textarea H.! A.id "subtext"
                      H.! A.name "subtext"
                      H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                      $ H.toHtml (aorbStx aorb)
          H.div $ do
            H.label H.! A.for "option_a" $ "Option A"
            H.input H.! A.type_ "text"
                   H.! A.id "option_a"
                   H.! A.name "option_a"
                   H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                   H.! A.value (H.toValue $ aorbA aorb)
                   H.! A.required "required"
          H.div $ do
            H.label H.! A.for "option_b" $ "Option B"
            H.input H.! A.type_ "text"
                   H.! A.id "option_b"
                   H.! A.name "option_b"
                   H.! A.class_ "w-full p-2 border border-base-300 rounded-lg"
                   H.! A.value (H.toValue $ aorbB aorb)
                   H.! A.required "required"

          H.div H.! A.class_ "flex gap-4" $ do
            H.button H.! A.type_ "submit"
                    H.! A.class_ "px-4 py-2 bg-primary text-primary-content rounded-lg"
                    $ "Save Changes"
            H.a H.! A.href "/admin"
                H.! A.class_ "px-4 py-2 border border-base-300 rounded-lg"
                $ "Cancel"

data ShowAorbMode = Population Aorb [Int]
                  | Individual AorbWithAnswer [Int] (Maybe AorbID) (Maybe T.Text)

aorbDynamicCSS :: [(String, Int)] -> H.AttributeValue
aorbDynamicCSS orderPairs =
  H.preEscapedTextValue $ T.intercalate "; " [ "--order-" <> T.pack name <> ": " <> T.pack (show order) | (name, order) <- orderPairs ]

orderAorbs :: (Eq a) => [a] -> [OrderingFunction a] -> [(a, [Int])]
orderAorbs as orderingFuncs =
  let orderedLists = map (\f -> f as) orderingFuncs
      lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
  in [ (a, map (\orderedList -> lookupOrder orderedList a) orderedLists) | a <- as ]

rootTemplate :: Int -> Int -> Int -> Int -> Int -> Int -> MatchStatus -> [Aorb] -> H.Html
rootTemplate totalQuestions totalAnswers todayAnswers activeUsers newUsers matchingEnrolled matchStatus aorbs =
  H.docTypeHtml $ H.html $ do
  pageHead "anorby"
    ( H.style $ H.preEscapedText $ T.unlines
      [ ".aorb { order: var(--order-diced); }"
      , ":root:has(#diced:target) .aorb { order: var(--order-diced) !important; }"
      , ":root:has(#sided:target) .aorb { order: var(--order-sided) !important; }"
      , ":root:has(#split:target) .aorb { order: var(--order-split) !important; }"
      ]
    )
  H.body $ do
    H.div $ do
      H.div H.! A.id "diced" $ mempty
      H.div H.! A.id "sided" $ mempty
      H.div H.! A.id "split" $ mempty
      navBar Nothing
      H.div H.! A.class_ "w-full max-w-4xl mx-auto grid gap-8 place-items-center" $ do
        rootStats totalQuestions totalAnswers todayAnswers activeUsers newUsers matchingEnrolled matchStatus
        H.fieldset H.! A.class_ "flex flex-col mb-2 items-center gap-4" $ do
          H.div "sorter:"
          H.div H.! A.class_ "flex gap-8" $ do
            H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-neutral" H.! A.href "#diced" $ "random"
            H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-neutral" H.! A.href "#sided" $ "unanimity"
            H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-neutral" H.! A.href "#split" $ "deadlocks"
        H.div H.! A.class_ "grid gap-8 justify-items-center" $ do
          mapM_ (showAorb . uncurry Population) (orderAorbs aorbs orderFuncs)
  where
    orderFuncs =
      [ id
      , List.sortOn (Ord.Down . \a -> abs (aorbMean a - 0.5))
      , List.sortOn (\a -> abs (aorbMean a - 0.5))
      ]

rootStats :: Int -> Int -> Int -> Int -> Int -> Int -> MatchStatus -> H.Html
rootStats totalQuestions totalAnswers todayAnswers activeUsers newUsers matchingEnrolled matchStatus =
  H.div H.! A.class_ "ds-stats ds-stats-vertical lg:ds-stats-horizontal grid grid-cols-2 lg:grid-cols-4 shadow" $ do
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "population"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show activeUsers
      H.div H.! A.class_ "ds-stat-desc" $ H.toHtml $ ("+" <> show newUsers <> " this week")
    H.div H.! A.class_ "ds-stat" $ do
      H.div H.! A.class_ "ds-stat-title" $ "questions"
      H.div H.! A.class_ "ds-stat-value" $ H.toHtml $ show totalQuestions
      H.div H.! A.class_ "ds-stat-desc" $ "(more to come)"
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
      (True, True) -> A.class_ "aorb w-screen max-w-3xl ds-card ds-card-border border-3 rounded-4xl border-warning hover:-translate-y-1 hover:bg-base-200 transition-all"
      (True, False) -> A.class_ "aorb w-screen max-w-3xl ds-card ds-card-border border-3 rounded-4xl border-primary"
      (False, True) -> A.class_ "aorb w-screen max-w-3xl ds-card ds-card-border border-3 rounded-4xl hover:-translate-y-1 hover:bg-base-200 transition-all"
      (False, False) -> A.class_ "aorb w-screen max-w-3xl ds-card ds-card-border border-3 rounded-4xl"
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
      [ ".aorb { order: var(--order-flake); }"
      , ":root:has(#basic:target) .aorb { order: var(--order-basic) !important; }"
      , ":root:has(#flake:target) .aorb { order: var(--order-flake) !important; }"
      ]
    )
  H.body $ do
    H.div H.! A.id "flake" $ mempty
    H.div H.! A.id "basic" $ mempty
    navBar $ case maybeUuid of
      Just uuid -> Just $ "#" <> uuid
      Nothing -> Just "whoami"
    H.div H.! A.class_ "grid gap-8 place-items-center" $ do
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
          H.fieldset H.! A.class_ "flex flex-col mb-8 items-center gap-4" $ do
            H.div "sorter:"
            H.div H.! A.class_ "flex gap-8" $ do
              H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-neutral" H.! A.href "#basic" $ "conformist"
              H.a H.! A.role "button" H.! A.class_ "ds-btn ds-btn-soft ds-btn-neutral" H.! A.href "#flake" $ "contrarian"
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
          if isFavorite
            then mempty
            else
              H.div H.! A.class_ "ds-card-actions justify-center" $ do
                H.form H.! A.method "POST" H.! A.action (H.toValue $ "/aorb/favorite/" ++ show (aorbId aorb)) $ do
                  H.button H.! A.type_ "submit" H.! A.class_ "px-8 py-4 border-2 border-base-300 bg-transparent cursor-pointer font-inherit text-base transition-all hover:border-warning hover:text-warning rounded-lg" $
                    "set as favorite question"
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

matchTemplate :: Config -> Maybe POSIXTime.POSIXTime -> Maybe POSIXTime.POSIXTime -> POSIXTime.POSIXTime -> Bool -> Int -> Maybe (Match, Double) -> MatchStatus -> H.Html
matchTemplate config maybeCutoffTime maybeReleaseTime now isEnrolled enrolledCount maybeMatchScore matchStatus = H.docTypeHtml $ H.html $ do
  pageHead "match" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "match"
      H.div H.! A.class_ "flex justify-center gap-4 flex-wrap" $ do
        H.a H.! A.href "/match/found" H.! A.class_ "link hover:text-primary transition-colors" $ "past"
        H.span H.! A.class_ "text-base-content/50" $ "|"
        H.a H.! A.href "/match/type" H.! A.class_ "link hover:text-primary transition-colors" $ "future"
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "today"
      matchTodaySection config maybeCutoffTime maybeReleaseTime now isEnrolled enrolledCount maybeMatchScore matchStatus

matchTodaySection :: Config -> Maybe POSIXTime.POSIXTime -> Maybe POSIXTime.POSIXTime -> POSIXTime.POSIXTime -> Bool -> Int -> Maybe (Match, Double) -> MatchStatus -> H.Html
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
      effectiveMatchStatus = if isBeforeCutoff || not isBeforeRelease
                                then NotStarted
                                else matchStatus
  in H.div $ do
      case effectiveMatchStatus of
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
        NotStarted -> do
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
  pageHead "match type" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ "match type"

      H.div $ do
        schemeCard PPPod (userAssoc user)
        schemeCard Swing (userAssoc user)
        schemeCard Bipolar (userAssoc user)

    H.div $ do
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
  pageHead "matches" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.h1 H.! A.class_ "text-2xl font-bold mb-4 text-center" $ "matches"
      if null matchData
        then do
          H.h3 H.! A.class_ "text-center mb-8 text-base-content/70" $ "no matches found"
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

matchProfileTemplate :: Config -> Integer -> UserID -> UserID -> MatchView -> [Message] -> H.Html
matchProfileTemplate config days mainUserId _ view messages = H.docTypeHtml $ H.html $ do
  pageHead "match profile" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "stats"
      H.div H.! A.class_ "grid grid-cols-2 gap-4 max-w-xl mx-auto w-full" $ do
        statsBox "matched on" (formatMatchDate (viewTimestamp view))
        statsBox "agreement rate" (formatPercent (viewAgreementRate view))
        statsBox "you answered" (T.pack $ show $ viewYourTotalAnswers view)
        statsBox "they answered" (T.pack $ show $ viewTargetTotalAnswers view)

    H.div $ do
      H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "two and the truth is a majority"
      case viewTopAgreement view of
        Just mawa -> matchProfileAgreement mawa
        Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no agreements found"

    H.div $ do
      H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "their roman empire"
      case viewMainAorbs view of
        Just (_, theirMain) -> spotlightAorbSection mainUserId theirMain
        Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no main question found"

    H.div $ do
      H.h2 H.! A.class_ "text-2xl font-bold mb-8" $ "plz mend the rift"
      case viewTopDisagreement view of
        Just mawa -> matchProfileDisagreement mainUserId mawa
        Nothing -> H.div H.! A.class_ "text-base-content/60 italic p-8" $ "no disagreements found"

    H.div $ do
      H.h1 H.! A.class_ "text-2xl font-bold mb-8" $ "head-to-head"
      renderMessages config days mainUserId messages

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

renderMessages :: Config -> Integer -> UserID -> [Message] -> H.Html
renderMessages config days uid messages = do
  let userMessageCount = length $ filter ((== uid) . messageSenderId) messages
      remainingMessages = matchMessageLimit config - userMessageCount
      hasReachedLimit = userMessageCount >= matchMessageLimit config

  H.div H.! A.class_ "w-full max-w-2xl mx-auto space-y-4" $ do
    mapM_ (renderMessage uid) messages

    if hasReachedLimit
      then H.div H.! A.class_ "p-4 text-center rounded-lg bg-base-200" $ do
        "You have reached the limit of "
        H.toHtml $ show $ matchMessageLimit config
        " messages"
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
          H.! A.placeholder
            ( "message (max " <>
              (H.toValue $ show $ matchMessageMaxLength config) <>
              " characters)"
            )
          H.! A.required "required"
          H.! A.autocomplete "off"
          H.! A.maxlength (H.toValue $ show $ matchMessageMaxLength config)
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
      decodedContent = H.preEscapedToHtml $ XSS.sanitize $
        TE.decodeUtf8With TEE.lenientDecode $ TE.encodeUtf8 $
          messageContent msg
  H.div H.! A.class_ messageClasses $ do
    H.div H.! A.class_ "message-content" $ decodedContent

-- | Auth Templates

accountTemplate :: User -> H.Html
accountTemplate user = H.docTypeHtml $ H.html $ do
  pageHead "account" mempty
  H.body $ do
    H.div $ do
      navBar Nothing
      H.div H.! A.class_ "w-full max-w-xl mx-auto" $ do
        H.h2 H.! A.class_ "text-2xl font-bold mb-4 pb-2 border-b border-base-300" $
          "account information"
        H.p H.! A.class_ "mb-4" $ do
          H.text "email: "
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

loginTemplate :: T.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  pageHead "login" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "text-3xl mb-8" $ H.a H.! A.href "/" $ anorbyTitle
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
  pageHead "register" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "text-3xl mb-8" $ H.a H.! A.href "/" $ anorbyTitle
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
confirmTemplate title warning action token actionText cancelUrl = H.docTypeHtml $ H.html $ do
  pageHead title mempty
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

-- | Message Templates

msgTemplate :: MessageTemplate -> H.Html
msgTemplate template = H.docTypeHtml $ H.html $ do
  pageHead (messageTitle template) mempty
  H.body $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "text-3xl mb-8" $ H.a H.! A.href "/" $ anorbyTitle
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
