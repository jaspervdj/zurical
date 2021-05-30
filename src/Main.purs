module Main where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Data.Array as Array
import Data.Traversable (traverse)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe)
import Web.DOM.ParentNode (QuerySelector (..), querySelector, querySelectorAll)
import Web.DOM.NodeList as Dom.NodeList
import Data.JSDate as Date
import Web.HTML.Window as Html.Window
import Data.HashMap as HM
import Web.DOM.Element as Dom.Element
import Web.HTML.HTMLElement as Html.Element
import Web.HTML as Html
import Web.HTML.HTMLDocument as Html.Doc
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Entry a =
    { start   :: Date.JSDate
    , end     :: Date.JSDate
    , content :: String
    }

data Schedule a
    = Single (Entry a)
    | After (Schedule a) (Schedule a)
    | Parallel (Schedule a) (Schedule a)

derive instance genericSchedule :: Generic (Schedule a) _

instance showSchedule :: Show a => Show (Schedule a) where
    show x = genericShow x

scheduleStart :: forall a. Schedule a -> Date.JSDate
scheduleStart (Single entry) = entry.start
scheduleStart (After x _) = scheduleStart x
scheduleStart (Parallel x y) = min (scheduleStart x) (scheduleStart y)

scheduleEnd :: forall a. Schedule a -> Date.JSDate
scheduleEnd (Single entry) = entry.end
scheduleEnd (After _ y) = scheduleEnd y
scheduleEnd (Parallel x y) = max (scheduleEnd x) (scheduleEnd y)

scheduleParallelism :: forall a. Schedule a -> Int
scheduleParallelism (Single _) = 1
scheduleParallelism (After x y) =
    max (scheduleParallelism x) (scheduleParallelism y)
scheduleParallelism (Parallel x y) =
    scheduleParallelism x + scheduleParallelism y

scheduleInsert :: forall a. Entry a -> Schedule a -> Schedule a
scheduleInsert entry schedule
    | entry.start >= scheduleEnd schedule =
        After schedule (Single entry)
    | entry.end <= scheduleStart schedule =
        After (Single entry) schedule
    | otherwise = case schedule of
        Single other -> Parallel (Single other) (Single entry)
        After x y
           | entry.start >= scheduleEnd x ->
               After x (scheduleInsert entry y)
           | entry.end <= scheduleEnd y ->
               After (scheduleInsert entry x) y
           | otherwise -> Parallel (After x y) (Single entry)
        Parallel x y ->
            let x' = scheduleInsert entry x
                l  = scheduleParallelism x' + scheduleParallelism y
                y' = scheduleInsert entry y
                r  = scheduleParallelism x + scheduleParallelism y' in
            if l < r then Parallel x' y else Parallel x y'

type Day = String

type Calendar a = HM.HashMap Day (Schedule a)

calendarInsert :: forall a. Entry a -> Calendar a -> Calendar a
calendarInsert entry calendar = case HM.lookup day calendar of
    Nothing -> HM.insert day (Single entry) calendar
    Just events -> HM.insert day (scheduleInsert entry events) calendar
  where
    day = Date.toDateString entry.start

calendarFromEntries :: forall a. Array (Entry a) -> Calendar a
calendarFromEntries = Array.foldl (flip calendarInsert) HM.empty

foreign import innerText :: Html.Element.HTMLElement -> Effect String

parseEntry
    :: Dom.Element.Element -> Effect (Maybe (Entry String))
parseEntry element = do
    tds <- querySelectorAll
            (QuerySelector "td") (Dom.Element.toParentNode element) >>=
        Dom.NodeList.toArray
    case map Html.Element.fromNode tds of
        [Just startElement, Just endElement, Just contentElement] -> do
            startText <- innerText startElement
            start <- Date.parse startText
            endText <- innerText endElement
            end <- Date.parse endText
            content <- innerText contentElement
            pure $ Just {start, end, content}
        _ -> pure Nothing

parseEntries
    :: Dom.Element.Element -> Effect (Array (Entry String))
parseEntries schedule = do
    trs <- querySelectorAll (QuerySelector "tr")
            (Dom.Element.toParentNode schedule) >>=
        Dom.NodeList.toArray
    map Array.catMaybes $
        traverse parseEntry $ Array.mapMaybe Dom.Element.fromNode trs

main :: Effect Unit
main = do
    window <- Html.window
    doc <- Html.Window.document window
    schedule <- querySelector
        (QuerySelector ".schedule") (Html.Doc.toParentNode doc) >>=
        maybe (throw "no schedule") pure

    entries <- parseEntries schedule
    log $ show (calendarFromEntries entries :: Calendar String)
    log "🍝"
