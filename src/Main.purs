module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.HashMap as HM
import Data.JSDate as Date
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect.Console (log)
import Effect (Effect)
import Effect.Exception (throw)
import Web.DOM.ChildNode as Dom.ChildNode
import Web.DOM.Document as Dom.Document
import Web.DOM.Element as Dom.Element
import Web.DOM.HTMLCollection as Dom.HTMLCollection
import Web.DOM.Node as Dom.Node
import Web.DOM.NodeList as Dom.NodeList
import Web.DOM.ParentNode as Dom.ParentNode
import Web.DOM.ParentNode (QuerySelector (..), querySelector, querySelectorAll)
import Web.HTML as Html
import Web.HTML.HTMLDocument as Html.Doc
import Web.HTML.HTMLElement as Html.Element
import Web.HTML.Window as Html.Window

foreign import innerText :: Html.Element.HTMLElement -> Effect String

type Entry a =
    { start   :: Date.JSDate
    , end     :: Date.JSDate
    , content :: a
    }

data Schedule a
    = Single (Entry a)
    | After (Schedule a) (Schedule a)
    | Parallel (Schedule a) (Schedule a)

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

scheduleRender
    :: Dom.Document.Document -> Schedule String -> Effect Dom.Element.Element
scheduleRender doc = \schedule -> do
    container <- Dom.Document.createElement "div" doc
    let start = ticks $ scheduleStart schedule
        end = ticks $ scheduleEnd schedule
        height = end - start
    Dom.Element.setAttribute "style"
        ("position: relative; height: " <> show height <> "px; width: 100%;")
        container
    log $ "start: " <> show (scheduleStart schedule)
    log $ "start: " <> show start
    log $ "end: " <> show (scheduleEnd schedule)
    log $ "end: " <> show end
    log $ "height: " <> show height
    go container (scheduleStart schedule) 0 100 schedule
    pure container
  where
    go container zero left width schedule = case schedule of
        Single {start, end, content} -> do
            let y = ticks start - ticks zero
                height = ticks end - ticks start
            div <- Dom.Document.createElement "div" doc
            Dom.Element.setAttribute "style"
                ("position: absolute;" <>
                    "top: " <> show y <> "px;" <>
                    "left: " <> show left <> "%; " <>
                    "width: " <> show width <> "%; " <>
                    "height: " <> show height <> "px;")
                div
            Dom.Node.setTextContent content (Dom.Element.toNode div)
            Dom.Node.appendChild
                (Dom.Element.toNode div) (Dom.Element.toNode container)
        After x y -> do
            go container zero left width x
            go container zero left width y
        Parallel x y -> do
            let xp = scheduleParallelism x
                yp = scheduleParallelism y
                unitWidth = width / (xp + yp)
            go container zero left (xp * unitWidth) x
            go container zero (left + xp * unitWidth) (yp * unitWidth) y

    ticks date = 1.5 * Date.getTime date / 1000.0 / 60.0


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

calendarRender
    :: Dom.Document.Document -> Calendar String
    -> Effect (Array Dom.Element.Element)
calendarRender doc calendar = traverse (scheduleRender doc) $
    Array.sortWith scheduleStart $ Array.fromFoldable calendar

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
    let calendar = calendarFromEntries entries :: Calendar String

    -- Remove original content.
    Dom.ParentNode.children (Dom.Element.toParentNode schedule) >>=
        Dom.HTMLCollection.toArray >>=
        traverse_ (Dom.Element.toChildNode >>> Dom.ChildNode.remove)

    -- Add rendered content.
    rendered <- calendarRender (Html.Doc.toDocument doc) calendar
    for_ rendered $ \c -> Dom.Node.appendChild
        (Dom.Element.toNode c) (Dom.Element.toNode schedule)
