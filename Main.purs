module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Generic.Rep (class Generic)
import Data.HashMap as HM
import Data.Int as Int
import Data.JSDate as Date
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
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
foreign import renderDateTime :: Date.JSDate -> Effect String

type Entry t a =
    { start   :: t
    , end     :: t
    , content :: a
    }

data Schedule t a
    = Single (Entry t a)
    | After (Schedule t a) (Schedule t a)
    | Parallel (Schedule t a) (Schedule t a)

derive instance genericSchedule :: Generic (Schedule t a) _

instance showSchedule :: (Show t, Show a) => Show (Schedule t a) where
    show x = genericShow x

scheduleStart :: forall t a. Ord t => Schedule t a -> t
scheduleStart (Single entry) = entry.start
scheduleStart (After x _) = scheduleStart x
scheduleStart (Parallel x y) = min (scheduleStart x) (scheduleStart y)

scheduleEnd :: forall t a. Ord t => Schedule t a -> t
scheduleEnd (Single entry) = entry.end
scheduleEnd (After _ y) = scheduleEnd y
scheduleEnd (Parallel x y) = max (scheduleEnd x) (scheduleEnd y)

scheduleParallelism :: forall t a. Schedule t a -> Int
scheduleParallelism (Single _) = 1
scheduleParallelism (After x y) =
    max (scheduleParallelism x) (scheduleParallelism y)
scheduleParallelism (Parallel x y) =
    scheduleParallelism x + scheduleParallelism y

scheduleInsert :: forall t a. Ord t => Entry t a -> Schedule t a -> Schedule t a
scheduleInsert entry schedule
    | entry.start >= scheduleEnd schedule =
        After schedule (Single entry)
    | entry.end <= scheduleStart schedule =
        After (Single entry) schedule
    | otherwise = case schedule of
        Single other -> Parallel (Single other) (Single entry)
        After (After x y) z
            | entry.start >= scheduleEnd x ->
                After x (scheduleInsert entry (After y z))
        After x y
            | entry.start >= scheduleEnd x ->
                After x (scheduleInsert entry y)
            | entry.end <= scheduleStart y ->
                After (scheduleInsert entry x) y
            | otherwise -> Parallel (After x y) (Single entry)
        Parallel x y ->
            let x' = scheduleInsert entry x
                l  = scheduleParallelism x' + scheduleParallelism y
                y' = scheduleInsert entry y
                r  = scheduleParallelism x + scheduleParallelism y' in
            if l < r then Parallel x' y else Parallel x y'

scheduleRender
    :: Dom.Document.Document -> String -> Schedule Date.JSDate Info
    -> Effect Dom.Element.Element
scheduleRender doc day schedule0 = do
    container <- Dom.Document.createElement "div" doc
    let start = ticks $ scheduleStart schedule0
        end = ticks $ scheduleEnd schedule0
        height = end - start + Int.toNumber margin
    Dom.Element.setAttribute "class" "day" container
    Dom.Element.setAttribute "style"
        ("position: relative; height: " <> show height <> "px;")
        container

    div <- Dom.Document.createElement "div" doc
    Dom.Element.setAttribute "class" "date" div
    Dom.Node.setTextContent day (Dom.Element.toNode div)
    Dom.Node.appendChild (Dom.Element.toNode div) (Dom.Element.toNode container)

    go container 0.0 100.0 schedule0
    pure container
  where
    margin = 30
    zero   = scheduleStart schedule0

    go container left width schedule = case schedule of
        Single {start, end, content: {kind, link, title}} -> do
            let y = ticks start - ticks zero + Int.toNumber margin
                height = ticks end - ticks start
            div <- case String.trim link of
                "" -> Dom.Document.createElement "div" doc
                link' -> do
                    a <- Dom.Document.createElement "a" doc
                    Dom.Element.setAttribute "href" link' a
                    pure a

            Dom.Element.setAttribute "class" ("entry " <> kind) div
            Dom.Element.setAttribute "style"
                ("position: absolute;" <>
                    "top: " <> show y <> "px;" <>
                    "left: " <> show left <> "%; " <>
                    "width: " <> show width <> "%; " <>
                    "height: " <> show height <> "px;")
                div

            timeSpan <- Dom.Document.createElement "span" doc
            Dom.Element.setAttribute "class" "time" timeSpan
            startStr <- renderDateTime start
            endStr <- renderDateTime end
            Dom.Node.setTextContent (startStr <> " - " <> endStr <> ":")
                (Dom.Element.toNode timeSpan)
            Dom.Node.appendChild
                (Dom.Element.toNode timeSpan) (Dom.Element.toNode div)

            titleSpan <- Dom.Document.createElement "span" doc
            Dom.Element.setAttribute "class" "title" titleSpan
            Dom.Node.setTextContent title (Dom.Element.toNode titleSpan)
            Dom.Node.appendChild
                (Dom.Element.toNode titleSpan) (Dom.Element.toNode div)

            Dom.Node.appendChild
                (Dom.Element.toNode div) (Dom.Element.toNode container)
        After x y -> do
            go container left width x
            go container left width y
        Parallel x y -> do
            let xp = Int.toNumber $ scheduleParallelism x
                yp = Int.toNumber $ scheduleParallelism y
                unitWidth = width / (xp + yp)
            go container left (xp * unitWidth) x
            go container (left + xp * unitWidth) (yp * unitWidth) y

    ticks date = Date.getTime date / 1000.0 / 60.0


type Day = String

type Calendar a = HM.HashMap Day (Schedule Date.JSDate a)

calendarInsert :: forall a. Entry Date.JSDate a -> Calendar a -> Calendar a
calendarInsert entry calendar = case HM.lookup day calendar of
    Nothing -> HM.insert day (Single entry) calendar
    Just events -> HM.insert day (scheduleInsert entry events) calendar
  where
    day = Date.toDateString entry.start

calendarFromEntries :: forall a. Array (Entry Date.JSDate a) -> Calendar a
calendarFromEntries = Array.foldl (flip calendarInsert) HM.empty

calendarRender
    :: Dom.Document.Document -> Calendar Info
    -> Effect (Array Dom.Element.Element)
calendarRender doc calendar = traverse (uncurry $ scheduleRender doc) $
    Array.sortWith (snd >>> scheduleStart) $ HM.toArrayBy Tuple calendar

type Info =
    { kind  :: String
    , link  :: String
    , title :: String
    }

parseEntry
    :: Dom.Element.Element -> Effect (Maybe (Entry Date.JSDate Info))
parseEntry element = do
    cells <- querySelectorAll
            (QuerySelector "td") (Dom.Element.toParentNode element) >>=
        Dom.NodeList.toArray >>=
        (Array.mapMaybe Html.Element.fromNode >>> pure) >>=
        traverse innerText
    case cells of
        [startText, endText, kind, link, title] -> do
            start <- Date.parse startText
            end <- Date.parse endText
            pure $ Just {start, end, content: {kind, link, title}}
        _ -> pure Nothing

parseEntries
    :: Dom.Element.Element -> Effect (Array (Entry Date.JSDate Info))
parseEntries schedule = do
    trs <- querySelectorAll (QuerySelector "tr")
            (Dom.Element.toParentNode schedule) >>=
        Dom.NodeList.toArray
    map Array.catMaybes $
        traverse parseEntry $ Array.mapMaybe Dom.Element.fromNode trs

main :: Effect Unit
main = do
    -- Find schedule table.
    window <- Html.window
    doc <- Html.Window.document window
    schedule <- querySelector
        (QuerySelector ".schedule") (Html.Doc.toParentNode doc) >>=
        maybe (throw "no schedule") pure

    -- Parse calendar from schedule table.
    calendar <- calendarFromEntries <$> parseEntries schedule

    -- Remove original content.
    Dom.ParentNode.children (Dom.Element.toParentNode schedule) >>=
        Dom.HTMLCollection.toArray >>=
        traverse_ (Dom.Element.toChildNode >>> Dom.ChildNode.remove)

    -- Add rendered content.
    rendered <- calendarRender (Html.Doc.toDocument doc) calendar
    for_ rendered $ \c -> Dom.Node.appendChild
        (Dom.Element.toNode c) (Dom.Element.toNode schedule)
