module Router (initRouter) where

import Dict (Dict)
import Dict
import Graphics.Input as I

import Html (Html)
import Html
import Window

import Types (..)

onRoute : Route -> Signal Route -> Signal Bool
onRoute r routes = (\r' -> r' == r) <~ routes

initRouter : Route -> [RoutePattern] -> Signal Html
initRouter start signals =
    let input = I.input start
        activeHtml (route, htmlFn) =
            let predicate     = onRoute route input.signal
                html          = htmlFn input.handle
                changes       = merge (count predicate) (count html)
                activeChanges = keepWhen predicate 0 changes
            in sampleOn activeChanges html
        htmlSignals    = map activeHtml signals
        signalSwitcher = merges htmlSignals
    in signalSwitcher
