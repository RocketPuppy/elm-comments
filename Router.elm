module Router where

import Dict (Dict)
import Dict
import Graphics.Input as I

import Html (Html)
import Html
import Window

routeInput = I.input "titles"

type Route = (Signal Bool, Signal Html)

initRouter : [Route] -> Signal Html
initRouter signals =
    let f (p, s) = sampleOn (keepWhen p 0 (merge (count s) (count p))) s
    in merges << map f <| signals
