module Router (initRouter) where

import Graphics.Input (Handle, input)

import Html (Html)

onRoute : a -> Signal a -> Signal Bool
onRoute r routes = (\r' -> r' == r) <~ routes

initRouter : a -> [(a, Handle a -> Signal b)] -> Signal b
initRouter start signals =
    let routeInput = input start
        activeOutput (route, outFn) =
            let predicate     = onRoute route routeInput.signal
                output        = outFn routeInput.handle
                changes       = merge (count predicate) (count output)
                activeChanges = keepWhen predicate 0 changes
            in sampleOn activeChanges output
        outputSignals  = map activeOutput signals
        signalSwitcher = merges outputSignals
    in signalSwitcher
