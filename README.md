# excelsior

Excelsior uses [pure](https://github.com/grumply/pure) `Components` to globaly share state.

## Counter

```haskell
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Main where

import Pure
import Excelsior

import Control.Monad (void)

data Counter = Counter !Int

data CountCmd = Inc | Dec
instance Command Counter CountCmd

counter Inc (Counter n) = Counter (n + 1)
counter Dec (Counter n) = Counter (n - 1)

data LogCmd = Log
instance Command Counter LogCmd

logger continue Log (Counter n) = print n >> return (Counter n)

data Count = Count
instance Pure Count where
  view = ComponentIO $ \self -> def
    { construct = return 0
    , executing = void $ watch' $ \(Counter n) -> modify_ self $ \_ _ -> n
    , render = \_ n -> 
        Div <||> 
          [ text n
          , Button <| OnClick (\_ -> command Inc) |> [ "Increment" ]
          , Button <| OnClick (\_ -> command Dec) |> [ "Decrement" ]
          , Button <| OnClick (\_ -> command Log) |> [ "Log Count" ]
          ]
    }

main = inject body $ 
  Div <||>
    [ View $ Excelsior (Counter 0) [ reducer counter ] [ middleware logger ]
    , View Count
    , View Count
    ]
```