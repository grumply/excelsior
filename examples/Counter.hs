{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Pure.App hiding (value)
import qualified Pure.App
import Pure.View hiding (Sub, value)

import Excelsior

data Store = Store 
  { value :: Int
  }

data Arith = Add Int | Sub Int deriving Show
instance Store `Command` Arith

reduceArith = reducer $ \state cmd -> 
  case cmd of
    Add n -> state { value = value state + n }
    Sub n -> state { value = value state - n }

loggingMiddleware = middleware $ \_ next cmd -> do
  print (cmd :: Arith)
  next cmd

data Routes = HomeR deriving Eq

main = run App {..}
  where
    key = "my-app"
    build = return
    prime = createStore (Store 0) [reduceArith] [loggingMiddleware]
    root = Nothing
    routes = Pure.App.dispatch HomeR
    pages HomeR = pure $ page _Head _Home

_Head = simple "Head" $
  Head [] []

data HomeState ms = HomeState
  { hsValue :: Int
  }

_Home = Controller {..}
  where
    key = "Home"
    build = return
    prime = excel $ \state _ -> HomeState (value state)
    model = HomeState 0
    view (HomeState val) = 
        let button cmd (txt :: Txt) = Button [ onClick (command cmd) ] [Text txt]
        in Div []
            [ Text val 
            , Br [] []
            , button (Add 1)  "Add 1"
            , button (Add 10) "Add 10"
            , Br [] []
            , button (Sub 1)  "Sub 1"
            , button (Sub 10) "Sub 10"
            , Button [ onClick (commands [toCommand $ Add 10, toCommand $ Sub 1]) ] "Add 9"
            ]

