# Excelsior

This document is a work in progress.

Excelsior is a global state management library in the style of [Redux](https://redux.js.org) for Pure.  With Pure and Ef's core asynchronous types, Excelsior is implemented as a simple wrapper over observable values. You can check out the full implementation [here](https://github.com/grumply/excelsior/blob/master/src/Excelsior.hs).

## An Example

A simple example will seed context; some extraneous code omitted.

```haskell
data Store = Store 
  { value :: Int
  }

data Arith = Add Int | Sub Int
instance Store `Command` Arith

reduceArith = reducer $ \store cmd -> 
  case cmd of
    Add n -> store { value = value store + n }
    Sub n -> store { value = value store - n }

loggingMiddleware = middleware $ \store next cmd -> do
  print (cmd :: Arith)
  next cmd

main = App {..}
   where
     ...
     prime = createStore (Store 0) [reduceArith] [loggingMiddleware]
     ...

_Counter = Controller {..}
  where
    ...
    prime = excel $ \store counter -> counter { current = value store }
    view Counter {..} =
      let add = command (Add 1)
          sub = command (Sub 1)
      in
        Div []
          [ Button [ onClick add ] "Add 1"
          , Button [ onClick sub ] "Sub 1"
          , Txt current
          ]
```

Of import are: 

* the `Command` instance for `Arith`
* the `Reducer`, `reduceArith`
* the `Middleware`, `loggingMiddleware`
* the `createStore` in `App.prime` with `reduceArith` and `loggingMiddleware`
* the `excel` command to inject changes from the store into the `_Counter` controller
* the `command` calls in `_Counter.view` click handlers


