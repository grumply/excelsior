# Excelsior

This document is a work in progress.

Excelsior is a new global state management library in the style of [Redux](https://redux.js.org) for Pure.  With Pure and Ef's core asynchronous types, Excelsior is implemented as a simple wrapper in approximately 100 lines of code. You can check out the full implementation [here](https://github.com/grumply/excelsior/blob/master/src/Excelsior.hs).

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
* the `createStore` during the priming of `App` with `reduceArith` and `loggingMiddleware`
* the `excel` command to inject changes from the store into the `_Counter` controller
* the use of `command` in `_Counter.view`

## Motivation

Excelsior, and, more generally, Redux-style libraries are, in effect, glorified global state.  But global state has been widely condemned for decades, cf. [c2/GlobalVariablesAreBad](wiki.c2.com/?GlobalVariablesAreBad).  So what makes it okay to use in a Pure application?  Let's lay out the problems with global state management, contrast that with local state management, and see what Excelsior does to ameliorate the issues and find a clean balance between the two.

### Locality vs. Non-Locality

-- TODO: More...

Context is always important when reading code, especially UI-centric code.  

<example>

### Access Control and Coupling

-- TODO: More...

State access across boundaries, especially module boundary, is a critical metric for determining software complexity.  The more disparate the use of state modification, the more difficult it will be to debug issues dealing with flow of control.  Thus arose the [Law of Demeter/principle of least knowledge](wiki.c2.com/?LawOfDemeter); that access limitation via localization of context is critical for creating maintainable and adaptable software.

<example>

### Locking and Blocking

As an extension of access control, the issues imposed by desynchronization of global state can lead to unpredicatable, often difficult to debug, scenarios, including locking in the case of synchronized mutable variables. 

While Excelsior does not prevent you from creating desynchronization issues, following the generally applicable guidelines for Excelsior development can.  Specifically, it is best not to use mutable variables in a `store`.  Instead, rely on the command queue behind the store to control state access; only allow reducers to modify a store's content by always using `command` for store updates.
