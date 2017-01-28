[![Hackage](https://img.shields.io/hackage/v/glazier.svg)](https://hackage.haskell.org/package/glazier)
[![Build Status](https://secure.travis-ci.org/louispan/glazier.png?branch=master)](http://travis-ci.org/louispan/glazier)

Functional version of (Elm Update/View & startApp architecture) enabling composable widgets, and a FRP-like framework.

See [slides](http://www.slideshare.net/LouisPan3/composable-widgets-with-reactive-pipes)

See [Example.hs](src/Example.hs) for example usage.

See [glazier-tutorial](https://github.com/louispan/glazier-tutorial/blob/08d24800c58c6ec683b618bf2e4061e58aac1753/src/Glazier/Tutorial/Console.hs#L193) for a working console example.

This framework makes it easier to modularize the Elm architecture idea of View/Update:
based on the [deprecated Elm Architecture version of Jan 2016](https://github.com/evancz/elm-architecture-tutorial/tree/de5682a5a8e4459aed4637533adb25e462f8a2ae)

The Elm View/Update is basically as follows:

```
data Model = Blah....
data Action = DoThis | DoThat deriving Show

-- | update is fired from an event processing loop
update :: Action -> Model -> Model

-- | The widget from 'view' knows how to send Action to a mailbox
view :: Signal Address -> Model -> Html
```

This module uses isomorphic implementations Update and View resulting in instances that can be be composed together into larger Widgets.
Original inspiration from https://arianvp.me/lenses-and-prisms-for-modular-clientside-apps/

This framework provides three main combinators:
* Semigroup and Monoid instances for concatenating widgets.
* 'dispatch' is used to re-route the action type.
* 'implant' is used to modify the model type.
