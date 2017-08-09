-- Functional version of Elm Action/Model/View/Update architecture, but additionally powered by Haskell typeclasses, monad transformers, and lens.
-- #Features
-- ## Composable widgets
-- Larger widgets can be made out of smaller widgets without changing any existing code, or "lifting of" states.
-- ## Easily embed widget
-- Use of lens and prisms to embed a smaller widget Action &Model within larger widget Action & Model.
-- ## Typeclasses and Monad Transformers
-- Using Haskell typeclasses and monad transformer enables a disciplined and lawful way of composing widgets and effects together.
-- ## Orthogonal wiring
-- Unlike other GUI frameworks, the signal framework (how the widgets interact with other stateful effects) are not fixed by this library. I recommend using the pipe ecosystem, but you could probably use conduit or manually run the StateT transformer yourself.
-- ## Combine multiple concurrent stateful effects
-- I recommend using the pipe ecosystem the signal framework because [pipes-fluid](https://github.com/louispan/pipes-fluid) allows combining multiple concurrent stateful effects whilst maintaining a single source of truth. This is possible because the stateful effects are running over [STM](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html) which will ensure a consistent ordering of stateful effects. Haskell is
-- ## Isolation of IO
-- The  stateful effects are pure and do not involve IO. All IO effects are isolated to an interpreter of the command output of the gadget. This has the benefit of allowing better testing of the intention of gadgets; increasing confidence of the behaviour of the gadget,  reducing the surface area of IO misbehaviour.
-- # Examples
-- ## TodoMVC
-- This is a fully featured TodoMVC in in Haskell and ReactJS using the [glazier-react](https://github.com/louispan/glazier-react) library.
-- For a live demo, see https://louispan.github.io/glazier-react-examples/
-- For more details, see the [todo example README.md](https://github.com/louispan/glazier-react-examples/tree/master/examples/todo)
-- # Slides
-- See [slides](https://github.com/louispan/glazier-react-intro)
module Glazier
    ( module Glazier.Gadget
    , module Glazier.Window
    ) where

import Glazier.Gadget
import Glazier.Window
