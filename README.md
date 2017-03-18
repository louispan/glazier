[![Hackage](https://img.shields.io/hackage/v/glazier.svg)](https://hackage.haskell.org/package/glazier)
[![Build Status](https://secure.travis-ci.org/louispan/glazier.png?branch=master)](http://travis-ci.org/louispan/glazier)

Functional version of Elm Action/Model/View/Update architecture, but additionally powered by Haskell typeclasses, monad transformers, and lens.

#Features

## Composable widgets
Larger widgets can be made out of smaller widgets without changing any existing code, or "lifting of" states.

## Easily embed widget
Use of lens and prisms to embed a smaller widget Action &Model within larger widget Action & Model.

## Typeclasses and Monad Transformers
Using Haskell typeclasses and monad transformer enables a disciplined and lawful way of composing widgets and effects together.

## Orthogonal wiring
Unlike other GUI frameworks, the signal framework (how the widgets interact with other stateful effects) are not fixed by this library. I recommend using the pipe ecosystem, but you could probably use conduit or manually run the StateT transformer yourself.

## Combine multiple concurrent stateful effects
I recommend using the pipe ecosystem the signal framework because [pipes-fluid](https://github.com/louispan/pipes-fluid) allows combining multiple concurrent stateful effects whilst maintaining a single source of truth. This is possible because the stateful effects are running over [STM](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html) which will ensure a consistent ordering of stateful effects.

## Isolation of IO
The  stateful effects are pure and do not involve IO. All IO effects are isolated to an interpreter of the command output of the gadget. This has the benefit of allowing better testing of the intention of gadgets; increasing confidence of the behaviour of the gadget,  reducing the surface area of IO misbehaviour.


# Examples

## TodoMVC
This is a fully featured TodoMVC in in Haskell and ReactJS using the [glazier-react](https://github.com/louispan/glazier-react) library.

For a live demo, see https://louispan.github.io/glazier-react-examples/

For more details, see the [todo example README.md](https://github.com/louispan/glazier-react-examples/tree/master/examples/todo)

# Slides

See [slides](https://github.com/louispan/glazier-react-intro)

# Architecture

## Original Elm architecture
This framework makes it easier to modularize the Elm architecture idea of View/Update, based on the [deprecated Elm Architecture version of Jan 2016](https://github.com/evancz/elm-architecture-tutorial/tree/de5682a5a8e4459aed4637533adb25e462f8a2ae)

The Elm View/Update is basically as follows:
### Model
There is the model with data for logic and rendering. It is usually a record product type.
```
data Model = Model { foo :: Int, bar :: Int }
```
### View
There is a way to render the model
```
view :: Model -> Html
```
### Action
These are the events that can happen in to the app. This is a sum type.
```
data Action = DoThis | DoThat
```
### Update
The app reacts to Action events which may modify the model.
```
update :: Action -> Model -> Model
```
### Update result
More complicated widgets also produce a result which is effectfully interpreted.
```
update : Action -> Model -> (Model, Effects Action)
```

### Wiring
The Elm startApp takes care of the wiring. the Html knows how to send `Action`s to a mailbox `Signal Address`, which is then dispatched to the `update` function.

## Haskelly Elm Architecture
We are able to super-power the Elm architecture when using Haskell, because Haskell is just awesome.

### Model
We can use `Control.Lens` and `TemplateHaskell` to generate lens for the model.
This comes in useful later to embed the model in larger models using  `Control.Lens`'s `magnify` and `zoom`.
```
data Model = Model { _foo :: Int, _bar :: Int }
makeClassy ''Model 
-- The above generates 'HasModel' class with 'foo' and 'bar' lens
```

### View renamed to Window
The Elm `view` function is really just a specialized form of the `ReaderT` transformer. This is wrapped in a newtype `WindowT` to allow for custom Monoid instances.
```
view ::            Model -> Html
view ::     Reader Model Html
view ::    ReaderT Model m Html
view ::    ReaderT Model (ReactMlT m) ()
```
One of the benefits of a monad transformer the ability to stack on top of other monadic effects. For example, in [glazier-react](https://github.com/louispan/glazier-react),  lucid/blazehtml-inspired `do` notation can be used to build up the React elements for rendering by using a StateT monad called  [`ReactMlT`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Markup.hs)

#### Composing Windows
You can combine windows using `Monoid` like [diagrams](http://projects.haskell.org/diagrams/)
```
labelWindow <> textInputWindow
```
Using `Monad` and `Foldable` to render a list of another widget.
```
bh “div” [] $ do
   bh “label” [] “hello”
   lf “input” […]
   traverse_ (view _WindowT todoWindow) todos
```
#### Magnifying windows
`Window`s have to be the same `model` type to compose together.
We can use `Control.Lens.magnify` to modify the `model` type to transform different `Window`s into the same type.
```
data TodoModel = …
data AppModel = AppModel { _title :: Text
                         , _todo :: TodoModel }
makeClassy ‘‘AppModel
todo :: Lens AppModel TodoModel

todoWindow :: ReaderT TodoModel m r

todoToAppWindow :: ReaderT AppModel m r
todoToAppWindow = magnify todo todoWindow
```

### Action
Likewise, we can use `Control.Lens` and `TemplateHaskell` to generate prisms for `Action`.
This comes in useful later to embed the action in larger widget using  `Control.Lens.magnify`.
```
data Action = DoThis | DoThat
makeClassyPrisms ''Action
-- The above generates 'AsAction' class with '_DoThis' and '_DoThat' prisms
```
### Update renamed to Gadget
The Elm `update` function is just a specialized form of `ReaderT action StateT model`. This is wrapped in a newtype `GadgetT` to allow for custom Monoid instances.
```
update :: Action -> Model -> Model
update :: Action -> Model -> (Model, Commands)
update :: Action -> State Model Commands
update :: ReaderT Action (State Model) Commands
update :: ReaderT Action (StateT Model m) Commands
update :: ReaderT Action (StateT Model STM) Commands
```
Again the benefit of the monad transformer is the ability to stack monadic effects. In this case, the gadget stateful effects can be stack on to of STM. This means that we are now able to combine multiple stateful effects and yet maintain a consistent order of stateful effects. This is only possible with STM (not with IO) and is yet another reason why Haskell is just awesome.

#### Command interpreter
The result gadget is `Command`, which can be interpreted effectfully. This is the only place where IO is allowed.

#### Composing Gadget
You can combine gadgets using `Monoid`.
```
labelGadget <> textInputGadget
```
Using `Monad` to make a gadget of a list of gadgets
```
listGadget = do
    (k, _) <- ask
    (fmap (\cmd -> (k, cmd)) <$>
        magnify (to snd) $
        zoom (at k . _Just) itemGadget
```
#### Magnifying gadget
`Gadgets`s have to be the same `action` and  `model` type to compose together.
We can use `Control.Lens.magnify` to modify the `action` type, and `Control.Lens.zoom` to modify the `model` type, to transform different `Gadget`s into the same type.
```
data AppAction
 = ForceRender
 | DestroyTodoAction Int
 | InputAction Input.Action
 | TodosAction (Map Int Todo.Action)

data AppModel = AppModel
 { _seqNum :: Int
 , _todoInput :: Input.Model
 , _todosModel :: Map Int Todo.Model
 }

gadget :: ReaderT Input.Action (StateT Input.Model m) [InputCmd]

gadget’ :: ReaderT AppAction (StateT AppModel m) [AppCmd]
gadget’ = fmap foo <$> zoom todoInput (magnify _InputAction gadget)
```

### Wiring
Unlike the original Elm architecture, there is no `startApp` equivalent, since that is single-threaded & web-browser centric.
However, this means you can use any Haskell library/techniques appropriate for your specific use case.
The following is my recommendation for wiring:
* Use [Pipes.Concurrent](https://hackage.haskell.org/package/pipes-concurrency/docs/Pipes-Concurrent.html) for sending and receiving `Action`s from the view/gui.
* Dispatch the `Action` from the `Pipes.Concurrent.Input` into the application gadget.
    * The [todo example](https://github.com/louispan/glazier-react-examples/blob/86942dc761c1ad4849f0133eca2c4e45a8b3fff0/examples/todo/haskell/app/Main.hs#L109) is an example of using a single `Input`.
    * [glazier-tutorial](https://github.com/louispan/glazier-tutorial/blob/6f9bf9e78b9eb70282a32270ad2851e8bf25d457/src/Glazier/Tutorial/Console.hs#L444) has an example of using multiple `Input`s.

# Why Glazier
## Windows are composable
* `Window` is the more disciplined way of representing rendering functions.
* `Window`s of the same type can be combined using `Monoid` and `Monad`
* `Window`s of different types can be magnified into the same type.

## Gadgets are composable
* `Gadget` is the more disciplined way of representing action-activated state effects.
* `Gadget` of the same type can be combined using `Monoid` and `Monad`
* `Gadget`s of different types can be zoomed and magnified into the same type

## Wiring is also composable
* Multiple mailboxes/Inputs and gadget state effects can be composed using  [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad from [`pipes-fluid`](https://github.com/louispan/pipes-fluid).
* Also Pipe instances for Arrow, ArrowChoice in [pipes-cateory](https://github.com/louispan/pipes-category) if you want to wiring things together using arrow notation.

## Simple to understand
* `Window` is just a newtype wrapper around ReaderT
* `Gadget` is just a newtype wrapper around ReaderT & StateT
* Pipes & Producers are an existing tested and lawful concept
* Input/Output (eg mailbox/address) are just newtype wrappers around STM
* For animation/multiple Inputs there is a new [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad from [`pipes-fluid`](https://github.com/louispan/pipes-fluid)
* Really just one new [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad… if you want animation.

# Glazier.React
[`Glazier.React`](https://github.com/louispan/glazier-react) contains efficient bindings to React JS where render will only be called for the react components with changed states.

## Galzier.React.Widgets
[`Glazier.React.Widgets`](https://github.com/louispan/glazier-react-widget) contains a library of reusable widgets. For example, [List Widget](https://github.com/louispan/glazier-react-widget/blob/master/src/Glazier/React/Widgets/List.hs) creates a list of any other widget. Please help me add more widgets to this library!
