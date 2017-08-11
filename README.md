[![Hackage](https://img.shields.io/hackage/v/glazier.svg)](https://hackage.haskell.org/package/glazier)
[![Build Status](https://secure.travis-ci.org/louispan/glazier.png?branch=master)](http://travis-ci.org/louispan/glazier)

Functional version of Elm Action/Model/View/Update architecture, but additionally powered by Haskell typeclasses, monad transformers, and lens.

# Features

## Composable widgets
In Haskell, we can use lens, prisms and typeclasses to embed a smaller widget within a larger widget; without modifying any existing widget code or manual ["lifting of state"](https://facebook.github.io/react/docs/lifting-state-up.html).

### Easily convert widgets with lens
The  `Control.Lens.TH` (original [inspiration from arianp](https://arianvp.me/lenses-and-prisms-for-modular-clientside-apps/)) can be used to automatically generate lens and prisms.

The lens and prisms can be used with `Control.Lens`'s [`magnify`](https://hackage.haskell.org/package/lens/docs/Control-Lens-Zoom.html#v:magnify) and [`zoom`](https://hackage.haskell.org/package/lens/docs/Control-Lens-Zoom.html#v:zoom) to convert widgets of different types to the same type.

### Typeclasses for disciplined composition
Using Haskell typeclasses enables a disciplined and lawful way of composing widgets and effects together.
The `Monoid` typeclass can be used to simply append rendering, or to merge stateful effects.
The `Monad` typeclass can be used for complex combinations like creating a list of widgets.

### Monad Transformers for additional pure effects
Monad transformer allow stacking additional effects lawfully. This can be used to enable a blaze/lucid-style `do` notation for rendering; or stacking stateful effects over STM to allow concurrent but consistent stateful changes.

## Isolation of IO
The  stateful effects are pure and do not involve IO. All IO effects are isolated to an interpreter of the command output of the gadget,  reducing the surface area of IO misbehaviour.

All widget stateful effects are pure, increasing confidence of the understanding of the behaviour of the widgets, and allowing easier testing.

## Orthogonal wiring
Unlike other GUI frameworks, the signal framework (how the widgets interact with other stateful effects) are not fixed by this library. I recommend using the pipe ecosystem, but you could probably use conduit or manually run the StateT transformer yourself.

## Combine multiple concurrent stateful effects
Even though the signal framework is not fixed, I recommend using the pipe ecosystem the signal framework because the [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad from [`pipes-fluid`](https://github.com/louispan/pipes-fluid) allows combining multiple concurrent stateful effects whilst maintaining a single source of truth.

This is possible because because the stateful effects are running in a `StateT` transformer stack over [STM](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html) which will ensure a consistent ordering of stateful effects.

AFAIK, Haskell is the only language where this is possible (where there is both STM and monad transformer stack).

# Examples

## TodoMVC
This is a fully featured TodoMVC in in Haskell and ReactJS using the [glazier-react](https://github.com/louispan/glazier-react) library.

For a live demo, see https://louispan.github.io/glazier-react-examples/

For more details, see the [todo example README.md](https://github.com/louispan/glazier-react-examples/tree/master/examples/todo)


# Slides

See [slides](https://github.com/louispan/glazier-react-intro)

# Architecture

This framework makes it easier to modularize the Elm architecture idea of View/Update, based on the [deprecated Elm Architecture version of Jan 2016](https://github.com/evancz/elm-architecture-tutorial/tree/de5682a5a8e4459aed4637533adb25e462f8a2ae)

We are able to super-power the Elm architecture when using Haskell, because Haskell is just awesome.

## Model
In the original Elm Architecture, there is the model with data for logic and rendering. It is usually a record product type.
```haskell
data Model = Model { foo :: Int, bar :: Int }
```
`Glazier` uses `Control.Lens` and `TemplateHaskell` to generate lens for the model with [`makeClassy`](https://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeClassy).

The lens comes in useful later to embed the model in larger models using  `Control.Lens`'s [`magnify`](https://hackage.haskell.org/package/lens/docs/Control-Lens-Zoom.html#v:magnify) and [`zoom`](https://hackage.haskell.org/package/lens/docs/Control-Lens-Zoom.html#v:zoom).
```haskell
makeClassy ''Model
-- The above generates 'HasModel' class with 'foo' and 'bar' lens
```
## View
In the original Elm Architecture, there is a way to render the model:
```haskell
view :: Model -> Html
```
In the Elm documentation, you will sometimes see this version:
```haskell
view : Signal.Address Action -> Model -> Html
```
where `Signal.Address Action` gives the renderer of Html knowledge of how to dispatch `Action` events.

However,  the `Signal.Address Action` parameter is actually not the essential for rendering. It is actually just one of zero or many possible configuration parameters that renderer might need to render something. Basically, at a fundamental level, after all the configuration parameters are partially applied away, the core function is `view :: Model -> Html`.

### View renamed to Window
This `view` function is really just a specialized form of the `ReaderT` transformer.
```haskell
view ::                    Model -> Html
view ::             Reader Model Html
view :: Monad m => ReaderT Model m Html
view :: Monad m => ReaderT Model (ReactMlT m) ()
```
In `Glazier`, this is wrapped in a newtype [`WindowT`](https://github.com/louispan/glazier/blob/master/src/Glazier/Window.hs) to allow for custom `Semigroup` and [`Monoid`](https://github.com/louispan/glazier/blob/1b5330be4f2a42c8a86863a8355489bf3a5837f5/src/Glazier/Window.hs#L97) instances. I don't know why the [`transformers`](https://hackage.haskell.org/package/transformers) package don't have these instances by default.
```haskell
newtype WindowT s m v = WindowT
    { runWindowT :: ReaderT s m v
    } deriving ( MonadReader s
               , Monad
               , Applicative
               , Functor
               , MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO )
```

One of the benefits of a monad transformer the ability to stack on top of other monadic effects. For example, in [glazier-react](https://github.com/louispan/glazier-react),  lucid/blazehtml-inspired `do` notation can be used to build up the React elements for rendering by using a StateT monad called  [`ReactMlT`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Markup.hs)

### Composing Windows
You can combine windows using `Monoid` like [diagrams](http://projects.haskell.org/diagrams/)
```haskell
labelWindow <> textInputWindow
```
Or in more complex ways using `Monad` to render a list of another widget.
```haskell
bh “div” [] $ do
   bh “label” [] “hello”
   lf “input” […]
   traverse_ (view _WindowT todoWindow) todos
```
### Magnifying windows
`Window`s have to be the same `model` type to compose together.
We can use `Control.Lens.magnify` to modify the `model` type to transform different `Window`s into the same type.
```haskell
data TodoModel = …
data AppModel = AppModel { _title :: Text
                         , _todo :: TodoModel }
makeClassy ‘‘AppModel
todo :: Lens AppModel TodoModel

todoWindow :: ReaderT TodoModel m r

todoToAppWindow :: ReaderT AppModel m r
todoToAppWindow = magnify todo todoWindow
```

## Action
In Elm, there are events that can happen in to the app. This is a sum type.
```haskell
data Action = DoThis | DoThat
```
We can use `Control.Lens` and `TemplateHaskell` to generate prisms for the`Action` with [`makeClassyPrisms`](https://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeClassyPrisms).

The prisms comes in useful later to embed the action in larger widgets using  `Control.Lens`'s [`magnify`].

```haskell
data Action = DoThis | DoThat
makeClassyPrisms ''Action
-- The above generates 'AsAction' class with '_DoThis' and '_DoThat' prisms
```

## Update
In Elm, the the app reacts to `Action` events which may modify the model.
```haskell
update :: Action -> Model -> Model
```
More complicated widgets also produce a result which is interpreted with effects (possibly IO).
```haskell
update : Action -> Model -> (Model, Effects Action)
```
### Update renamed to Gadget
The Elm `update` function is just a specialized form of `ReaderT action StateT model`. 
```haskell
update :: Action -> Model -> Model
update :: Action -> Model -> (Model, Commands)
update :: Action -> State Model Commands
update :: ReaderT Action (State Model) Commands
update :: ReaderT Action (StateT Model m) Commands
update :: ReaderT Action (StateT Model STM) Commands
```
In `Glazier`, this is wrapped in a newtype [`GadgetT`](https://github.com/louispan/glazier/blob/master/src/Glazier/Gadget.hs) to allow for custom `Semigroup` and [`Monoid`](https://github.com/louispan/glazier/blob/1b5330be4f2a42c8a86863a8355489bf3a5837f5/src/Glazier/Gadget.hs#L97) instances. I don't know why the [`transformers`](https://hackage.haskell.org/package/transformers) package don't have these instances by default.
```haskell
newtype GadgetT a s m c = GadgetT
    { runGadgetT :: ReaderT a (StateT s m) c
    } deriving ( MonadState s
               , MonadReader a
               , Monad
               , Applicative
               , Functor
               , MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO )
```

Again the benefit of the monad transformer is the ability to stack monadic effects. In this case, the gadget stateful effects can be stack on to of STM. This means that we are now able to combine multiple stateful effects and yet maintain a consistent order of stateful effects. This is only possible with a transformer stack over STM and Haskell and is yet another reason why Haskell is just awesome.

#### Command interpreter
The result of gadget is `Command`, which can be interpreted effectfully. This is the only place where IO is allowed.

#### Composing Gadget
You can combine gadgets using `Monoid`.
```haskell
labelGadget <> textInputGadget
```
You can use `Monad` to make a gadget of a list (actually a `Map k v`) of gadgets
```haskell
listGadget = do
    -- converts original Action to (key, Action)
    (k, _) <- ask
    -- Apply original item gadget logic inside a Map using lens `at`
    -- and annotate resulting command with the key used.
    (fmap (\cmd -> (k, cmd)) <$>
        magnify (to snd) $
        zoom (at k . _Just) itemGadget
```
#### Magnifying gadget
`Gadgets`s have to be the same `action` and  `model` type to compose together.
We can use `Control.Lens.magnify` to modify the `action` type, and `Control.Lens.zoom` to modify the `model` type, to transform different `Gadget`s into the same type.
```haskell
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

## Wiring
In Elm there is a bootstrap `startApp` that takes care of the wiring. The Html knows how to send `Action`s to a mailbox `Signal Address`, which is then dispatched to the `update` function. 

In `Glazier`there is no `startApp` equivalent, since there is no wiring framework that will work for all application requirements, whereas Elm is web-browser centric, and web-browsers are single threaded.

However, this means you can use any Haskell library/techniques/concurrent threads as appropriate for your specific use case.

The following is my recommendation for wiring:
* Use [Pipes.Concurrent](https://hackage.haskell.org/package/pipes-concurrency/docs/Pipes-Concurrent.html) for sending and receiving `Action`s from the view/gui.
* Dispatch the `Action` from the `Pipes.Concurrent.Input` into the application gadget.
    * The [todo example](https://github.com/louispan/glazier-react-examples/blob/86942dc761c1ad4849f0133eca2c4e45a8b3fff0/examples/todo/haskell/app/Main.hs#L109) is an example of using a single `Input`.
* Use multiple threads with multiple `Input` and `Output` as required.
* Use [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad from [`pipes-fluid`](https://github.com/louispan/pipes-fluid) to combine multiple `Input` together safely and consistently.
    * [glazier-tutorial](https://github.com/louispan/glazier-tutorial/blob/6f9bf9e78b9eb70282a32270ad2851e8bf25d457/src/Glazier/Tutorial/Console.hs#L444) has an example of using multiple `Input`s.

## Widget
In `Glazier` there is no separate data type for `Widget`. It is just a word for the combination of `Window` rendering and `Gadget` state changes, similar to the original [etymology](https://en.wikipedia.org/wiki/Widget_(GUI)#Etymology).

# Glazier Benefits
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
* `Window` is just a newtype wrapper around `ReaderT`
* `Gadget` is just a newtype wrapper around `ReaderT` & `StateT`
* Pipes & Producers are an existing tested and lawful concept
* Input/Output (eg mailbox/address) are just newtype wrappers around STM
* For animation/multiple Inputs there is a new [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad from [`pipes-fluid`](https://github.com/louispan/pipes-fluid)
* Really just one new [Impulse](https://github.com/louispan/pipes-fluid/blob/master/src/Pipes/Fluid/Impulse.hs) monad… if you want animation.

# Glazier.React
[`Glazier.React`](https://github.com/louispan/glazier-react) contains efficient bindings to React JS where render will only be called for the react components with changed states.

## Galzier.React.Widgets
[`Glazier.React.Widgets`](https://github.com/louispan/glazier-react-widget) contains a library of reusable widgets. For example, [List Widget](https://github.com/louispan/glazier-react-widget/blob/master/src/Glazier/React/Widgets/List.hs) creates a list of any other widget. Please help me add more widgets to this library!

# Changelog

* 0.12.0.0
  - Combined WindowT and GadgetT into a single newtype GizmoT.
  - GizmoT contains MaybeT effects for a consistent Alternative and MonadPlus instance.
