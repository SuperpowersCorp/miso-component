# Miso-Component

This is a library that helps you pretty-easily combine separate Miso
"components" together. A component is just a regular Miso `App`
(the type you pass to `startApp`) that is combined with a parent by
specifying a lens that points to the child model, and a reaction
function that allows the parent to easily react to any child actions.

Since this library lets you nest `App`s easily, you can have a model,
action, update, view, and subs devoted to each component and easily
reuse them in other apps.

We also provide easy ways to create Maps of components as well as
union sum types of varying components (i.e. for page navigation).

## Quick Code Summary/Demo

```
import qualified Miso.Component as C
import qualified Demo.Component.Timer as Timer
```

Here, Timer is just a regular Miso `App` that works like a simple
kitchen timer. The timer Model must be declared in the map of the parent model:

```
data Model = Model
  { _buzzcount :: Int
  , _timer1 :: Timer.Model
  }
makeLenses ''Model
```

Then wrap an Action around the timer's action:

```
data Action = NoOp
            | Timer1 Timer.Action
```

Then define a component:

```
timer1Comp :: Component Action Model Timer.Action Timer.Model
timer1Comp = C.Component {
    app = Timer.app 20
  , converter = Timer1
  , lens = timer1
  }
```

(`Timer.app` happens to take an `Int` as an argument to
create the `App`)

Now use the `C.updater` function in the parent's update function to
handle the timer's actions:

```
updateModel NoOp m = return m
updateModel (Timer1 a) m = C.updater timer1Comp m a C.noReaction
```

`C.updater` does the following:
* runs the child component's update function on the
action
* updates the child model in the parent model
* converts any new actions generated by the child's update into parent
actions to be sent back to the parent's update function
* runs all child actions through the reaction function you specify

The type signature for a reaction function is:

```
childAction -> childModel -> parentModel -> Effect parentAction parentModel
```

So you can case match on the child action and use that info to update
the parent model and/or generate new effects in the parent. Let's say
we want to increment `buzzcount` in the parent model whenever the
timer emits a `Timer.Buzz` message:

```
updateModel (Timer1 a) m = C.updater timer1Comp m a $ \ca cm pm ->
  case ca of
  Timer.Buzz -> return $ pm & buzzcount %~ (+1)
  _ -> return pm
```

(Notice that `pm` will have the updated child component in it, so
it's important not to return `m` instead of `pm`, unless you want to
purposefully undo a child action's effect on its model)

There are four other convenience functions for incorporating the
component into the parent:

### initialModel
```
initialModel :: Component pa pm ca cm -> cm
```
Grabs the initial model out of any component.

### addInitialAction
```
addInitialAction :: Effect pa pm -> Component pa pm ca cm -> Effect pa pm
```
Use this to mappend a component's initial actions to an effect. In the
parent, set an initial action to something, say, `Init`, then in the
your `updateModel`:

```
updateModel Init m = noEff m
                     `C.addInitialAction` component1
                     `C.addInitialAction` component2
```

### view
```
view :: pm -> Component pa pm ca cm -> View pa
```

`view` takes the parent's current model and the component and gives
you back a view of the component that returns a parent's action. So
you can use it in the parent's `viewModel`:

```
viewModel m = div_ []
  [ button_ [ onClick SomeParentAction ] [ text "click here" ]
  , C.view m someComponent
  ]
```

### subs
```
subs :: Component pa pm ca cm -> [Sub pa pm]
```
Returns a list of the component's subs, mapped properly as parent
subs, so you can just append them to the parent's subs:

```
app :: App Model Action
app = App { ...
          , subs = [ someParentSub ]
                   ++ C.subs component1
                   ++ C.subs component2
          }
```

## Components in Union Types

Oftentimes you might want to have a variety of components in a union
type. For instance, you could make each page of your site a component
and wrap them all up in one union type that can be selected by a page
navigation.

For example, let's say we have a `Page1` component, a `Page2`
component, and an `ErrorPage` that just tells you there was an error,
but isn't really a component. They can be represented as a union type:

```
data Pages = ErrorPage
             | Page1 Page1.Model
             | Page2 Page2.Model
```

Recall that you have to specify a Lens to get and set the model of
each component, but in this case you can't be sure if the component
you want is really there or not since `Pages` might be an `ErrorPage`
or `Page1` when you want `Page2`, etc.

Really, it's more like you'd want to set a Prism to access the model
instead of a Lens, and if the wanted model isn't there at the moment,
just ignore any actions/subs aimed at that component.

To do this, we can transform the component's `App` to take the
union type its inside of as a model instead of the child model itself,
then only update itself if the union type is the correct variation.

This `prismify` function does this automatically to any `App`:

```
prismify :: (m -> s) -> (s -> Maybe m) -> App m a -> App s a
```

The first two functions are essentially `review _SomePrism` and
`preview _SomePrism`. `s` is the sum/union type and `m` is the component's
model. If your app is already importing `Control.Lens`,
you can make a prismify that just takes a Prism:

```
prismify :: Prism' s m -> App m a -> App s a
prismify p = C.prismify (review p) (preview p)
```

Back to the `Pages` example... Supposing the `Pages`
type is stored in the parent model under the lens `pages`, and there's
an action in the parent action `HandlePage1Action Page1.Action`, to
make a component for `Page1` using the Prism `prismify`
defined above:

```
page1Comp :: Component Action Model Page1.Action Pages
page1Comp = C.Component {
    app = prismify _Page1 $ Page1.app
  , converter = HandlePage1Action
  , lens = pages
  }
```

Notice that the model type of this component is `Pages` instead of
`Page1.Model`. You can treat incorporate this just like a regular
component, defining an `updater` and reaction for `HandlePage1Action`,
and you can define a component for each of the union type constructors.
However, remember that things like `C.initialModel` will return the
whole union type, so to set the initial page to `Page1`, you could
just set `pages = C.initialModel page1Comp`. Also, if the pages have
initial actions, you should add them on to whatever Effect in the
parent model switches to a new page.

See a full example of using `prismify` in `demo/src/Prism.hs`

## Maps

The other thing that's often really useful but tediously difficult is
to store components dynamically inside a Map. To help with this, we
made the `Miso.Component.Map` module.

To use it, import it and declare a standard Haskell Map in your parent model:
```
import Data.Map (Map)
import qualified Miso.Component.Map as CMap

data Model = Model { timers :: Map Int Timer.Model }
```

Then declare a timer map component:

```
timersComp :: Component Action Model
              (CMap.Action Int Timer.Action Timer.Model)
              (Map Int Timer.Model)
timersComp = C.Component {
    app = CMap.app $ Timer.app 15
  , converter = HandleTimersAction
  , lens = timers
  }
```

Now you can use `timersComp` to get `subs`, a `view`, and to
declare an `updater` function that automatically delegates actions to
the correct model in the map. The Map Component acts as a sort of
middle-man between the parent model and its child components.

Here are some helpful functions for working with the map of components:
* `add_`, `addWithModel`, `addWithAction`, and `add` generate
parent actions for that add a new component to the map.
* `remove` and `remove_` delete a component from the map and
  optionally let you specify a final action for the child component to
  complete in its last dying gasp.
* `send` lets to send child actions to specific components in the map
* `viewMap` returns a map of `View`s for each component that can be displayed in the
  parent model. It also allows you to wrap a `View` around each
  component so you can easily add parent controls for each, like a
  delete button.

The `RecvAction index childAction childModel` action can be pattern
matched in the reaction function for the map component to snoop and
react to any messages happening in the components of the map. For
example, to send out a `Log` action (which is a Parent action)
whenever a Timer buzzes:

```
updateModel (HandleTimersAction a) = C.updater timersComp m a $ \ca' _ m' -> case ca' of
    CMap.RecvAction k ta _tm ->
      case ta of
      Timer.Buzz -> m' <# do
        return . Log $ ms (show k) <> " is BUZZZING!!!!!!"
      _ -> return m'
    _ -> return m'
```

You can see a full demo of using it in `/demo/src/Demo/Map.hs`.


## Concise

The `Miso.Concise` module is a variation of `Miso.Component` that
needs less boilerplate but requires that you put a function in the
parent's `Action`, which means you can't derive `Show` for it, which
makes some people sad.

To use it, add an action constructor to the parent Action type
that takes a function `Model -> Effect Action Model`. Then in the
`updateModel` function, whenever you get that action, run the function
on the current model.

This means that you only need to add one extra action and it can
handle any number of components, instead of having to add one action
per component. But it's worse for debugging probably runs a little slower,
so I'd recommend using the `Miso.Component` module instead.

You can see a demo of using `Concise` in `demo/src/Concise.hs`.
