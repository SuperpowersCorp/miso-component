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
messages to be sent back to the parent's update function
* runs all messages through the reaction function you specify

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

* `initialModel` -- so `C.initialModel
timer1Comp` would give you the timer's model.
* `addInitialAction` -- mappends a component's initial action as a
parent `Effect`
* `view` -- `C.view m timer1Comp` (where m is parent's model) can be
called in the parent's view to display the child's view. The child's
view actions are automatically mapped to the parent.
* `subs` -- `C.subs timer1Comp` gives you a list of the child's
  subs converted to parent subs, so you can just `++` them to the
  parent's subs list.

## Maps

It's sort of a hastle, but also very useful, to put components inside
of a map. To ease the pain, we made the `Miso.Component.Map` library.
You can see a demo of using it in `/demo/src/Demo/Map.hs`.
