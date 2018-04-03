## Module Firkin

#### `View`

``` purescript
newtype View eff
```

The (abstract) type of views.

You can create (incremental) functions returning values of this type by
using the `text` and `element` functions.

`View`s can be initially rendered to the DOM using the `render` function.

##### Instances
``` purescript
Patch (View eff) (ViewChanges eff)
```

#### `ViewChanges`

``` purescript
newtype ViewChanges eff
```

The (abstract) type of view updates.

`View`s can be applied to the DOM using the `applyPatch` function.

##### Instances
``` purescript
Semigroup (ViewChanges eff)
Monoid (ViewChanges eff)
Patch (View eff) (ViewChanges eff)
```

#### `text`

``` purescript
text :: forall eff. Jet (Atomic String) -> Jet (View eff)
```

Create a text node wrapped in a `<span>` element.

#### `textWith`

``` purescript
textWith :: forall eff. String -> Jet (Atomic String) -> Jet (View eff)
```

Create a text node wrapped in an element with the specified name.

#### `element`

``` purescript
element :: forall eff. String -> Jet (IMap String (Atomic String)) -> Jet (IMap String (Atomic (EventListener eff))) -> Jet (IArray (View eff)) -> Jet (View eff)
```

Create an element with the given name, attributes, event listeners and
children.

#### `element_`

``` purescript
element_ :: forall eff. String -> Jet (IArray (View eff)) -> Jet (View eff)
```

Create an element with no attributes or event handlers.

#### `render`

``` purescript
render :: forall eff. Node -> View (dom :: DOM | eff) -> Eff (dom :: DOM | eff) Unit
```

Render a `View` to the DOM, under the given `Node`, and connect any
event listeners.

Once the initial `View` is rendered, the DOM can be updated using the
`applyPatch` function.

#### `applyPatch`

``` purescript
applyPatch :: forall eff. Element -> View (dom :: DOM | eff) -> ViewChanges (dom :: DOM | eff) -> Eff (dom :: DOM | eff) Unit
```

Apply a set of `ViewChanges` to the DOM, under the given `Node`, which should
be the same as the one initially passed to `render`.

The second argument is the _most-recently rendered_ `View`, i.e. the one which
should correspond to the current state of the DOM.

_Note_: in order to correctly remove event listeners, the `View` passed in
must contain the same event listeners as those last attached, _by reference_.
In practice, this means that the `View` passed into this function should be
obtained using the `patch` function.

See the implementation of the `tap` function for an example.

#### `tap`

``` purescript
tap :: forall model change eff. Patch model change => Element -> ((Change model -> Eff (dom :: DOM, ref :: REF | eff) Unit) -> Jet model -> Jet (View (dom :: DOM, ref :: REF | eff))) -> model -> Eff (dom :: DOM, ref :: REF | eff) Unit
```

An example implementation of an application loop.

Renders a `View` to the DOM under the given `Node`. The `View` can depend
on the current value of the `model`, which can change over time by the
application of `Change`s in event handlers.


