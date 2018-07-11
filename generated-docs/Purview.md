## Module Purview

#### `View`

``` purescript
newtype View
```

The (abstract) type of views.

You can create (incremental) functions returning values of this type by
using the `text` and `element` functions.

`View`s can be initially rendered to the DOM using the `render` function.

##### Instances
``` purescript
Patch View ViewChanges
```

#### `ViewChanges`

``` purescript
newtype ViewChanges
```

The (abstract) type of view updates.

`View`s can be applied to the DOM using the `applyPatch` function.

##### Instances
``` purescript
Semigroup ViewChanges
Monoid ViewChanges
Patch View ViewChanges
```

#### `text`

``` purescript
text :: Jet (Atomic String) -> Jet View
```

Create a text node wrapped in a `<span>` element.

#### `textWith`

``` purescript
textWith :: String -> Jet (Atomic String) -> Jet View
```

Create a text node wrapped in an element with the specified name.

#### `element`

``` purescript
element :: String -> Jet (IMap String (Atomic String)) -> Jet (IMap String (Atomic EventListener)) -> Jet (IArray View) -> Jet View
```

Create an element with the given name, attributes, event listeners and
children.

#### `element_`

``` purescript
element_ :: String -> Jet (IArray View) -> Jet View
```

Create an element with no attributes or event handlers.

#### `unsafeEventListener`

``` purescript
unsafeEventListener :: (Event -> Effect Unit) -> EventListener
```

Create an `EventListener` unsafely.

This operation is unsafe in the sense that applying it multiple times
to the same argument will result in event listeners which are not
substitutable in all contexts.

However, for the purposes of the current API, it's very useful to be
able to create `EventListener`s unsafely in this way, and then store
the results in a `View`.

#### `render`

``` purescript
render :: Node -> View -> Effect Unit
```

Render a `View` to the DOM, under the given `Node`, and connect any
event listeners.

Once the initial `View` is rendered, the DOM can be updated using the
`applyPatch` function.

#### `Component`

``` purescript
type Component model = Jet (Atomic (Change model -> Effect Unit)) -> Jet model -> Jet View
```

An example component type, used by the `run` function.

A component takes a changing update function, and a changing `model`
and returns a changing `View`. The update function receives a `Change` to
the model and applies it.

#### `run`

``` purescript
run :: forall model change. Patch model change => Element -> Component model -> model -> Effect Unit
```

An example implementation of an application loop.

Renders a `View` to the DOM under the given `Node`. The `View` can depend
on the current value of the `model`, which can change over time by the
application of `Change`s in event handlers.


