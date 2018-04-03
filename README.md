# `purescript-firkin`

A proof-of-concept UI library based on the incremental lambda calculus.

## Motivation

As I've claimed before, [you might not need the virtual DOM](http://blog.functorial.com/posts/2018-03-12-You-Might-Not-Need-The-Virtual-DOM.html). In [purescript-sdom](https://github.com/paf31/purescript-sdom/), I tried to remove the need for the virtual DOM by using types to force parts of the DOM structure to be static, and then exploiting that in order to remove the need for a diff algorithm.

Here, I take a different approach, where I restrict the rendering function `model -> view` to the class of _incrementalizable functions_. That is, the rendering function must be computable in the regular sense, but it must also tell us how the view _changes_ in response to changes in the model. In this way, we can avoid a diff algorithm, and use the incremental lambda calculus (provided by [purescript-incremental](https://github.com/paf31/purescript-incremental/)) in order to propagate model changes to the DOM.

The approach here is to provide an unopinionated toolkit which can be used to define a variety of APIs. An example API is provided for reference.

## Hello, World!

Here is the "Hello, World" of UI libraries - a counter which can be incremented by clicking a button:

```purescript
type Model = Atomic Int

view :: (Change Model -> Eff _ Unit) -> Jet Model -> Jet (View _)
view change counter =
    element "button" (constant (wrap mempty))
      -- ^ a <button> with no attributes
      (IMap.singleton "click" (mapAtomic onClick counter))
      -- ^ onclick handler
      (IArray.singleton (text (mapAtomic (("Current value = " <> _) <<< show) counter)))
  where
    onClick current = eventListener \_ ->
      change (replace (current + 1))
```
