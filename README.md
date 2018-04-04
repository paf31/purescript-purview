# Purview

A proof-of-concept UI library based on the incremental lambda calculus.

- [Module Documentation](generated-docs/Firkin.md)
- [Counter Example](test/Main.purs)

## Motivation

As I've claimed before, [you might not need the virtual DOM](http://blog.functorial.com/posts/2018-03-12-You-Might-Not-Need-The-Virtual-DOM.html). In [purescript-sdom](https://github.com/paf31/purescript-sdom/), I tried to remove the need for the virtual DOM by using types to force parts of the DOM structure to be static, and then exploiting that in order to remove the need for a diff algorithm.

Here, I take a different approach, where I restrict the rendering function `model -> view` to the class of _incrementalizable functions_. That is, the rendering function must be computable in the regular sense, but it must also tell us how the view _changes_ in response to changes in the model. In this way, we can avoid a diff algorithm, and use the incremental lambda calculus (provided by [purescript-incremental](https://github.com/paf31/purescript-incremental/)) in order to propagate model changes to the DOM.

The approach here is to provide an unopinionated toolkit which can be used to define a variety of APIs. An example API is provided for reference. See the [module documentation](generated-docs/Purview.md) for an overview of the API.
