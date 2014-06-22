namespace FSharp.Monad

/// Natural Transformation
type NT<'F, 'G> =
  abstract member Apply: _1<'F, 'T> -> _1<'G, 'T>

module NT =

  let id<'F> () = { new NT<'F, 'F> with
    member this.Apply(fa) = fa }