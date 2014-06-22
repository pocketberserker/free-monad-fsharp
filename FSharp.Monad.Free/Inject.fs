namespace FSharp.Monad

[<AbstractClass>]
type Inject<'F, 'G> =
  abstract member Inj: _1<'F, 'A> -> _1<'G, 'A>
  abstract member Proj: _1<'G, 'A> -> _1<'F, 'A> option

module Inject =

  let inject (i: Inject<_, _>) ga = i.Inj(ga) |> Free.suspend

  let match_<'X1, 'X2, 'G, 'F, 'T> (i: Inject<'G, 'F>) (f: Functor<'F>) (fa: Free<'F, 'T>) =
    fa
    |> Free.resume<'X1, 'X2, 'F, 'T> f
    |> function
      | Choice1Of2 l -> i.Proj(l)
      | Choice2Of2 _ -> None
