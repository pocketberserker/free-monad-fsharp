namespace FSharp.Monad

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

  let reflexiveInjectInstance = { new Inject<_, _> with
    member this.Inj(fa) = fa
    member this.Proj(ga) = Some ga }

  let leftInjectInstance = { new Inject<_, Coproduct> with
    member this.Inj(fa) = Coproduct.left fa :> _1<_, _>
    member this.Proj(ga) =
      match ga :?> Coproduct<_, _, _> with
      | Coproduct (Choice1Of2 l) -> Some l
      | Coproduct (Choice2Of2 _) -> None }

  let rightInjectInstance (i: Inject<_, _>) = { new Inject<_, Coproduct> with
    member this.Inj(fa) = i.Inj(fa) |> Coproduct.right :> _1<_, _>
    member this.Proj(ga) =
      match ga :?> Coproduct<_, _, _> with
      | Coproduct (Choice1Of2 _) -> None
      | Coproduct (Choice2Of2 r) -> i.Proj(r) }
