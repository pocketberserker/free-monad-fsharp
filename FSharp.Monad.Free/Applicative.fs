namespace FSharp.Monad

[<AbstractClass>]
type Applicative<'F> =
  abstract member Point : F0<'T> -> _1<'F, 'T>
  abstract member Ap: F0<_1<'F, F1<'T, 'U>>> * F0<_1<'F, 'T>> -> _1<'F, 'U>
  abstract member Map: ('T -> 'U) * _1<'F, 'T> -> _1<'F, 'U>
  default this.Map(F1 f, fa) =
    let f = F0.ofFunc <| fun () -> this.Point(F0.ofFunc <| fun () -> f)
    let fa = F0.ofFunc <| fun () -> fa
    this.Ap(f, fa)
  interface Apply<'F> with
    member this.Ap(f, fa) = this.Ap(f, fa)
    member this.Map(f, fa) = this.Map(f, fa)
