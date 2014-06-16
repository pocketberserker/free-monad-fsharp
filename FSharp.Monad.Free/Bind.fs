namespace FSharp.Monad

[<AbstractClass>]
type Bind<'F> =
  abstract member Bind: F1<'A, _1<'F, 'B>> * _1<'F, 'A> -> _1<'F, 'B>
  abstract member Ap: F0<_1<'F, F1<'A, 'B>>> * F0<_1<'F, 'A>> -> _1<'F, 'B>
  abstract member Map: ('T -> 'U) * _1<'F, 'T> -> _1<'F, 'U>
  default this.Ap(f, fa) =
    let ff = F1.ofFunc <| fun (StdF1 x) -> this.Map(x, fa.Apply())
    let fa = f.Apply()
    this.Bind(ff, fa)
  interface Apply<'F> with
    member this.Ap(f, fa) = this.Ap(f, fa)
    member this.Map(f, fa) = this.Map(f, fa)