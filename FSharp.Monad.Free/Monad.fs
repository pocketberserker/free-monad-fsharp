namespace FSharp.Monad

[<AbstractClass>]
type Monad<'F>() =
  abstract member Point : F0<'T> -> _1<'F, 'T>
  abstract member Bind: F1<'A, _1<'F, 'B>> * _1<'F, 'A> -> _1<'F, 'B>
  abstract member Ap :F0<_1<'F, F1<'A, 'B>>> * F0<_1<'F, 'A>> -> _1<'F, 'B>
  default this.Ap(f, fa) =
    let ff = F1.ofFunc <| fun (StdF1 x) -> this.Map(x, fa.Apply())
    let fa = f.Apply()
    this.Bind(ff, fa)
  abstract member Map: ('T -> 'U) * _1<'F, 'T> -> _1<'F, 'U>
  default this.Map(F1 f, fa) =
    let f = F1.ofFunc <| fun a -> this.Point(F0.ofFunc <| fun () -> f.Apply(a))
    this.Bind(f, fa)
  interface Apply<'F> with
    member this.Map(f, fa) = this.Map(f, fa)
    member this.Ap(f, fa) = this.Ap(f, fa)
  interface Applicative<'F> with
    member this.Point(f) = this.Point(f)
  interface Bind<'F> with
    member this.Bind(f, fa) = this.Bind(f, fa)
