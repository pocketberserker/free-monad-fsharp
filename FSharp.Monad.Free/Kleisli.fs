namespace FSharp.Monad

type Kleisli<'F, 'T, 'U>(run: 'T  -> _1<'F, 'U>) =
  member this.Map<'V>(f, g: Functor<'F>) =
    Kleisli(run >> (fun a -> g.Map(f, a)))
