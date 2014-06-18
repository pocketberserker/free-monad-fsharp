namespace FSharp.Monad

type Kleisli<'F, 'T, 'U>(run: 'T  -> _1<'F, 'U>) =
  member this.Run = run
  member this.Map(f, g: Functor<'F>) =
    Kleisli(run >> (Functor.map g f))
  member this.Ap(f: unit -> Kleisli<_, _, _>, g: Apply<'F>) =
    Kleisli(fun a -> Apply.ap g (fun () -> (f ()).Run a) (fun () -> run a))
  member this.Bind(f: _ -> Kleisli<_, 'T, _>, g: Bind<'F>) =
    Kleisli(fun a -> Bind.bind g (fun b -> f b :> _1<_, _>) (run a))
  member this.AndThen(k: Kleisli<_, _, _>, g: Bind<'F>) =
    Kleisli(fun a -> Bind.bind g k.Run (run a))
  member this.Compose(k: Kleisli<_, _, _>, g: Bind<_>) = k.AndThen(this, g)
  interface _1<'F, 'U>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Kleisli =

  let create run = Kleisli(run)

  let map g f (k: Kleisli<_, _, _>) = k.Map(f, g)

  let ap g f (k: Kleisli<_, _, _>) = k.Ap(f, g)

  let bind b f (k: Kleisli<_, _, _>) = k.Bind(f, b)

  let compose b f (k: Kleisli<_, _, _>) = k.Compose(f, b)

  let point (ap: Applicative<_>) b =
    fun _ -> b |> Applicative.point ap
    |> create

  let functor_ g = { new Functor<_> with
    member this.Map(f, fa) = (fa :?> Kleisli<_, _, _>).Map(f, g) :> _1<_, _> }
