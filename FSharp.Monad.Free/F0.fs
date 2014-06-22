namespace FSharp.Monad

type F0 = F0

type F0<'T> = {
  Apply : unit -> 'T
}
  with
    interface _1<F0, 'T>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F0 =

  let ofFunc f = { Apply = f }

  let toFunc f = f.Apply

  let map f f0 = { Apply = fun () -> f (f0.Apply ()) }

  let functor_ = { new Functor<F0> with
    member this.Map(f, fa: _1<F0, _>) = (fa :?> F0<_>) |> map f :> _1<_, _> }

[<AutoOpen>]
module F0DefaultOps =

  let (|F0|) f = F0.ofFunc f
