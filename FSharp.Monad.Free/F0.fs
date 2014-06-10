namespace FSharp.Monad

type F0 = F0

type F0<'T> =
  inherit _1<F0, 'T>
  abstract member Apply: unit -> 'T

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F0 =

  let ofFunc (f: unit -> 'T) = { new F0<_> with member this.Apply() = f () }

  let toFunc (f: F0<'T>) = fun () -> f.Apply()

  let map (f: 'T -> 'U) (f0: F0<'T>) = { new F0<_> with member this.Apply() = f (f0.Apply()) }

  let functor_ = { new Functor<F0> with
    member this.Map(f, fa: _1<F0, _>) = (fa :?> F0<_>) |> map f :> _1<_, _> }

  let run (x: _1<F0, Free<F0, 'T>>) = (x :?> F0<Free<F0, _>>).Apply()
