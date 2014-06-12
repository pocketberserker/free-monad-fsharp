namespace FSharp.Monad

type F0 = F0

[<Sealed>]
type F0<'T>(f: unit -> 'T) =
  member this.Value = f
  interface _1<F0, 'T>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F0 =

  let ofFunc (f: unit -> 'T) = F0(f)

  let toFunc (f: F0<'T>) = f.Value

  let map (f: 'T -> 'U) (f0: F0<'T>) = F0(fun () -> f (f0.Value ()))

  let functor_ = { new Functor<F0> with
    member this.Map(f, fa: _1<F0, _>) = (fa :?> F0<_>) |> map f :> _1<_, _> }

  let run (x: _1<F0, Free<F0, 'T>>) = (x :?> F0<Free<F0, _>>).Value ()
