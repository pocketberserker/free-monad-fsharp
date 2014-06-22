namespace FSharp.Monad

type Id = Id

type Id<'T> = {
  Value : 'T
}
  with
    interface _1<Id, 'T>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Id =

  let map f i = { Value = f i.Value }

  let bind (f: _ -> Id<_>) i = f i.Value

  let monad = { new Monad<Id>() with
    member this.Map(f, fa) = (fa :?> Id<_>) |> map f :> _1<Id, _>
    member this.Point(a) = { Value = a.Apply() } :> _1<Id, _>
    member this.Bind(f, fa) = (fa :?> Id<_>) |> bind (fun a -> f.Apply(a) :?> Id<_>) :> _1<Id, _> }

  let run i = i.Value
