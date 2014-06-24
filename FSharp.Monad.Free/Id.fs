namespace FSharp.Monad

type Id = Id

type Id<'T, 'U> = {
  Value : 'T
}
  with
    interface _1<Id, 'U>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Id =

  let map f i = { Value = f i.Value }

  let bind (f: _ -> Id<_, _>) i = f i.Value

  let monad = { new Monad<Id>() with
    member this.Map(f, fa) = (fa :?> Id<_, _>) |> map f :> _1<Id, _>
    member this.Point(a) = { Value = a.Apply() } :> _1<Id, _>
    member this.Bind(f, fa) =
      fa :?> Id<_, _> |> bind (fun a -> f.Apply(a) :?> Id<_, _>) :> _1<Id, _> }

  let run i = i.Value
