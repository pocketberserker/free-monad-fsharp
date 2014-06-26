namespace FSharp.Monad

type Coproduct = Coproduct

type Coproduct<'F, 'G, 'T> = Coproduct of Choice<_1<'F, 'T>, _1<'G, 'T>>
  with
    interface _1<Coproduct, 'T>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Coproduct =

  let map f g func = function
    | Coproduct (Choice1Of2 l) -> Functor.map f func l |> Choice1Of2 |> Coproduct
    | Coproduct (Choice2Of2 r) -> Functor.map g func r |> Choice2Of2 |> Coproduct

  let functor_ f g = { new Functor<Coproduct> with
    member x.Map(func, fa) =
      fa :?> Coproduct<_, _, _>
      |> map f g func
      :> _1<Coproduct, _> }

  let left x = Coproduct (Choice1Of2 x)

  let right x = Coproduct (Choice2Of2 x)

  let fold f g = function
    | Coproduct (Choice1Of2 l) -> f l
    | Coproduct (Choice2Of2 r) -> g r
