namespace FSharp.Monad

type CoYoneda = CoYoneda

type internal CoYonedaF<'T> = interface end

type internal CoYonedaF<'T, 'U> =
  inherit _1<CoYoneda, 'U>
  inherit CoYonedaF<'U>
  abstract member Apply: 'T -> 'U

[<Sealed>]
type CoYoneda<'F, 'A> internal (fi: 'F, k: CoYonedaF<'A>) =
  member this.Func<'T>() = fun x -> (k :?> CoYonedaF<'T, 'A>).Apply(x)
  member this.Value = fi
  interface _1<CoYoneda, 'A>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CoYoneda =
  
  let apply<'F, 'T, 'U> (fa: 'F) (k: 'T -> 'U) =
    CoYoneda(fa, { new CoYonedaF<'T, 'U> with
      member this.Apply(x: 'T) = k x })
  
  let map<'F, 'T, 'U, 'V> (f: 'U -> 'V) (coyoneda: CoYoneda<'F, 'U>) =
    apply<'F, 'T, 'V> coyoneda.Value (coyoneda.Func<'T>() >> f)

  let functor_<'F, 'T> = { new Functor<CoYoneda> with
    member x.Map<'A, 'B>(f: 'A -> 'B, ya: _1<CoYoneda,'A>) =
      ya :?> CoYoneda<'F, 'A> |> map<'F, 'T, 'A, 'B> f :> _1<CoYoneda, 'B> }

  let run<'F, 'T, 'A> (coyoneda: CoYoneda<_1<CoYoneda, 'T>, 'A>) =
    functor_<'F, 'T>.Map(coyoneda.Func<'T>(), coyoneda.Value)

  let lift<'F, 'T> (fa: 'F) = apply<'F, 'T, 'T> fa id
