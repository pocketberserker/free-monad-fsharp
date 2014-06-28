namespace FSharp.Monad

module Operational =

  let singleton<'F, 'T> (ta: 'F) =
    ta |> CoYoneda.lift<'F, 'T> |> Free.liftF CoYoneda.functor_<'F, 'T>

  let run (interpreter: NT<'S, 'M>) (m: Monad<'M>) (sa: Free<CoYoneda, 'T>) =
    let nt = { new NT<CoYoneda, 'M> with
      member this.Apply(cy) =
        let co = cy :?> CoYoneda<_, _>
        let f = co.Func<'T>()
        let k = interpreter.Apply(co.Value)
        Functor.map m f k
      }
    Free.foldMap nt CoYoneda.functor_<'S, 'T> m sa

  let runId interpreter sa = run interpreter Id.monad sa :?> Id<_, _> |> Id.run

  let rec interpret<'F, 'T, 'U, 'V> (returnI: 'T -> 'U)
    (bind: ('T -> 'U) -> 'V -> 'U) (eval: 'F -> 'V) (p: Free<CoYoneda, 'T>) =
    match p |> Free.resume<'T, 'T, CoYoneda, 'T> CoYoneda.functor_<'F, 'T> with
    | Choice1Of2 a ->
       let a = (a :?> CoYoneda<'F, _>)
       eval a.Value |> bind (interpret returnI bind eval << a.Func<'T>())
    | Choice2Of2 a -> returnI a
