namespace FSharp.Monad

module Operational =

  let singleton<'F, 'T> (ta: 'F) =
    ta |> CoYoneda.lift<'F, 'T> |> Free.liftF CoYoneda.functor_<'F, 'T>

  let rec interpret<'F, 'T, 'U, 'V> (returnI: 'T -> 'U)
    (bind: ('T -> 'U) -> 'V -> 'U) (eval: 'F -> 'V) (p: Free<CoYoneda, 'T>) =
    match p |> Free.resume<'T, 'T, CoYoneda, 'T> CoYoneda.functor_<'F, 'T> with
    | Choice1Of2 a ->
       let a = (a :?> CoYoneda<'F, _>)
       eval a.Value |> bind (interpret returnI bind eval << a.Func<'T>())
    | Choice2Of2 a -> returnI a
