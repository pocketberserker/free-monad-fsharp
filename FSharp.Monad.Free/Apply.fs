namespace FSharp.Monad

type Apply<'F> =
  inherit Functor<'F>
  abstract member Ap :F0<_1<'F, F1<'A, 'B>>> * F0<_1<'F, 'A>> -> _1<'F, 'B>

module Apply =

  let ap (fa: Apply<_>) (F0 f) (F0 g) = fa.Ap(f, g)
