namespace FSharp.Monad

type Apply<'F> =
  inherit Functor<'F>
  abstract member Ap :F0<_1<'F, F1<'A, 'B>>> * F0<_1<'F, 'A>> -> _1<'F, 'B>
