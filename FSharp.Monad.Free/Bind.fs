namespace FSharp.Monad

type Bind<'F> =
  inherit Apply<'F>
  abstract member Bind: F1<'A, _1<'F, 'B>> * _1<'F, 'A> -> _1<'F, 'B>
