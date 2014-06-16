namespace FSharp.Monad

type Applicative<'F> =
  inherit Apply<'F>
  abstract member Point : F0<'T> -> _1<'F, 'T>
