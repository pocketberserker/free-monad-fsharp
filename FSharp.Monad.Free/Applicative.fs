namespace FSharp.Monad

type Applicative<'F> =
  inherit Apply<'F>
  abstract member Point : F0<'T> -> _1<'F, 'T>

module Applicative =

  let point (ap: #Applicative<_>) (F0 f) = ap.Point(f)
