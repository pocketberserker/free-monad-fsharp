namespace FSharp.Monad

type Functor<'F> =
  abstract member Map: ('A -> 'B) * _1<'F, 'A> -> _1<'F, 'B>

module Functor =

  let map (fa: Functor<_>) f a = fa.Map(f, a)
