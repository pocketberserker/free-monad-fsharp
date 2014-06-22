namespace FSharp.Monad

type Semigroup<'F> =
  abstract member Append: 'F * 'F -> 'F

module Semigroup =

  let append (s: #Semigroup<_>) a b = s.Append(a, b)
