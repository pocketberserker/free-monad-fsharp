namespace FSharp.Monad

type T2<'T, 'U> = {
  _1 : 'T
  _2 : 'U
}

module T2 =

  let fst t = t._1
  let snd t = t._2
