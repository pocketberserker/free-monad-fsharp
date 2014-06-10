namespace FSharp.Monad

// http://eed3si9n.com/learning-scalaz/Free+Monad.html

type Z = Z

[<AbstractClass>]
type CharToy<'A> internal () =
  abstract member Fold: ('A -> char -> 'Z) * ('A -> 'Z) * 'Z -> 'Z
  abstract member Map: ('A -> 'B) -> CharToy<'B>
  interface _1<Z, 'A>

type CharOutput<'A> (a: char, next: 'A) =
  inherit CharToy<'A>()
  override this.Fold(output, _, _) = output next a
  override this.Map(f) = CharOutput(a, f next) :> CharToy<_>

type CharBell<'A> (next: 'A) =
  inherit CharToy<'A>()
  override this.Fold(_, bell, _) = bell next
  override this.Map(f) = CharBell(f next) :> CharToy<_>

type CharDone<'A> () =
  inherit CharToy<'A>()
  override this.Fold(_, _, done_) = done_
  override this.Map(f) = CharDone() :> CharToy<_>

module CharToy =

  let functor_ = { new Functor<Z> with
    member this.Map(f, fa: _1<Z, _>) = (fa :?> CharToy<_>).Map(f) :> _1<_, _> }

  let output a = CharOutput(a, ()) |> Free.liftF functor_
  let bell () = CharBell(()) |> Free.liftF functor_
  let done_ () = CharDone<unit>() |> Free.liftF functor_

  let pointed a = Free.done_ a

  let inline fold output bell done_ (toy: CharToy<_>) = toy.Fold(output, bell, done_)
  let inline map f (toy: CharToy<_>) = toy.Map(f)
