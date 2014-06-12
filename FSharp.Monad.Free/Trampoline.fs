namespace FSharp.Monad

type Trampoline<'T> = Free<F0, 'T>

module Trampoline =

  let suspend (f: unit -> Trampoline<'T>) : Trampoline<_> = f |> F0.ofFunc |> Free.suspend

  let delay f : Trampoline<_> = suspend (fun () -> Free.done_ (f ()))
