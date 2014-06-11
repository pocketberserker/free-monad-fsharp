namespace FSharp.Monad.Free.Tests

open NUnit.Framework
open FsUnit
open FSharp.Monad

[<TestFixture>]
module TrampolineTest =

  let rec fib n =
    if n < 2M then Free.done_ n
    else
      free {
        let! x = Trampoline.suspend (fun () -> fib (n - 1M))
        let! y = Trampoline.suspend (fun () -> fib (n - 2M))
        return x + y
      }

  [<Test>]
  let ``fib test`` () =
    fib 6M
    |> Free.run<decimal, decimal, F0, decimal> F0.functor_ F0.run
    |> should equal 8M

  //[<Test>]
  //let ``more fib test`` () =
  //  fib 35M
  //  |> Free.run<decimal, decimal, F0, decimal> F0.functor_ F0.run
  //  |> should equal 9227465M

  let rec factorial n =
    if n < 2M then Free.done_ 1M
    else
      free {
        let! x = Trampoline.suspend (fun () -> factorial (n - 1M))
        return n * x
      }

  [<Test>]
  let ``factorial test`` () =
    factorial 5M
    |> Free.run<decimal, decimal, F0, decimal> F0.functor_ F0.run
    |> should equal 120M
