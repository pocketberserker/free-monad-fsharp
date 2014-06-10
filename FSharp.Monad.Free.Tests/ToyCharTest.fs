namespace FSharp.Monad.Free.Tests

open NUnit.Framework
open FsUnit
open FSharp.Monad

module Program =
  let rec showProgram (program: Free<Z, 'R>) =
    match program |> Free.resume CharToy.functor_ with
    | Choice1Of2 r ->
      r :?> CharToy<Free<Z, 'R>>
      |> CharToy.fold (fun next a -> "output " + string a + "\n" + showProgram(next)) (fun next -> "bell \n" + showProgram(next)) "done\n"
    | Choice2Of2 r -> "return " + (r.ToString()) + "\n"
  
[<TestFixture>]
module ToyCharTest =

  [<Test>]
  let ``example`` () =
    CharToy.output 'A'
    |> Free.bind (fun () -> CharToy.bell () |> Free.bind (fun () -> CharToy.done_ ()))
    |> Program.showProgram |> should equal "output A\nbell \ndone\n"
