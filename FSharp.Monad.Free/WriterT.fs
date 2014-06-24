namespace FSharp.Monad

type WriterT = WriterT

type WriterT<'F, 'W, 'A> = {
  Run: _1<'F, T2<'W, 'A>>
}
  with
    interface _1<WriterT, 'A>

[<AutoOpen>]
module WriterTDefaultOps =

  let (|WriterT|) w = w.Run

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WriterT =

  let run w = w.Run

  let map f func w =
    { Run = Functor.map f (fun t -> { _1 = t._1; _2 = func t._2 }) w.Run }

  let bind b s f w =
    { Run = Bind.bind b (fun wa ->
      Functor.map b (fun wb ->
        { _1 = Semigroup.append s wa._1 wb._1; _2 = wb._2 }) (f wa._2 |> run)) w.Run }

  let written f (WriterT run) = Functor.map f T2.fst run

  let value f (WriterT run) = Functor.map f T2.snd run

  let mapValue f func (WriterT run) = { Run = Functor.map f func run }

  let mapWritten f func w = mapValue f (fun wa -> { _1 = func wa._1; _2 = wa._2 }) w
  
  let add f s w1 w = mapWritten f (fun w2 -> Semigroup.append s w2 w1) w
