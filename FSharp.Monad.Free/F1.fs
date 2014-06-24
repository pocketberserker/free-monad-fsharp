namespace FSharp.Monad

type F1 = F1

type F1<'T, 'U> = {
  Apply : 'T -> 'U
}
  with
    interface _1<F1, 'U>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F1 =

  let ofFunc f = { Apply = f }

  let toFunc f = f.Apply

[<AutoOpen>]
module F1DefaultOps =

  let (|F1|) f = F1.ofFunc f
  let (|StdF1|) f = F1.toFunc f
