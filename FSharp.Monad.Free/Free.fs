namespace FSharp.Monad

// port from https://github.com/xuwei-k/free-monad-java/

type Free = Free

type Free<'F, 'A> =
  inherit _1<Free, 'A>
  abstract member Bind: ('A -> Free<'F, 'B>) -> Free<'F, 'B>

[<Sealed>]
type private Gosub<'F, 'A, 'B> (a: Free<'F, 'A>, f: 'A -> Free<'F, 'B>) =
  member this.Value = a
  member this.Func = f
  interface Free<'F, 'B> with
    member this.Bind(g) = Gosub(a, fun a -> Gosub(f a, g) :> Free<'F, 'C>) :> Free<_, _>

[<Sealed>]
type private Done<'F, 'A> (a: 'A) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

[<Sealed>]
type private Suspend<'F, 'A> (a: _1<'F, Free<'F, 'A>>) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Free =

  let done_ b = Done(b) :> Free<_, _>
  let suspend a = Suspend(a) :> Free<_, _>
  let gosub f a = Gosub(a, f) :> Free<_, _>

  let rec resume<'X1, 'X2, 'F, 'A> (f: Functor<'F>) (free: Free<'F, 'A>) =
    match free with
    | :? Done<'F, 'A> as d -> Choice2Of2 d.Value
    | :? Suspend<'F, 'A> as s -> Choice1Of2 s.Value
    | _ ->
      let gosub1 = free :?> Gosub<'F, 'X1, 'A>
      match gosub1.Value with
      | :? Done<'F, 'X1> as d -> d.Value |> gosub1.Func |> resume f
      | :? Suspend<'F, 'X1> as d ->
        let g = fun (o: Free<_, _>) -> o.Bind(gosub1.Func)
        let value = f.Map(g, d.Value)
        Choice1Of2 value
      | _ ->
        let gosub2 = gosub1.Value :?> Gosub<'F, 'X2, 'X1>
        gosub2.Value.Bind(fun o -> (gosub2.Func o).Bind(gosub1.Func))
        |> resume f
        
  let liftF (g: Functor<'G>) (value: _1<'G, 'B>) = g.Map(done_, value) |> suspend

  let inline bind f (free: Free<_, _>) = free.Bind(f)
  let (>>=) m f = bind f m

  // port from https://github.com/scalaz/scalaz
  let rec run<'X1, 'X2, 'F, 'T> functor_ cast_ (free: Free<'F, 'T>) =
    match resume<'X1, 'X2, 'F, 'T> functor_ free with
    | Choice1Of2 k -> run functor_ cast_ (cast_ k)
    | Choice2Of2 a -> a

  let castF0<'T> (x: _1<F0, Free<F0, 'T>>) = (x :?> F0<Free<F0, 'T>>).Apply ()

  let rec foldMap (nt: NT<_, _>) (f: Functor<_>) (m: Monad<_>) free =
    match resume f free with
    | Choice1Of2 l -> Bind.bind m (fun x -> foldMap nt f m x) (nt.Apply(l))
    | Choice2Of2 r -> Applicative.point m (fun () -> r)

  let monad (functor_: Functor<'F>) = { new Monad<Free>() with
    member this.Point(a) = a.Apply() |> done_ :> _1<Free, _>
    member this.Bind(f, fa) =
      (fa :?> Free<'F, _>) |> bind (fun a -> f.Apply(a) :?> Free<'F, _>) :> _1<Free, _> }

  let coyoneda<'F, 'T> = monad CoYoneda.functor_<'F, 'T>

type FreeBuilder () =
  member this.Return(x) = Free.done_ x
  member this.ReturnFrom(x) = x
  member this.Bind(x, f) = Free.bind f x

[<AutoOpen>]
module FreeDefaultOps =
  let free = FreeBuilder()
