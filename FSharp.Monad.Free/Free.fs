﻿namespace FSharp.Monad

// port from https://github.com/xuwei-k/free-monad-java/

type Free<'F, 'A> =
  abstract member Bind: ('A -> Free<'F, 'B>) -> Free<'F, 'B>

and Done<'F, 'A> (a: 'A) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

and Gosub<'F, 'A, 'B> (a: Free<'F, 'A>, f: 'A -> Free<'F, 'B>) =
  member this.Value = a
  member this.Func = f
  interface Free<'F, 'B> with
    member this.Bind(g) = Gosub(a, fun a -> Gosub(f a, g) :> Free<'F, 'C>) :> Free<_, _>

type _1<'F, 'A> = interface end

type Suspend<'F, 'A> (a: _1<'F, Free<'F, 'A>>) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

type Functor<'F> =
  abstract member Map: ('A -> 'B) * _1<'F, 'A> -> _1<'F, 'B>

module Free =

  let done_ b = Done(b) :> Free<_, _>
  let suspend a = Suspend(a) :> Free<_, _>
  let gosub f a = Gosub(a, f) :> Free<_, _>

  let rec resume (f: Functor<'F>) (free: Free<'F, 'A>) =
    match free with
    | :? Done<'F, 'A> as d -> Choice2Of2 d.Value
    | :? Suspend<_, _> as s -> Choice1Of2 s.Value
    | _ ->
      let gosub1 = free :?> Gosub<'F, _, 'A>
      match gosub1.Value with
      | :? Done<'F, 'A> as d -> d.Value |> gosub1.Func |> resume f
      | :? Suspend<_, _> as d ->
        let g = fun (o: Free<_, _>) -> o.Bind(gosub1.Func)
        let value = f.Map(g, d.Value)
        Choice1Of2 value
      | _ ->
        let gosub2 = gosub1.Value :?> Gosub<'F, 'A, 'A>
        gosub2.Value.Bind(fun o -> (gosub2.Func o).Bind(gosub1.Func))
        |> resume f
        
  let liftF (g: Functor<'G>) (value: _1<'G, 'B>) = g.Map(done_, value) |> suspend

  let inline bind f (free: Free<_, _>) = free.Bind(f)
