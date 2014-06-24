module Example

open FSharp.Monad

// https://gist.github.com/runarorama/a8fab38e473fafa0921d
// https://gist.github.com/xuwei-k/fd34a5889f40f6b937bf

type Interact = Interact

type Interact<'T> =
  | Ask of string
  | Tell of string
  with
    interface _1<Interact, 'T>

type Tester<'A> = WriterT<Map<string, string> -> 'A, string list, 'A>

let tester (F1 f) = { WriterT.Run = f :> _1<F1, _> }

let console = { new NT<Interact, Id> with
  member x.Apply(i) =
    match i :?> Interact<_> with
    | Ask prompt ->
      printfn "%s" prompt
      { Value = System.Console.ReadLine() } :> _1<Id, _>
    | Tell msg ->
      { Value = printfn "%s" msg } :> _1<Id, _>
}

let testConsole = { new NT<Interact, WriterT> with
  member x.Apply(i) =
    match i :?> Interact<_> with
    | Ask prompt ->
      tester (fun m -> { _1 = []; _2 = Map.find prompt m })
      :> _1<_, _>
    | Tell msg ->
      tester (fun _ -> { _1 = [ msg ]; _2 = failwith "unit" })
      :> _1<_, _>
}

type UserId = string
type Password = string
type Permission = string
type Auth = Auth
type App<'A> = Coproduct<Auth, Interact, 'A>

type User = {
  Id : string
}

type Auth<'A> =
  | Login of UserId * Password
  | HasPermission of User * Permission
  with
    interface _1<Auth, 'A>

[<Literal>]
let KnowSecret = "KnowSecret"

let lift (i: Inject<_, _>) f = Free.liftF Free.coyoneda (i.Inj f)

module Interacts =
  let tell i msg = Tell msg |> lift i
  let ask i prompt = Ask prompt |> lift i
 
module Auths =
  let login i (id: UserId) (pwd: Password) = Login(id, pwd) |> lift i
  let hasPermission i (u: User) (p: Permission) = HasPermission(u, p) |> lift i

let prg (i: Inject<Interact, _>) (ia: Inject<Auth, _>) =
  free {
    let! uid = Interacts.ask i "What's your user ID?"
    let! pwd = Interacts.ask i "Password, please."
    let! u = Auths.login ia uid pwd
    let! b =
      u
      |> Option.map (fun x -> Auths.hasPermission ia x KnowSecret)
      |> function
        | Some f -> f
        | None -> Applicative.point Free.coyoneda (fun () -> false) :?> Free<_, _>
    let! _ = if b then Interacts.tell i "UUDDLRLRBA" else Interacts.tell i "Go away!"
    return ()
  }

let consolePrg (i: Inject<Interact, _>) =
  free {
    let! first = Interacts.ask i "first name?"
    let! last = Interacts.ask i "last name?"
    let! _ = Interacts.tell i "Hello, %s , %s!"
    return ()
  }

let or_ (fg: NT<'F, 'G>) (hg: NT<'H, 'G>) = { new NT<Coproduct, 'G> with
  member x.Apply(c: _1<Coproduct,'T>) =
    c :?> Coproduct<_, _, _>
    |> Coproduct.fold (fun l -> fg.Apply(l)) (fun r -> hg.Apply(r)) }

let testAuth = { new NT<Auth, Id> with
  member x.Apply(a) =
    match a :?> Auth<_> with
    | Login(uid, pwd) as l ->
      let value =
        if uid = "john.snow" && pwd = "Ghost" then Some({ User.Id = "john.snow" })
        else None
      { Id.Value = value } :> _1<Id, _>
    | HasPermission(u, _) ->
      { Id.Value = u.Id = "john.snow" } :> _1<Id, _>
}
