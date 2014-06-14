module Program

open FSharp.Monad

open System 

// http://fssnip.net/6i

type IO<'T> = private | Action of (unit -> 'T)

[<AutoOpen>]
module MonadIO =
  let private raw  (Action f) = f
  let ret x = Action (fun () -> x)
  let run io = raw io ()
  let private eff g io = raw io () |> g
  let bind rest io = Action (fun () -> io |> eff rest |> run)
  let (|Action|) io = run io

[<AutoOpen>]
module PreludeIO =
  let putChar (c:char) = Action (fun () -> stdout.Write(c); c)
  let getChar     = Action (fun () -> stdin.Read() |> char)

// https://gist.github.com/xuwei-k/8646473

type CharIO<'A> =
  | GetCh
  | PutCh of char
  | LiftIO of IO<'A>

let getCh = Operational.singleton<CharIO<char>, char> GetCh
 
let putCh c = Operational.singleton<CharIO<char>, char> (PutCh c)
 
let liftIO<'A> (a: IO<'A>) = Operational.singleton<CharIO<'A>, 'A> (LiftIO a)
 
let advent = function
  | GetCh -> getChar
  | PutCh c -> putChar c
  | LiftIO f -> f
 
let runCharIO a = Operational.interpret ret bind advent a 

let getAndPut = Free.bind putCh getCh

let run io = runCharIO io |> run |> ignore

[<EntryPoint>]
let main argv = 
  run getAndPut
  0
