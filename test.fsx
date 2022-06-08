#r "nuget: FSharp.Json"
#r "FSharp.Schema/bin/Debug/net6.0/FSharp.Schema.dll"

open FSharp.Json
open FSharp.Schema

type Buzz = { Foo: int; Bar: int }

type Piyo =
  | A
  | B of string
  | C of prime: int * pi: float
  | D of Buzz

type Hoge = { Name: string; Piyo: Piyo }

[ { Name = "aaa"; Piyo = A }
  { Name = "bbb"; Piyo = B "piyopiyo" }
  { Name = "ccc"; Piyo = C(57, 3.14) }
  { Name = "ddd"
    Piyo = D { Foo = 123; Bar = 456 } } ]
|> Json.serializeEx { JsonConfig.Default with unformatted = true }
|> printfn "%s"

Schema.ofSystemType typeof<Hoge>
|> Schema.toCustomTypes
|> Seq.map Schema.Rust.toRustType
|> Seq.iter (printfn "%s\n")
