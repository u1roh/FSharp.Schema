module FSharp.Schema.Rust

let rec private toRustTypeName t =
  match t with
  | Schema.BuiltinType t ->
    match t with
    | Schema.Int -> "i32"
    | Schema.Float -> "f64"
    | Schema.Bool -> "bool"
    | Schema.String -> "String"
    | Schema.Tuple types ->
      types
      |> Seq.map toRustTypeName
      |> String.concat ", "
      |> sprintf "(%s)"
    | Schema.List t
    | Schema.Array t -> toRustTypeName t |> sprintf "Vec<%s>"
    | Schema.Option t -> toRustTypeName t |> sprintf "Option<%s>"
    | Schema.Map (k, v) -> sprintf "std::collections::HashMap<%s, %s>" (toRustTypeName k) (toRustTypeName v)
  | Schema.UserDefinedType t ->
    match t with
    | Schema.RecordType t -> t.RecordName
    | Schema.UnionType t -> t.UnionName
  | Schema.Unknown _ -> "Unknown"

let rec private isEq t =
  match t with
  | Schema.BuiltinType t ->
    match t with
    | Schema.Float -> false
    | Schema.Int
    | Schema.Bool
    | Schema.String -> true
    | Schema.Tuple types -> types |> Seq.forall isEq
    | Schema.List t
    | Schema.Array t
    | Schema.Option t -> isEq t
    | Schema.Map (k, v) -> false
  | Schema.UserDefinedType t ->
    match t with
    | Schema.RecordType t -> t.Fields |> Seq.forall (fun x -> isEq x.Type)
    | Schema.UnionType t -> t.Cases |> Seq.forall (fun x -> isEq x.Type)
  | Schema.Unknown _ -> false

let rec private isCopy t =
  match t with
  | Schema.BuiltinType t ->
    match t with
    | Schema.Float
    | Schema.Int
    | Schema.Bool -> true
    | Schema.String -> false
    | Schema.Tuple types -> types |> Seq.forall isCopy
    | Schema.List t
    | Schema.Array t -> false
    | Schema.Option t -> isCopy t
    | Schema.Map (k, v) -> false
  | Schema.UserDefinedType t ->
    match t with
    | Schema.RecordType t -> t.Fields |> Seq.forall (fun x -> isCopy x.Type)
    | Schema.UnionType t -> t.Cases |> Seq.forall (fun x -> isCopy x.Type)
  | Schema.Unknown _ -> false

let toRustType t =
  "#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]\n"
  + (if isEq (Schema.UserDefinedType t) then
       "#[derive(Eq, Hash, PartialOrd, Ord)]\n"
     else
       "")
  + (if isCopy (Schema.UserDefinedType t) then
       "#[derive(Copy)]\n"
     else
       "")
  + match t with
    | Schema.RecordType t ->
      t.Fields
      |> Seq.map (fun item ->
        toRustTypeName item.Type
        |> sprintf "    pub %s: %s,\n" item.Name)
      |> String.concat ""
      |> sprintf "pub struct %s {\n%s}" t.RecordName
    | Schema.UnionType t ->
      t.Cases
      |> Seq.map (fun item ->
        match item.Type with
        | Schema.BuiltinType (Schema.Tuple []) -> ""
        | t -> toRustTypeName t
        |> sprintf "    %s%s,\n" item.Name)
      |> String.concat ""
      |> sprintf "pub enum %s {\n%s}" t.UnionName
