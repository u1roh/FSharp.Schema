module FSharp.Schema.TypeScript

let rec private toTypeName t =
  match t with
  | Schema.BuiltinType t ->
    match t with
    | Schema.Int
    | Schema.Float -> "number"
    | Schema.Bool -> "boolean"
    | Schema.String -> "string"
    | Schema.Tuple types ->
      match types with
      | [] -> "void"
      | [t] -> toTypeName t
      | _ ->
        types
        |> Seq.map toTypeName
        |> String.concat ", "
        |> sprintf "[%s]"
    | Schema.List t
    | Schema.Array t -> toTypeName t |> sprintf "%s[]"
    | Schema.Option t -> toTypeName t |> sprintf "%s | undefined"
    | Schema.Map (k, v) -> sprintf "{ [key: %s]: %s }" (toTypeName k) (toTypeName v)
  | Schema.UserDefinedType t ->
    match t with
    | Schema.RecordType t -> t.RecordName
    | Schema.UnionType t -> t.UnionName
  | Schema.Unknown _ -> "Unknown"

let genType t =
  match t with
  | Schema.RecordType t ->
    t.Fields
    |> Seq.map (fun item ->
      toTypeName item.Type
      |> sprintf "    %s: %s;\n" item.Name)
    |> String.concat ""
    |> sprintf "export type %s = {\n%s}" t.RecordName
  | Schema.UnionType t ->
    t.Cases
    |> Seq.map (fun item ->
      match item.Type with
      | Schema.BuiltinType (Schema.Tuple []) ->
        sprintf "    | \"%s\"" item.Name
      | t ->
        toTypeName t
        |> sprintf "    | { %s: %s }" item.Name)
    |> String.concat "\n"
    |> sprintf "export type %s =\n%s" t.UnionName
