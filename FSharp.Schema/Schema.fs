module FSharp.Schema.Schema

open Microsoft.FSharp.Reflection

type Type =
  | BuiltinType of BuiltinType
  | UserDefinedType of UserDefinedType
  | Unknown of System.Type

and BuiltinType =
  | Int
  | Float
  | Bool
  | String
  | Tuple of Type list // unit if list is empty
  | List of Type
  | Array of Type
  | Option of Type
  | Map of Type * Type

and UserDefinedType =
  | RecordType of RecordType
  | UnionType of UnionType

and RecordType =
  { RecordName: string
    Fields: NameWithType list }

and UnionType =
  { UnionName: string
    Cases: NameWithType list }

and NameWithType = { Name: string; Type: Type }

let private primitiveTypes =
  [ typeof<int>, Int
    typeof<float>, Float
    typeof<bool>, Bool
    typeof<string>, String
    typeof<unit>, Tuple [] ]
  |> readOnlyDict

let rec ofSystemType (t: System.Type) =
  let contains, found = primitiveTypes.TryGetValue t

  if contains then
    BuiltinType found
  elif t.IsArray && t.HasElementType then
    t.GetElementType()
    |> ofSystemType
    |> Array
    |> BuiltinType
  elif t.IsGenericType then
    let genericDef = t.GetGenericTypeDefinition()

    if genericDef = typeof<list<int>>.GetGenericTypeDefinition () then
      ofSystemType t.GenericTypeArguments.[0]
      |> List
      |> BuiltinType
    elif genericDef = typeof<option<int>>.GetGenericTypeDefinition () then
      ofSystemType t.GenericTypeArguments.[0]
      |> Option
      |> BuiltinType
    elif genericDef = typeof<Map<int, int>>.GetGenericTypeDefinition () then
      (ofSystemType t.GenericTypeArguments.[0], ofSystemType t.GenericTypeArguments.[1])
      |> Map
      |> BuiltinType

    else
      Unknown t
  elif FSharpType.IsRecord t then
    { RecordName = t.Name
      Fields =
        FSharpType.GetRecordFields t
        |> Array.map (fun prop ->
          { Name = prop.Name
            Type = ofSystemType prop.PropertyType })
        |> Array.toList }
    |> RecordType
    |> UserDefinedType
  elif FSharpType.IsUnion t then
    { UnionName = t.Name
      Cases =
        FSharpType.GetUnionCases t
        |> Array.map (fun case ->
          { Name = case.Name
            Type =
              case.GetFields()
              |> Array.map (fun prop -> ofSystemType prop.PropertyType)
              |> Array.toList
              |> Tuple
              |> BuiltinType })
        |> Array.toList }
    |> UnionType
    |> UserDefinedType
  else
    Unknown t

let rec toUserDefinedTypes t =
  let types =
    match t with
    | BuiltinType t ->
      match t with
      | Int
      | Float
      | Bool
      | String -> Seq.empty
      | Option t
      | List t
      | Array t -> toUserDefinedTypes t
      | Map (k, v) -> Seq.append (toUserDefinedTypes k) (toUserDefinedTypes v)
      | Tuple types -> types |> Seq.collect toUserDefinedTypes
    | UserDefinedType t ->
      match t with
      | RecordType t ->
        Seq.singleton (RecordType t)
        |> Seq.append (
          t.Fields
          |> Seq.collect (fun item -> toUserDefinedTypes item.Type)
        )
      | UnionType t ->
        Seq.singleton (UnionType t)
        |> Seq.append (
          t.Cases
          |> Seq.collect (fun item -> toUserDefinedTypes item.Type)
        )
    | Unknown _ -> Seq.empty

  types |> Seq.distinct
