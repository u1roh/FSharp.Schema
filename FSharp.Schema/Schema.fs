module FSharp.Schema.Schema

open Microsoft.FSharp.Reflection

type CustomTypeType =
  | Record
  | Union

type Type =
  | BuiltinType of BuiltinType
  // | Int
  // | Float
  // | Bool
  // | String
  // | Tuple of Type list // unit if list is empty
  // | List of Type
  // | Array of Type
  // | Option of Type
  // | Map of Type * Type
  | Custom of CustomType
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


and TypedItem = { Name: string; Type: Type }

and CustomType =
  { SystemType: System.Type
    TypeType: CustomTypeType
    Name: string
    Items: TypedItem list }

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
    { SystemType = t
      TypeType = Record
      Name = t.Name
      Items =
        FSharpType.GetRecordFields t
        |> Array.map (fun prop ->
          { Name = prop.Name
            Type = ofSystemType prop.PropertyType })
        |> Array.toList }
    |> Custom
  elif FSharpType.IsUnion t then
    { SystemType = t
      TypeType = Union
      Name = t.Name
      Items =
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
    |> Custom
  else
    Unknown t

let rec toCustomTypes t =
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
      | Array t -> toCustomTypes t
      | Map (k, v) -> Seq.append (toCustomTypes k) (toCustomTypes v)
      | Tuple types -> types |> Seq.collect toCustomTypes
    | Custom t ->
      Seq.singleton t
      |> Seq.append (
        t.Items
        |> Seq.collect (fun item -> toCustomTypes item.Type)
      )
    | Unknown _ -> Seq.empty

  types |> Seq.distinct
