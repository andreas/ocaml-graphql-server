module L = Language
module S = Schema

module StringMap = Map.Make(String)
type fragment_map = L.fragment StringMap.t

let rec collect_fields : fragment_map -> ('ctx, 'src) S.obj -> L.selection list -> L.field list = fun fragment_map obj fields -> 
  List.map (function
  | L.Field field ->
      [field]
  | FragmentSpread spread ->
      (try
        let fragment = StringMap.find spread.name fragment_map in
        if obj.name = fragment.type_condition then
          collect_fields fragment_map obj fragment.selection_set
        else
          []
      with Not_found -> [])
  | InlineFragment fragment ->
      match fragment.type_condition with
      | None ->
          collect_fields fragment_map obj fragment.selection_set
      | Some condition ->
          if condition = obj.name then
            collect_fields fragment_map obj fragment.selection_set
          else
            []
  ) fields |> List.concat

let alias_or_name : L.field -> string = fun field ->
  match field.alias with
  | Some alias -> alias
  | None       -> field.name

let field_from_object : ('ctx, 'src) S.obj -> string -> ('ctx, 'src) S.field option = fun obj field_name ->
  try
    let field = List.find (fun (S.Field field) -> field.name = field_name) obj.fields in 
    Some field
  with Not_found -> None

let rec present : type src. 'ctx -> src -> fragment_map -> L.field -> ('ctx, src) S.typ -> Yojson.Basic.json = fun ctx src fragment_map query_field typ -> match typ with
  | S.Scalar s   -> s.coerce src
  | S.List t     -> `List (List.map (fun x -> present ctx x fragment_map query_field t) src)
  | S.Nullable t -> (match src with None -> `Null | Some x -> present ctx x fragment_map query_field t)
  | S.Object o   ->
      let fields = collect_fields fragment_map o query_field.selection_set in
      `Assoc (resolve_fields ctx src fragment_map o fields) 
  | S.Enum e ->
      try
        let _, s = List.find (fun (v, s) -> src == v) e.values in
        `String s
      with Not_found -> `Null

and resolve_field : type src. 'ctx -> src -> fragment_map -> L.field -> ('ctx, src) S.field -> (string * Yojson.Basic.json) = fun ctx src fragment_map query_field (S.Field field) ->
  let name     = alias_or_name query_field in
  let resolved = field.resolve ctx src in
  let value    = present ctx resolved fragment_map query_field field.typ in
  (name, value)

and resolve_fields : type src. 'ctx -> src -> fragment_map -> ('ctx, src) S.obj -> L.field list -> (string * Yojson.Basic.json) list = fun ctx src fragment_map obj fields ->
  List.map (fun (query_field : L.field) ->
    match field_from_object obj query_field.name with
    | Some field -> resolve_field ctx src fragment_map query_field field
    | None       -> (alias_or_name query_field, `Null)
  ) fields

let execute_operation (schema : 'ctx S.schema) fragment_map (operation : L.operation) ctx =
  match operation.optype with
  | L.Query ->
      let query  = schema.query in
      let fields = collect_fields fragment_map query operation.selection_set in
      let props  = resolve_fields ctx () fragment_map query fields in
      (`Assoc props)
  | L.Mutation ->
      `String "Mutation is not implemented"
  | L.Subscription ->
      `String "Subscription is not implemented"

let collect_fragments doc =
  List.fold_left (fun memo -> function
    | L.Operation _ -> memo
    | L.Fragment f -> StringMap.add f.name f memo
  ) StringMap.empty doc

let select_operation doc =
  try
    let L.Operation op = List.find (function
      | L.Operation _ -> true
      | L.Fragment _ -> false
    ) doc in
    Some op
  with Not_found -> None

let execute schema doc ctx =
  let fragment_map = collect_fragments doc in
  let schema' = Introspection.add_schema_field schema in
  match select_operation doc with
  | None -> `String "No operation found"
  | Some op -> execute_operation schema' fragment_map op ctx
