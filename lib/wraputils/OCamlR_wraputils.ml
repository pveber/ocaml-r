let ( |?> ) o f = match o with
  | Some x -> Some (f x)
  | None -> None

external ident : 'a -> 'a = "%identity"
