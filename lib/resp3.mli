type value =
  | Blob of bytes
  | String of string
  | Error of string
  | Number of int64
  | Null
  | Double of float
  | Boolean of bool
  | BlobError of { code : string; message : string; }
  | VerbatimString of { format : string; text: string; }
  | BigNumber of Num.num
  | Array of value list
  | Map of (value, value) Hashtbl.t
  | Set of (value, unit) Hashtbl.t
  | Attribute of (value, value) Hashtbl.t
  | Push of { kind : string; messages : value list }
(* TODO: handle stream aggregated data types *)

val parse : string -> value

val to_string : value -> string
