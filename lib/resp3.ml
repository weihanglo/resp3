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


let take_till_string token =
  let open Angstrom in
  let token_len = String.length token in
  if token_len == 0 then take 0
  (* TODO: handle \r\n correctly *)
  else Unsafe.take_till (fun c -> c == token.[0]) Bigstringaf.substring

module P = struct
  open Angstrom

  let eol = "\r\n"

  let line = take_till_string eol <* take (String.length eol)

  let expr = fix (fun expr ->
    let blob = line
      >>= fun s -> take (int_of_string s)
      <* string eol
      >>| fun s -> Blob (Bytes.of_string s)
    in

    let str = line >>| fun s -> String s in

    let error = line >>| fun s -> Error s in

    let number = line >>| fun s -> Number (Int64.of_string s) in

    let null = string eol *> return Null in

    let double = line >>| fun s -> Double (Float.of_string s) in

    let boolean = line >>= (function
      | "t" -> return (Boolean true)
      | "f" -> return (Boolean false)
      | _ -> fail "failed to parse boolean"
      )
    in

    let blob_error = line 
      >>= fun s -> take (int_of_string s)
      <* string eol
      >>| fun s -> 
        let pos = String.index s ' ' in
        BlobError {
          code = String.sub s 0 pos;
          message = String.sub s (pos + 1) (String.length s - pos - 1); 
      }
    in

    let verbatim_string = line 
      >>= fun s -> take (int_of_string s)
      <* string eol
      >>| fun s ->
        VerbatimString {
          format = String.sub s 0 3;
          text = String.sub s 4 (String.length s - 4);
        }
    in

    let big_number = line >>| fun s -> BigNumber (Num.num_of_string s) in

    (* TODO: use `many` with a fixed length *)
    let array = line >>= fun _ -> many expr >>| fun l -> Array l in

    (* TODO: use `many` with a fixed length *)
    let list_to_map l =
        List.fold_right
          (fun (k, v) hash -> Hashtbl.add hash k v; hash) 
          l
          (Hashtbl.create ~random:true 4) (* TODO: create table with exact capacity *)
    in

    let map = line
      >>= fun _ -> many (both expr expr) 
      >>| fun l -> Map (list_to_map l)
    in

    (* TODO: Should use `many` with a fixed length *)
    let set = line >>= fun _ -> many expr >>| fun l ->
      Set (
        List.fold_right
          (fun x hash -> Hashtbl.add hash x (); hash) 
          l
          (Hashtbl.create ~random:true 4) (* TODO: create hash table with exact capacity *)
      )
    in

    let attribute = line
      >>= fun _ -> many (both expr expr) 
      >>| fun l -> Attribute (list_to_map l)
    in

    (* TODO: use `many` with a fixed length *)
    let push = line 
      >>= fun _ -> both blob (many expr)
      >>| fun (kind, messages) ->
        let kind = match kind with 
          | Blob bytes -> Bytes.to_string bytes 
          | _ -> failwith "invalid push kind"
        in
        Push { kind = kind; messages = messages; }
    in

    choice [
      char '$' *> blob;
      char '+' *> str;
      char '-' *> error;
      char ':' *> number;
      char '_' *> null;
      char ',' *> double;
      char '#' *> boolean;
      char '!' *> blob_error;
      char '=' *> verbatim_string;
      char '(' *> big_number;
      char '*' *> array;
      char '%' *> map;
      char '~' *> set;
      char '|' *> attribute;
      char '>' *> push;
    ]
  )
end

let parse s : value =
  let open Angstrom in
  match parse_string ~consume:All P.expr s with
    | Ok v -> v
    | Error msg -> failwith msg
