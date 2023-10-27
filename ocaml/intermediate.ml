(* https://ocaml.org/exercises?difficulty_level=intermediate *)

(* Flatten a List
   Flatten a nested list structure. *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | One(v) -> v :: acc
    | Many l -> List.fold_left aux acc l
  in List.rev (aux [] lst)

(* Eliminate Duplicates 
   Eliminate consecutive duplicates of list elements. *)

let rec compress = function
  | [] -> []
  | [x] -> [x]
  | a :: (b :: _ as t) ->
    if a = b then compress t
    else a :: compress t