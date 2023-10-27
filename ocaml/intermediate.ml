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

(* Pack Consecutive Duplicates 
   Pack consecutive duplicates of list elements into sublists. *)

let pack lst =
  let rec aux acc curr = function
    | [] -> [] (* only reachable when input list is empty *)
    | [x] -> (x :: curr) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux acc (a :: curr) t
      else aux ((a :: curr) :: acc) [] t
  in List.rev (aux [] [] lst)