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

(* Decode a Run-Length Encoded List
   Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)
  
type rl_code =
  | One of string
  | Many of int * string

let rec decode lst =
  let rec extract code = match code with
    | One(v) -> [v]
    | Many(i, v) ->
      if i > 1 then v :: extract (Many(i-1, v))
      else [v]
  in match lst with
    | [] -> []
    | h :: t -> extract h :: decode t

(* Replicate the Elements of a List a Given Number of Times 
   Replicate the elements of a list a given number of times. *)

let replicate lst n =
  let rec aux i = function
    | [] -> []
    | (h :: t) as l ->
      if i > 0 then h :: aux (i - 1) l
      else aux n t
  in aux n lst

(* Drop Every N'th Element From a List 
   Drop every N'th element from a list. *)

let drop lst n =
  let rec aux i = function
    | [] -> []
    | h :: t -> 
      if i > 1 then h :: aux (i - 1) t
      else aux n t
  in aux n lst