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

(* Extract a Slice From a List 
   Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements). *)

let slice lst x y =
  let rec aux _x _y = function
    | [] -> []
    | h :: t ->
      if _x > 0 then aux (_x - 1) (_y - 1) t
      else
        if _y > -1 then h :: aux (_x - 1) (_y - 1) t
        else []
  in aux x y lst

(* Rotate a List N Places to the Left 
   Rotate a list N places to the left. *)

let rotate lst n =
  let rec aux acc i = function
    | []     -> []
    | h :: t ->
      if i > 1 then aux (h :: acc) (i - 1) t
      else t @ List.rev (h :: acc)
  in aux [] n lst

(* Extract a Given Number of Randomly Selected Elements From a List 
   The selected items shall be returned in a list. We use the Random module but and initialise it with Random.init 0 at the start of the function for reproducibility and validate the solution. To make the function truly random, however, one should remove the call to Random.init 0 *)

let rand_select lst n =
  let random_n_of_list l = List.length l |> Random.int
  in let rec remove_elem_from_list x = function
    | []     -> []
    | h :: t ->
      if h = x then t
      else h :: remove_elem_from_list x t
  in let random_elem_of_list l =
    let i = random_n_of_list l in
      let picked = List.nth l i in 
        (picked, remove_elem_from_list picked l)
  in let rec aux counter = function
    | [] -> []
    | l0 ->
      let elem, l1 = random_elem_of_list l0 in
        if counter > 0 then elem :: aux (counter - 1) l1
        else []
  in aux n lst