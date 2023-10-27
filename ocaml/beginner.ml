(* https://ocaml.org/exercises?difficulty_level=beginner *)

(* Tail of a List
   Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

(* Last Two Elements of a List
   Find the last but one (last and penultimate) elements of a list.*)
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t;;

(* N'th Element of a List
   Find the N'th element of a list.
   Remark: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds. *)
let rec nth_element_of_list lst n = match lst with
  | [] -> None
  | h :: t -> if n == 0 then Some h else nth_element_of_list t (n - 1)

(* Length of a List
   Find the number of elements of a list.
   OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t

(* Reverse a List
   Reverse a list.
   OCaml standard library has List.rev but we ask that you reimplement it. *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst

(* Palindrome
   Find out whether a list is a palindrome.
   Hint: A palindrome is its own reverse. *)
let is_palindrome lst =
  lst = rev lst

(* Run-Length Encoding
   If you need so, refresh your memory about [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding). *)
let encode lst =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else aux 0 ((count + 1, a) :: acc) t in
  List.rev (aux 0 [] lst)

(* Modified Run-Length Encoding
   Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
   Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let rec aux count acc = function
    | [] -> []
    | [x] ->
      let elem =
        if count = 0 then One(x)
        else Many(count + 1, x)
      in elem :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else
        let elem =
          if count = 0 then One(a)
          else Many(count + 1, a)
        in aux 0 (elem :: acc) t
      in List.rev (aux 0 [] lst)

(* Duplicate the Elements of a List
   Duplicate the elements of a list. *)
let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
in List.rev (aux [] lst)

(* Better implementation *)
let rec duplicate_btr = function
  | [] -> []
  | h :: t -> h :: h :: duplicate_btr t

(* Split a List Into Two Parts; The Length of the First Part Is Given
   Split a list into two parts; the length of the first part is given.
   If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
let split lst i =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if i = 0 then (List.rev acc, l)
        else aux (i - 1) (h :: acc) t
  in aux i [] lst

(* Remove the K'th Element From a List
   Remove the K'th element from a list.
   The first element of the list is numbered 0, the second 1,...*)
let rec remove_at i = function
  | [] -> []
  | h :: t -> if i = 0 then t else h :: remove_at (i - 1) t

(* Insert an Element at a Given Position Into a List
   Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.) *)
let rec insert_at x i = function
  | [] -> []
  | h :: t ->
    if i - 1 >= List.length t then t @ [x]
    else
      if i = 0 then x :: h :: t
      else h :: insert_at x (i - 1) t

let rec insert_at_btr x i = function
  | [] -> [x]
  | h :: t ->
    if i = 0 then x :: h :: t
    else h :: insert_at x (i - 1) t

(* Create a List Containing All Integers Within a Given Range
   If first argument is greater than second, produce a list in decreasing order. *)
let range x y =
  let rec aux n i =
    if i > 0 then n :: aux (n + 1) (i - 1)
    else [n]
  in if x <= y then aux x (y - x) else List.rev @@ aux y (x - y)

(* Lotto: Draw N Different Random Numbers From the Set 1..M
   Draw N different random numbers from the set 1..M.
   The selected numbers shall be returned in a list. *)
let rec lotto_select n b =
  if n > 0 then Random.int b + 1 :: lotto_select (n - 1) b
  else []

(* Generate a Random Permutation of the Elements of a List
  Generate a random permutation of the elements of a list. *)
(* TODO *)

(* Determine Whether Two Positive Integer Numbers Are Coprime
  Determine whether two positive integer numbers are coprime.
  Two numbers are coprime if their greatest common divisor equals 1. *)
let coprime x y = 
  let rec gcd x y =
    if x = 0 then y
    else
      if y = 0 then x
      else gcd y (x mod y)
  in (gcd x y) = 1

(* A List of Prime Numbers
   Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. *)
let rec all_primes x y =
  let is_prime x =
    if x < 2 then false
    else if x = 2 then true
    else
      let rec aux n =
        if n * n > x then true
        else x mod n <> 0 && aux (n + 1)
      in aux 2
  in
  if x = y then []
    else
      if is_prime x then x :: all_primes (x + 1) y
      else all_primes (x + 1) y

(* Count the Leaves of a Binary Tree
   A leaf is a node with no successors. Write a function count_leaves to count them. *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec count_leaves = function
  | Empty -> 0
  | Node(_, Empty, Empty) -> 1
  | Node(_, l, r) -> count_leaves l + count_leaves r

(* Collect the Leaves of a Binary Tree in a List
   A leaf is a node with no successors. Write a function leaves to collect them in a list. *)
let collect_leaves tree =
  let rec aux t acc = match t with
    | Empty -> acc
    | Node(v, Empty, Empty)-> v :: acc
    | Node(_, l, r) -> aux l (aux r acc)
  in aux tree []

(* Collect the Internal Nodes of a Binary Tree in a List
   An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list. *)
let internals tree =
  let rec aux t acc = match t with
    | Empty -> acc
    | Node(v, Empty, Empty) -> acc
    | Node(v, l, r) -> aux l (v :: aux r acc)
  in aux tree []

(* Collect the Nodes at a Given Level in a List 
   A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list. *)
let at_level tree lvl =
  let rec aux t acc lvl path = match t with
    | Empty -> acc
    | Node(v, l, r) ->
      if path = lvl - 1 then v :: acc
      else aux l (aux r acc lvl (path + 1)) lvl (path + 1)
  in aux tree [] lvl 0

(* Count the Nodes of a Multiway Tree *)
type 'a multiway_tree =
  | Empty
  | Node of 'a * 'a multiway_tree list

let count_nodes tree =
  let rec count_tree = function
    | Empty -> 0
    | Node(_, l) -> 1 + count_list_of_nodes l
  and count_list_of_nodes = function
    | [] -> 0
    | h :: t -> count_tree h + count_list_of_nodes t
  in count_tree tree