(* Exercise 3: Matrix Calculi.
   Write the matrix datatype with the following operations:

    A function zeroes to construct a matrix of size n×m filled with zeros.
    A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
    A function init to construct a square matrix of a given size n filled with the first n×n integers.
    A function transpose that transposes a generic matrix independently of its size and content.
    The basics operators + and * that adds and multiplies two matrices non necessarily squared. *)

module Matrix = struct
	type matrix = int list list

	let zeroes n m =
		List.init n (fun _ -> List.init m (fun _ -> 0))

	let identity n =
		List.init n (fun x -> List.init n (fun y -> if x = y then 1 else 0))

	let init n =
		List.init n (fun x -> List.init n (fun y -> (x + 1) * (y + 1)))

	let rec transpose = function
		| []            -> []
		| [] :: t       -> transpose t
		| (x :: y) :: t ->
			(x :: List.map List.hd t) :: transpose (y :: List.map List.tl t) (* ??? *)

	let ( + ) m1 m2 =
		List.map2 (fun r1 r2 -> List.map2 (fun x y -> x + y) r1 r2) m1 m2

	let ( * ) m1 m2 = () (* TODO *)
end