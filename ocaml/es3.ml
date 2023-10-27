(* Exercise 3: Matrix Calculi.
Write the matrix datatype with the following operations:

A function zeroes to construct a matrix of size n×m filled with zeros.
A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
A function init to construct a square matrix of a given size n filled with the first n×n integers.
A function transpose that transposes a generic matrix independently of its size and content.
The basics operators + and * that adds and multiplies two matrices non necessarily squared. *)

module Matrix = struct
  let zeroes n m = Array.make_matrix n m
  let identity n =
    Array.init n (
      fun i -> Array.init n (
        fun j -> if i = j then 1 else 0
      )
    )
end