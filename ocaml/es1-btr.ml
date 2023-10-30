(* Exercise 1: A few of Chemistry.
Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

Write a function that returns the highest atomic number in alkaline_earth_metals.
Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).
Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then

Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) sorted in ascending order on the element names. *)

type element = {name: string; value: int}

let string_of_element elem =
  Printf.sprintf "{ name: %s; value: %d }" elem.name elem.value

let print_element elem =
  Printf.printf "%s\n" (string_of_element elem)

let print_element_list lst =
  List.iter print_element lst

let alkaline_earth_metals = [
	{name = "barium"; value = 56};
	{name = "beryllium"; value = 4};
	{name = "magnesium"; value = 12};
	{name = "radium"; value = 88};
	{name = "strontium"; value = 38};
	{name = "calcium"; value = 20};
]
let noble_gases = [
	{name = "helium"; value = 2};
	{name = "neon"; value = 10};
	{name = "argon"; value = 18};
	{name = "krypton"; value = 36};
	{name = "xenon"; value = 54};
	{name = "radon"; value = 86}
]

let sort_element_list lst field order =
  let gt, eq, lt =
      if order = "asc" then 1, 0, -1
      else
        if order = "desc" then -1, 0, 1
        else invalid_arg "order must be one of: 'asc', 'desc'"
    in let get_compare_func = match field with
      | "name"  -> (
          fun x y ->
            if x.name > y.name then gt
            else
              if x.name = y.name then eq
              else lt
        )
      | "value" -> (
          fun x y ->
            if x.value > y.value then gt
            else
              if x.value = y.value then eq
              else lt
        )
      | _ -> invalid_arg "field must be one of: 'name', 'value'"
      in List.sort get_compare_func lst;;

let highest_atomic_number_of_element_list lst =
  sort_element_list lst "value" "desc" |> List.hd;;

let merge_element_lists lst1 lst2 =
  sort_element_list (lst1 @ lst2) "name" "asc";;

(* Main *)

print_endline "Alkaline earth metals:";
print_element_list alkaline_earth_metals;
print_endline "";

print_endline "Alkaline earth metals, sorted in ascending order by value:";
sort_element_list alkaline_earth_metals "value" "asc" |> print_element_list;
print_endline "";

print_endline "Alkaline earth metal with the highest atomic number:";
highest_atomic_number_of_element_list alkaline_earth_metals |> string_of_element |> Printf.printf "%s\n";
print_endline "";

print_endline "Alkaline earth metals and noble gases, sorted in ascending order by name:";
merge_element_lists alkaline_earth_metals noble_gases |> print_element_list;
print_endline "";