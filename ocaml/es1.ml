(* Exercise 1: A few of Chemistry.
Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

Write a function that returns the highest atomic number in alkaline_earth_metals.
Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).
Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then

Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) sorted in ascending order on the element names. *)

type element = {name: string; value: int}

let alkaline_earth_metals = [
	{name = "barium"; value = 56};
	{name = "beryllium"; value = 4};
	{name = "magnesium"; value = 12};
	{name = "radium"; value = 88};
	{name = "strontium"; value = 38};
	{name = "calcium"; value = 20};
]

let rec print_lst_of_elements = function
	[] -> ()
	| h :: t  -> Printf.printf "%s - %d\n" h.name h.value; print_lst_of_elements t

let _ = print_endline "Alkaline earth metals:";
	print_lst_of_elements alkaline_earth_metals;
	print_newline ()

let rec get_highest_atomic_number = function
	[] -> 0
	| h :: t -> max (match h with {name; value} -> value) (get_highest_atomic_number t)

let highest_atomic_number = get_highest_atomic_number alkaline_earth_metals;;
Printf.printf "The highest atomic number is: %d\n\n" highest_atomic_number;;

let compare_metals_by_value m1 m2 =
	if m1.value < m2.value then -1 else 
		if m1.value = m2.value then 0 else 1

let alkaline_earth_metals_sorted = List.sort compare_metals_by_value alkaline_earth_metals

let _ = print_endline "Alkaline earth metals, sorted by atomic number (asc):";
	print_lst_of_elements alkaline_earth_metals_sorted;
	print_newline ()

let noble_gases = [
	{name = "helium"; value = 2};
	{name = "neon"; value = 10};
	{name = "argon"; value = 18};
	{name = "krypton"; value = 36};
	{name = "xenon"; value = 54};
	{name = "radon"; value = 86}
]

let _ = print_endline "Noble gases:";
	print_lst_of_elements noble_gases;
	print_newline ()

let compare_metals_noble_gases m1 m2 =
	if m1.name < m2.name then -1 else
		if m1.name = m2.name then 0 else 1

let aem_ng_sorted = List.sort compare_metals_noble_gases (alkaline_earth_metals @ noble_gases)

let _ = print_endline "Alkaline earth metals @ noble gases, sorted by name (asc):";
	print_lst_of_elements aem_ng_sorted