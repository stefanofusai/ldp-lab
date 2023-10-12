type earth_metal = {name: string; value: int}

let alkaline_earth_metals = [
	{name = "beryllium"; value = 4};
	{name = "magnesium"; value = 12};
	{name = "calcium"; value = 20};
	{name = "strontium"; value = 38};
	{name = "barium"; value = 56};
	{name = "radium"; value = 88};
]

let rec get_highest_atomic_number = function
	[] -> 0
	| h :: t -> max (match h with {name; value} -> value) (get_highest_atomic_number t)

let highest_atomic_number = get_highest_atomic_number alkaline_earth_metals;;
Printf.printf "The highest atomic number is: %d\n" highest_atomic_number;;