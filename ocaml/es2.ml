(* Exercise 2: Temperature Conversion System.
Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales
to read about them).

Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note that the scale must be specified.
Hint. Define a proper datatype for the temperature. *)

type scale = Celsius | Fahrenheit | Kelvin | Rankine | Delisle | Newton | Reaumur | Romer
type temperature = {scale: scale; value: float}

let fahrenheit_of_celsius x = (x *. (9. /. 5.)) +. 32.
let kelvin_of_celsius x = x +. 273.15
let rankine_of_celsius x = (x +. 273.15) *. (9. /. 5.)
let delisle_of_celsius x = (100. -. x) *. (3. /. 2.)
let newton_of_celsius x = x *. (33. /. 100.)
let reamur_of_celsius x = x *. (4. /. 5.)
let romer_of_celsius x = (x *. (21. /. 40.) +. 7.5)

let celsius_of_fahrenheit x = (x -. 32.) *. 5. /. 9.
let celsius_of_kelvin x = x -. 273.15
let celsius_of_rankine x = (x -. 491.67) *. (5. /. 9.)
let celsius_of_delisle x = (100. -. x) *. (2. /. 3.)
let celsius_of_newton x = x *. (100. /. 33.)
let celsius_of_reaumur x = x *. (5. /. 4.)
let celsius_of_romer x = (x -. 7.5) *. (40. /. 21.)

let celsius_of_scale scale value = match scale with
  Celsius -> value
  | Fahrenheit -> celsius_of_fahrenheit value
  | Kelvin -> celsius_of_kelvin value
  | Rankine -> celsius_of_rankine value
  | Delisle -> celsius_of_delisle value
  | Newton -> celsius_of_newton value
  | Reaumur -> celsius_of_reaumur value
  | Romer -> celsius_of_romer value

let scale_of_string = function
  "Celsius" -> Celsius
  | "Fahrenheit" -> Fahrenheit
  | "Kelvin" -> Kelvin
  | "Rankine" -> Rankine
  | "Delisle" -> Delisle
  | "Newton" -> Newton
  | "Reaumur" -> Reaumur
  | "Romer" -> Romer
  | x -> Printf.printf "Invalid scale: %s. Defaulting to Celsius\n" x; Celsius

let string_of_scale = function
  Celsius -> "Celsius"
  | Fahrenheit -> "Fahrenheit"
  | Kelvin -> "Kelvin"
  | Rankine -> "Rankine"
  | Delisle -> "Delisle"
  | Newton -> "Newton"
  | Reaumur -> "Reaumur"
  | Romer -> "Romer"

let symbol_of_scale = function
  Celsius -> "C"
  | Fahrenheit -> "F"
  | Kelvin -> "K"
  | Rankine -> "R"
  | Delisle -> "De"
  | Newton -> "N"
  | Reaumur -> "Re"
  | Romer -> "Ro"

let rec print_list_of_scales = function
  [] -> ()
  | h :: t -> Printf.printf "Scale: %s - value: %.2f °%s\n%!" (string_of_scale h.scale) h.value (symbol_of_scale h.scale); print_list_of_scales t

let get_conversion_table x = (* assume that x is in Celsius*)
  [
    {scale=Celsius; value=x};
    {scale=Fahrenheit; value=(fahrenheit_of_celsius x)};
    {scale=Kelvin; value=(kelvin_of_celsius x)};
    {scale=Rankine; value=(rankine_of_celsius x)};
    {scale=Delisle; value=(delisle_of_celsius x)};
    {scale=Newton; value=(newton_of_celsius x)};
    {scale=Reaumur; value=(reamur_of_celsius x)};
    {scale=Romer; value=(romer_of_celsius x)};
  ]

let x = 15.
let conversion_table = get_conversion_table x
let _ =
  Printf.printf "Conversion table for %.2f °C:\n%!" x;
  print_list_of_scales conversion_table;
  Printf.printf "\n----- TEMPERATURE CONVERSION SHELL -----\n\n";
  Printf.printf "Enter the scale: %!"
let input_scale = scale_of_string (input_line stdin)

let _ = Printf.printf "Enter the temperature: %!"
let input_temp = float_of_string (input_line stdin)

let conversion_table = get_conversion_table (celsius_of_scale input_scale input_temp)
let _ =
  Printf.printf "\nConversion table for %.2f °%s:\n" input_temp (symbol_of_scale input_scale);
  print_list_of_scales conversion_table;