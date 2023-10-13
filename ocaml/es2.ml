(* Exercise 2: Temperature Conversion System.
Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales
to read about them).

Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note that the scale must be specified.
Hint. Define a proper datatype for the temperature. *)

type scale = Celsius | Fahrenheit | Kelvin | Rankine | Delisle | Newton | Reaumur | Romer
type temperature = {scale: scale; value: float}

let scale_to_str = function
  Celsius -> "Celsius"
  | Fahrenheit -> "Fahrenheit"
  | Kelvin -> "Kelvin"
  | Rankine -> "Rankine"
  | Delisle -> "Delisle"
  | Newton -> "Newton"
  | Reaumur -> "Reaumur"
  | Romer -> "Romer"

let scale_to_symbol = function
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
  | h :: t -> Printf.printf "Scale: %s - value: %.2f °%s\n" (scale_to_str h.scale) h.value (scale_to_symbol h.scale); print_list_of_scales t

let celsius_to_fahrenheit x = (x *. (9. /. 5.)) +. 32.
let celsius_to_kelvin x = x +. 273.15
let celsius_to_rankine x = (x +. 273.15) *. (9. /. 5.)
let celsius_to_delisle x = (100. -. x) *. (3. /. 2.)
let celsius_to_newton x = x *. (33. /. 100.)
let celsius_to_reamur x = x *. (4. /. 5.)
let celsius_to_romer x = (x *. (21. /. 40.) +. 7.5)

let fahrenheit_to_celsius x = (x -. 32.) *. 5. /. 9.
let kelvin_to_celsius x = x -. 273.15
let rankine_to_celsius x = (x -. 491.67) *. (5. /. 9.)
let delisle_to_celsius x = (100. -. x) *. (2. /. 3.)
let newton_to_celsius x = x *. (100. /. 33.)
let reaumur_to_celsius x = x *. (5. /. 4.)
let romer_to_celsius x = (x -. 7.5) *. (40. /. 21.)

let get_conversion_table x = (* assume that x is in Celsius*)
  [
    {scale=Celsius; value=x};
    {scale=Fahrenheit; value=(celsius_to_fahrenheit x)};
    {scale=Kelvin; value=(celsius_to_kelvin x)};
    {scale=Rankine; value=(celsius_to_rankine x)};
    {scale=Delisle; value=(celsius_to_delisle x)};
    {scale=Newton; value=(celsius_to_newton x)};
    {scale=Reaumur; value=(celsius_to_reamur x)};
    {scale=Romer; value=(celsius_to_romer x)};
  ]

let x = 15.
let conversion_table = get_conversion_table x
let _ =
  Printf.printf "Conversion table for %.2f °C:\n\n" x;
  print_list_of_scales conversion_table