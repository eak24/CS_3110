open Parser
open Quadtree

exception NoCityInList
val load_city_data : string -> string quadtree

val city_search : string quadtree -> region -> string list

