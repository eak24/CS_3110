open Parser
open Quadtree

exception NoCityInList

(*Requires a string in a CSV format as specified in the PS3 PDF, with latitude 
*and longitude coordinates as found on earth and returns
*a quadtree with all the cities mapped to the specified coordinate. If the 
*string is empty or formatted incorrectly, raise NoCityInList. No guarantees
*for the size of the region of each city.*)
let load_city_data (s:string) : string quadtree = 
	match (parse s) with
	| [] -> raise NoCityInList
	| (lat1,long1,e1)::clst ->  
	let rec list_to_quadtree (lst: (float*float*string) list) (acc: string quadtree) 
	: 'a quadtree =
	match lst with
	| [] -> acc
	| (lat,long,el)::t -> list_to_quadtree lst (insert acc (lat,long) el) in
	let acc = insert (new_tree (((-.180.0),(-.90.0)),
		((180.0),(90.0)))) (lat1,long1) e1 in 
	list_to_quadtree clst acc

(*Return all of the cities within a given region, specified by latitude and 
*longitude.*)
let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun a (c,el) -> el::a) [] q r 
