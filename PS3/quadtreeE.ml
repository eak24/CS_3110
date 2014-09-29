(*Constructs a quadtree datatype to store data with coordinates on a cartesion 
plane*)
type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let new_tree (r:region) : 'a quadtree = 
  let ((xl,yd),(xr,yu)) = r in
  if sqrt((xr-.xl)**2.0+.(yu-.yd)**2.0)/.2.0 < min_diagonal then Leaf (r,[]) 
else let midx = xl+.(xl+.xr)/.2.0 in
      let midy = yd+.(yd+.yu)/.2.0 in
      Node (r,Leaf (((midx,midy),(xr,yu)),[]),
        Leaf (((xl,midy),(midx,yu)),[]),
        Leaf (((xl,yd),(midx,midy)),[]),
        Leaf (((midx,yd),(xl,midy)),[]))
        
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree = 
  let in_region (qq: 'a quadtree) (cq : coord) : bool=
  let (((xl,yd),(xr,yu)),_) = qq in
  (x > xl || x < xr || y > yd || y < yu) in 
  match q with 
  | Leaf -> if (in_quad q c) then (c,s)::(snd q) else raise OutOfBounds
  | Node -> insert ()
  
let get_quad (qq: 'a quadtree) (cq : coord) : 'a quadtree =
  let (((xl,yd),(xr,yu)),i,ii,iii,iv) = qq
  let midx = xl+.(xl+.xr)/.2.0 in
  let midy = yd+.(yd+.yu)/.2.0 in
  let (x,y) = cq in
else if (x > midx && y > midy) then i
else if (x < midx && y > midy) then ii
else if (x < midx && y < midy) then iii
else if (x > midx && y < midy) then iv
else 0 in

let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

