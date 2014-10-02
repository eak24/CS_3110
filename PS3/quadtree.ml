type coord = float * float
type region = coord * coord

(*A quadtree is a sparse 2D representation of data. 
*Each leaf contains a region and points within that region associated with some 
*type a'. Each node points to four trees that have a region of one quarter of 
*the parent node as divided along the centerlines of either side. 
*A node represents each of its children's regions as a tuple in the order of 
*the cartesian quadrant numbering, i.e. quadrant i is where x>=0 and y>=0, and 
ii is to the left of i, iii below ii and iv to the right of iii*)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
           
let min_diagonal = 0.0000001
         
exception OutOfBounds
exception RegionTooSmall

(*Requires a region and returns the length of the diagonal of the region as a 
float.*)
let measure_diagonal (r:region) : float =
    let ((xll,yll),(xur,yur))= r in
    ((xur-.xll)**2.0+.(yur-.yll)**2.0)**0.5

(*Requires a region and returns a quadtree composed of an empty leaf that 
*contains that region. If the region is too small (the diagonal<.0000001), 
*then raises exception "RegionTooSmall"*)
let new_tree (r:region) : 'a quadtree = 
  if (measure_diagonal r)<min_diagonal then raise RegionTooSmall 
  else Leaf (r,[])

(*Requires a quadtree (q) of the same type as something (s) to be inserted, s 
and the coordinate at which to insert s. If the coordinate is on the dividing 
lines between two leave's regions, the coordinate will go in the leaf with
the region of the lowest numbered quadrant as numbered starting with the lowest
in the upper right corner and increasing as one moves counter-clockwise.*)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let (x,y) = c in
  match q with 
  | Node (((xl,yd),(xr,yu)),i,ii,iii,iv) ->
    let midx = xl+.(xl+.xr)/.2.0 in
    let midy = yd+.(yd+.yu)/.2.0 in
    if (x < xl || x > xr || y < yd || y > yu) then raise OutOfBounds
    else if (x >= midx && y >= midy) 
    then Node (((xl,yd),(xr,yu)),(insert i c s),ii,iii,iv)
    else if (x < midx && y >= midy) 
    then Node (((xl,yd),(xr,yu)),i,(insert ii c s),iii,iv)
    else if (x <= midx && y < midy) 
    then Node (((xl,yd),(xr,yu)),i,ii,(insert iii c s),iv)
    else Node (((xl,yd),(xr,yu)),i,ii,iii,(insert iv c s))
  | Leaf (((xl,yd),(xr,yu)),[]) -> 
  if (x < xl || x > xr || y < yd || y > yu) then raise OutOfBounds
else Leaf (((xl,yd),(xr,yu)),[(c,s)])
  | Leaf (((xl,yd),(xr,yu)),(hc,ha)::t) -> 
  if (x < xl || x > xr || y < yd || y > yu) then raise OutOfBounds 
else if (measure_diagonal ((xl,yd),(xr,yu)))< min_diagonal 
then Leaf (((xl,yd),(xr,yu)), (c, s)::(hc,ha)::t)
else insert (insert (new_tree ((xl,yd),(xr,yu))) (hc) (ha)) c s                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

(*Requires a valid quadtree and a function that can accept each element of the
*tree. Returns the quadtree that has beed folded using the passed in function 
*over every element within the tree. Applies f in no specific order.*)                    
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
      (a: 'a) (t: 'b quadtree): 'a =
      match t with
      | Leaf (r,[]) -> a
      | Leaf (r,(hc,ha)::t) -> fold_quad f (f a (hc,ha)) (Leaf (r,t))
      | Node (r,i,ii,iii,iv) -> 
        fold_quad f (fold_quad f (fold_quad f (fold_quad f (fold_quad f a iv) iii) ii) i) 
        (Leaf (r,[]))


(*Folds f over elements in a given region r in a given tree, t, by applying it to them in no specific 
*order. Takes in accumulator a.*)     
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
= let ((xl,yd),(xr,yu))= r in
let fold_if_in_region (acc: 'a) ((c: coord), (el: 'b)) : 'a =
let (x,y) = c in 
if (x < xl || x > xr || y < yd || y > yu) then acc else (f acc (c,el)) in
fold_quad fold_if_in_region a t 

