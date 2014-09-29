type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
           
let min_diagonal = 0.0000001
         
exception OutOfBounds


let new_tree (r:region) : 'a quadtree = 
  let ((xll,yll),(xur,yur))= r in 
  let midx=(xll+.(xur-.xll)/.2.0) in
  let midy=(yll+.(yur-.yll)/.2.0) in
  let mid=(midx,midy) in
  Node (r , Leaf ((mid , (snd r)) , []) , 
    Leaf (((xll,midy) , (midx,yur)) , []) , 
    Leaf (((fst r) , mid) , []) , 
    Leaf (((midx,yll) , (xur,midy)) , [])) 

let measure_diagonal (r:region) : float =
    let ((xll,yll),(xur,yur))= r in
    ((xur-.xll)**2.0+.(yur-.yll)**2.0)**0.5

(*let next_quad (q: 'a quadtree) (c : coord) : 'a quadtree =
  match q with
  | Leaf (_,_) -> q
  | Node (((xl,yd),(xr,yu)),i,ii,iii,iv) ->
    let midx = xl+.(xl+.xr)/.2.0 in
    let midy = yd+.(yd+.yu)/.2.0 in
    let (x,y) = c in
    if (x < xl || x > xr || y < yd || y > yu) then raise OutOfBounds
    else if (x > midx && y > midy) then i
    else if (x < midx && y > midy) then ii
    else if (x < midx && y < midy) then iii
    else iv*)

let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  match q with 
  | Node (((xl,yd),(xr,yu)),i,ii,iii,iv) ->
    let midx = xl+.(xl+.xr)/.2.0 in
    let midy = yd+.(yd+.yu)/.2.0 in
    let (x,y) = c in
    if (x < xl || x > xr || y < yd || y > yu) then raise OutOfBounds
    else if (x > midx && y > midy) 
    then Node (((xl,yd),(xr,yu)),(insert i c s),ii,iii,iv)
    else if (x < midx && y > midy) 
    then Node (((xl,yd),(xr,yu)),i,(insert ii c s),iii,iv)
    else if (x < midx && y < midy) 
    then Node (((xl,yd),(xr,yu)),i,ii,(insert iii c s),iv)
    else Node (((xl,yd),(xr,yu)),i,ii,iii,(insert iv c s))
  | Leaf (r,[]) -> Leaf (r,[(c,s)])
  | Leaf (r,(hc,ha)::t) -> if (measure_diagonal r)< min_diagonal
                     then Leaf (r, (c, s)::(hc,ha)::t)
                     else insert (insert (new_tree (r)) (hc) (ha)) c s 
  

                    
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
      (a: 'a) (t: 'b quadtree): 'a =
      match t with
      | Leaf (r,[]) -> a
      | Leaf (r,(hc,ha)::t) -> fold_quad f ((f ha)::a) (Leaf (r,t))
      | Node (((xl,yd),(xr,yu)),i,ii,iii,iv) -> 
        Node (((xl,yd),(xr,yu)),fold_quad i,fold_quad ii,
          fold_quad iii,fold_quad iv)

     
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
= failwith "TODO"

