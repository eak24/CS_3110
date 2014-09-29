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

let rec insert (q: 'a quadtree) (c : coord) (s:'a) : int =
  match q with 
  | Node (r, a1, a2, a3, a4)-> 
  | Leaf (lr, ll) -> if (measure_diagonal r)< min_diagonal
                     then Leaf (lr, (c, s)::ll)
                     else insert (new_tree lr) c s
  in
  let ((xll,yll),(xur,yur))= r in
  let midx=(xll+.(xur-.xll)/.2.0) in
  let midy=(yll+.(yur-.yll)/.2.0) in
  let mid=(midx,midy) in
  let (xc, yc)=c in 
  if false(*xc<xll|xc>xur|yc<yll|yc>yur*) then raise OutOfBounds else
  Node (r , Leaf ((mid , (snd r)) , []) , 
    Leaf (((xll,midy) , (midx,yur)) , []) , 
    Leaf (((fst r) , mid) , []) , 
    Leaf (((midx,yll) , (xur,midy)) , [])) 

                    
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
      (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
     
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

