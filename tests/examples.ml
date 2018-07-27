(* Grobner basis computations for K[X]-module *)
 (* Imports and abbreviations *)

    
open List;;
open Num;;

(* ------------------------------------------------------------------------- *)
(*  Utility functions                                                        *)
(* ------------------------------------------------------------------------- *)
  
let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let rec lexord_lt ord l1 l2 =
  match (l1,l2) with
    ([],[]) -> false
   |([],_) -> true
   |(_,[]) -> false
   | (h1::t1,h2::t2) -> if ord h1 h2 then true
                       else h1 = h2 && lexord_lt ord t1 t2;;
let rec tryfind f l =
  match l with
      [] -> failwith "tryfind"
    | (h::t) -> try f h with Failure _ -> tryfind f t;;


let rec distinctpairs l =
  match l with
   x::t -> itlist (fun y a -> (x,y) :: a) t (distinctpairs t)
  | [] -> [];;

(* ------------------------------------------------------------------------- *)
(* Defining polynomial types                                                 *)
(* ------------------------------------------------------------------------- *)

type id_var = int;;

type id_size = int;;

type vars = id_var list;;

type mon =
  { coeff : Num.num;
    vars : vars;
    length : int;
    size : id_size * id_size;
  };;

type pol = mon list;;

(* type pol_i = mon list * Expr.expr; *)

type i_var_set = int list;;

let mk_vmon (i:id_var) (size: id_size*id_size) :mon=
  {coeff = Num.Int 1; vars = [i]; length = 1; size};;

let is_null_mon (m:mon) =
   m.length=0 || (List.for_all (fun var -> var<0) m.vars)

let is_null (p:pol) : bool =
  match p with
  |[]-> true
  |m::_ -> is_null_mon m

let get_hd (p:pol) =
  if is_null p then
    None
  else
    Some (List.hd p)

let rec equals (p1:pol) (p2:pol) =
  match (get_hd p1,get_hd p2) with
       |None,None -> true
       |None, _ -> false
       |_,None -> false
       |Some m1,Some m2 -> m1=m2 && equals (List.tl p1) (List.tl p2)

let rec remainder (p:pol) =
  match p with
  |[] -> []
  |m::q -> if is_null_mon m then
             p
           else
             remainder q

let null_mon =
   { coeff = Int 0;
    vars = [];
    length = 0;
    size = (1,1);
  };;
(* ------------------------------------------------------------------------- *)
(* Operations on monomials.                                                  *)
(* ------------------------------------------------------------------------- *)

let veq_mon (m1:mon) (m2:mon) =
  (m1.length = m2.length ) && m1.vars=m2.vars;;

let mmul (m1:mon) (m2:mon) :mon  =
  if snd(m1.size) = fst(m2.size) then
  { coeff = m1.coeff*/m2.coeff;
    vars = m1.vars@m2.vars;
    length = m1.length + m2.length;
    size = (fst(m1.size),snd(m2.size));
  }
 else if m2.size=(-1,-1) then
   null_mon 
 else if m1.size=(-1,-1) then
   null_mon 
 else
   failwith "Monoms sizes uncompatible";;

exception NotPrefix;;

let rec is_prefix (m1:id_var list) (m2:id_var list) =
  match (m1,m2) with
     ([],[])-> ([],[])
    |([],m)-> ([],m)
    |(m,[]) -> (m,[])
    |(p1::q1,p2::q2) -> if p1 = p2 then 
                            is_prefix q1 q2
                        else
                           raise NotPrefix;;


(* ------------------------------------------------------------------------- *)
(* Monomial ordering.                                                        *)
(* ------------------------------------------------------------------------- *)

let morder_lt m1 m2 =
   m1.length < m2.length || (m1.length = m2.length &&  lexord_lt(<) m1.vars m2.vars);;

(* ------------------------------------------------------------------------- *)
(* Arithmetic on canonical multivariate polynomials.                         *)
(* ------------------------------------------------------------------------- *)

let mpoly_cmul c (pol:pol) :pol =
  if c = Int 0 then []
  else map (fun m -> {m with coeff=c*/m.coeff}) pol;;

let mpoly_mmul cm (pol:pol) :pol = map (mmul cm) pol;;

let mpoly_neg (pol) :pol = map (fun m -> {m with coeff=minus_num m.coeff}) pol ;;

let rec remove_null_mon (p:pol) =
  match p with
  |[] -> []
  |m::q -> if m.coeff = Int 0 then
             remove_null_mon q
           else
             m::(remove_null_mon q);;

let rec mpoly_add (l1:pol) (l2:pol):pol =
  match (l1,l2) with
    ([],_) -> remove_null_mon l2
  | (_,[]) -> remove_null_mon l1
  | (m1::o1,m2::o2) ->
        if veq_mon m1 m2 then
          let c = m1.coeff+/m2.coeff and rest = mpoly_add o1 o2 in
          if c = Num.Int 0 then rest
          else {m1 with coeff=c}::rest
        else if morder_lt m2 m1 then m1::(mpoly_add o1 l2)
        else m2::(mpoly_add l1 o2);;


let mpoly_mul l1 l2 =
  let rec aux_mul l1 l2 =
    match (l1,l2) with
      (_,[]) -> []
     |([],_)-> []
     |(p::q,l2) ->mpoly_add (mpoly_mmul p l2) (aux_mul q l2) in
  match (is_null l1, is_null l2) with
          |(false,false) ->  aux_mul l1 l2
          |_,_ -> [];;


let mpoly_sub l1 l2 = mpoly_add l1 (mpoly_neg l2);;

let polys_mul (l:pol list) =
  match l with
  |[] -> []
  |p::pols ->
    List.fold_right (fun pol acc -> mpoly_mul pol acc) pols p;;

let mpoly_muls ps = 
  match ps with
  | []      -> []
  | p :: ps -> 
     List.fold_left (fun p acc -> mpoly_mul p acc ) p ps;;


mpoly_muls  [[{coeff = Int 1; vars = [1]; length = 1; size = (2, 2)};
    {coeff = Int (-1); vars = [-1]; length = 0; size = (-1, -1)};
    {coeff = Int 1; vars = [-2]; length = 0; size = (-1, -1)}];
   [{coeff = Int 1; vars = [1]; length = 1; size = (2, 2)};
    {coeff = Int (-1); vars = [-1]; length = 0; size = (-1, -1)};
    {coeff = Int 1; vars = [-2]; length = 0; size = (-1, -1)}]];;

let s_poly (p1:pol) (p2:pol) =
  match (p1,p2) with
  |_,[] -> p1
  |[],_ -> p2
  |m1::_,m2::_-> match (m1.coeff,m2.coeff) with
                    |Int 0,_ -> p2
                    |_, Int 0 -> p1
                    |c1,c2 -> mpoly_sub p1 (mpoly_cmul (c1//c2) p2);;
(* Exemples *)
(*
let m1 = {coeff=Num.Int 1; vars=[27]; size=(2,2); length=1};;
let m2 = {coeff=Num.Int 1; vars=[27;78]; size=(2,4); length=2};;
let m3 = {coeff=Num.Int 1; vars=[27;27;78]; size=(2,4); length=3};;
let m4 = {coeff=Num.Int 1; vars=[27;27]; size=(2,2); length=2};;
let m5 = {coeff=Num.Int 1; vars=[78]; size=(2,4); length=1};;

let p1 = mpoly_add [m1] [m2];;
mpoly_mul [m1] [m2;m1];;

let base = DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]];;
DBase.get_all_prefix_lt base [1;2] ;;

get_all_products [1;2]   (DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]]);;

let base2 =  DBase.from_list [[m3];[m5]];;

monom_critical_pairs [1;1] base;;
monom_critical_pairs [1;1] base2;;

reduce_1 [m3] (DBase.from_list [[m2;m4];[m5;m3]]);;
reduce [m3;m1] (DBase.from_list [[m4;m1];[m5];[m2;m1];[m1]]);;


let lb =  [[m2];[m2;m1]];;
get_all_products m1.vars (DBase.from_list lb);;
reduce_1 [m1] (DBase.from_list lb);;
deduce [m1] lb;;
deduce [m2] lb;;
deduce [m3] lb;;
deduce [m4] lb;;
deduce [m5] lb;;
inverter [m1] lb;;
inverter [m2] lb;;
inverter [m3] lb;;
inverter [m4] lb;;
(*inverter [m5] lb;;*)

*)


