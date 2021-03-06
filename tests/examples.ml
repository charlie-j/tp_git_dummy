(* Grobner basis computations for K[X]-module *)
 (* Imports and abbreviations *)

#use "topfind";;
#require "num";;
#require "batteries";;
#cd "../_build/src/";;
#load "libdummy.cma";;

open NC_Gasbi

let test1 =mpoly_muls [[{coeff = Int 1; vars = [1]; length = 1; size = (2, 2)};
    {coeff = Int (-1); vars = [-1]; length = 0; size = (-1, -1)};
    {coeff = Int 1; vars = [-2]; length = 0; size = (-1, -1)}];
   [{coeff = Int 1; vars = [1]; length = 1; size = (2, 2)};
    {coeff = Int (-1); vars = [-1]; length = 0; size = (-1, -1)};
    {coeff = Int 1; vars = [-2]; length = 0; size = (-1, -1)}]];;

let m1 = {coeff=Num.Int 1; vars=[27]; size=(2,2); length=1};;
let m2 = {coeff=Num.Int 1; vars=[27;78]; size=(2,4); length=2};;
let m3 = {coeff=Num.Int 1; vars=[27;27;78]; size=(2,4); length=3};;
let m4 = {coeff=Num.Int 1; vars=[27;27]; size=(2,2); length=2};;
let m5 = {coeff=Num.Int 1; vars=[78]; size=(2,4); length=1};;

let lb =  [[m2];[m2;m1]];;

let p1 = mpoly_add [m1] [m4];;
let p2 = mpoly_add (mpoly_neg p1) [m1];;
let p3 = mpoly_mul p1 (mpoly_neg p1);;
let p4 = mpoly_mul p2 p1;;
mpoly_mul [m1] [m2;m1];;

let base = DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]];;
DBase.get_all_prefix_lt base [1;2] ;;

get_all_products [1;2]   (DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]]);;

let base2 =  DBase.from_list [[m3];[m5]];;

monom_critical_pairs [1;1] base;;
monom_critical_pairs [1;1] base2;;

inverter [m1] lb;;
inverter [m2] lb;;
inverter [m3] lb;;
inverter [m4] lb;;




