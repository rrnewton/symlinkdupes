(**
  [2005.01.31]


*)


open Set
open Lazy
open List
open Dirtree
open Printf
open Unix
open Unix.LargeFile
open Rutils
(*open Hashtbl*)


module IntSet = Set.Make(Int64)

type file = (string * Unix.LargeFile.stats)
type entry = A of file
	   | B of file

(* For now we just statically allocate this hash structure.  We make it pretty big.*)
let thehash = Hashtbl.create 10000


let product ls1 ls2 = 
  let pair x y = (x,y) in
    if ls2 == []
    then []
    else 
      let rec loop = function
	  [] -> []
	| (h::t) -> (map (pair h) ls2) @ (loop t)
      in loop ls1


(* Returns a SET of keys used in a hashtable. *)
let keys hsh = 
  Hashtbl.fold (fun key _ set -> IntSet.add key set) 
    thehash IntSet.empty

let rec add_tree wrap tree =
  match tree with 
      Ldir (_, rest) -> fold_left (+) 0 (map (add_tree wrap) (force rest))
    | Llink _ -> 0
    | Lfile (name, stats) -> 
	Hashtbl.add thehash stats.st_size (wrap (name,stats));
	1;;


(** This takes a list *)
(*val potential_matches : entry list -> (file,file) list*)
(* All entries in the entry list will have size equivalent to the key. *)
let potential_matches key elst =
  printf "Key entries: ";
  RList.print (fun x -> x)
    (map (function 
	    | A (n,_) -> "A_"^n^"_"^ Int64.to_string key
	    | B (n,_) -> "B_"^n^"_"^ Int64.to_string key)
       elst);
  printf "\n";
  let alst = filter (function A _ -> true | _ -> false) elst 
  and blst = filter (function B _ -> true | _ -> false) elst in
  let asizes = fold_left (fun set (A (name,stat))  -> 
			    IntSet.add stat.st_size set)
		 IntSet.empty alst
  and bsizes = fold_left (fun set (B (name,stat)) -> 
			    IntSet.add stat.st_size set)
		 IntSet.empty blst 
  in
    (* do we have any files exactly the same size? *)
    printf "intersection at key %Ld (%d | %d) %b is empty %b\n"
      key (IntSet.cardinal asizes) (IntSet.cardinal bsizes)
      (asizes = bsizes)
      (IntSet.is_empty (IntSet.inter asizes bsizes));
   
    map (fun e1 -> map
	   (fun e2 -> (e1,e2)) blst) alst;;

let find_dupes () =
  IntSet.iter (fun key -> 
		 potential_matches key (Hashtbl.find_all thehash key))
    (keys thehash);;


(*		   if List.length lst <= 1
		   then ()
		   else if *)
		     


(* (wrap (name,stats))*)

let main () = 
  let dira = read_ldir Sys.argv.(1)
  and dirb = read_ldir Sys.argv.(2)
in 
    force_ltree dira;
    force_ltree dirb;
    printf "Woot, what up?\n";
    print_tree (tree_of_ltree dira);
    printf "And B tree\n";
    print_tree (tree_of_ltree dirb);
    printf "ADDED STUFF: %d\n" (add_tree (fun x -> A x) dira); 
    printf "ADDED MORE: %d\n" (add_tree (fun x -> B x) dirb);
    printf "Got total: %d\n" (Hashtbl.length thehash);
(*    printf "Add files: %d\n" (add_tree 
*)
    find_dupes (); 
;;


	       
main ();;
