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

let version = "0.1"

(****************************************)
module IntSet = Set.Make(Int64)

type file = (string * Unix.LargeFile.stats)
type entry = A of file
	   | B of file
type trash = Pretend | Trash | Delete 
type checktype = SizeMod | Sparsecheck | Fullcheck (* Checksum *)

(** Trash means using the my unix "trash" command to move the file to the trash.
    Delete means using Unix unlink. *)
(** SizeMod means that two files must only have the same modtime + size to match.
    Sparsecheck checks a random subset of bytes in the file.
    Fullcheck compares the entire files (and also requires same modtime). *)

(****************************************)


(* For now we just statically allocate this hash structure.  We make it pretty big.*)
let thehash = Hashtbl.create 10000;;
let actually_unlink = ref false;;
let trash_mode = ref Pretend;;
let check_level = ref Fullcheck;;

let show_entry = function
  | A (name,stats) -> sprintf "A %s %Ld" name stats.st_size
  | B (name,stats) -> sprintf "B %s %Ld" name stats.st_size;;
let printhash () =
  Hashtbl.iter (fun k x -> 
		  printf " %Ld : %s \n" k (show_entry x);)
    thehash;;


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
      Adir (_, rest) -> fold_left (+) 0 (map (add_tree wrap) (force rest))
    | Alink _ -> 0
    | Afile (name, stats, path) -> 
	Hashtbl.add thehash stats.st_size (wrap (path,stats));
	1;;


(** This takes a list of entries matching a particular key.  It
  returns a list of all A-B pairings that might represent potential
  matches. *)
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
  let alst = RList.filter_some (map (function A x -> Some x | _ -> None) elst) 
  and blst = RList.filter_some (map (function B x -> Some x | _ -> None) elst) in
    product alst blst;;
        


(** Takes two file entries and verifies they match. *)
(* This is a simple version that needs to be improved.  It should do a
   checksum.  TODO! *)
let verify_match (f1,s1) (f2,s2) = 
  (s1.st_size = s2.st_size) &&
  (s1.st_mtime = s2.st_mtime) &&
  (*  s1.st_ctime = s2.st_ctime*)
  match !check_level with
      SizeMod -> true
    | Sparsecheck -> failwith "sparsecheck not implemented"
    | Fullcheck -> failwith "fullcheck not implemented"
;;


let find_dupes () =
  concat
    (map 
       (fun key -> 
	  filter (fun (x,y) -> verify_match x y)
	  (potential_matches key (Hashtbl.find_all thehash key)))
       (IntSet.elements (keys thehash)));;

		    
let load_dirs a b = 
  let dira = read_adir a
  and dirb = read_adir b
  in 
    force_atree dira;
    force_atree dirb;
    printf "Woot, what up?\n";
    print_tree (tree_of_atree dira);
    printf "And B tree\n";
    print_tree (tree_of_atree dirb);
    printf "ADDED STUFF: %d\n" (add_tree (fun x -> A x) dira); 
    printf "ADDED MORE: %d\n" (add_tree (fun x -> B x) dirb);
    printf "Got total: %d\n" (Hashtbl.length thehash);
(*    printf "Add files: %d\n" (add_tree 
*)
(*    ignore (find_dupes ()); *)
    printf "Hash contents:\n ";
    printhash();
;;


let symlink_dupes dupes =
  let check_return cmd stat = 
    match stat with 
	WEXITED n -> () (* Exited ok *)
      | WSIGNALED n -> failwith (sprintf "Command %s was killed by signal %d\n" cmd n)
      | WSTOPPED n  -> failwith (sprintf "Command %s was stopped by signal %d\n" cmd n)
  in
  (* Link file1 to file2 *)
  iter (fun ((f1,s1), (f2,s2)) -> 
	  printf "%s ->\n" f1;
	  printf "  %s\n" f2;
	  flush Pervasives.stdout;
	  match !trash_mode with 
	      Pretend -> ()
	    | Trash -> 
		let cmd = ("trash " ^ f1) in
		  check_return cmd (system cmd);
		  symlink f2 f1
	    | Delete ->
		unlink f1;
		symlink f2 f1)
    dupes;;


(********************************************************************)


let print_version () = 
  print_string "symlinkdupes: ver ";
  print_endline version;
  exit 0;;

let print_help () = 
  print_endline "Usage: dirsize/ds <options> <path-names>";
  print_endline "options are:";
  print_endline "  -h   Show this help message.";
  print_endline "  -v   Show the version.";
  print_endline "  -c1  Use only filesize/modtime to determine equivalence.";
  print_endline "  -c2  Check a random subset of bytes to determine equivalence.";
  print_endline "  -c3  Check entire files to determine equivalence. (default)";
  print_endline "  -t   Use 'trash' command to delete duplicates.";
  print_endline "  -f   Use rm to force deletion of duplicates.";  
  print_endline "  -p   Only pretend; print the links that would (default).";
(*  print_endline "  -c   Use ANSI color.  Green and red theme.";
  print_endline "  -b   Use ANSI color.  Blue and yellow theme.";
  print_endline "  -nc  Don't use color.  Plain ASCII.";
  print_endline "  -s   Sort output by filesize, increasing."*)
;;


(********************************************************************)
(* Main script *)

let main () = 
  let args = List.filter
	       (* Process flags *)
	       (function
		  | "-h"  -> print_help (); exit 0;
		  | "-v"  -> print_version (); false
		  | "-c1"  -> check_level := SizeMod; false
		  | "-c2"  -> check_level := Sparsecheck; false
		  | "-c3"  -> check_level := Fullcheck; false
		  | "-p"  -> trash_mode := Pretend; false
		  | "-t"  -> trash_mode := Trash; false
		  | "-f"  -> trash_mode := Delete; false
		  | _     -> true)
	       (List.tl (Array.to_list Sys.argv))
  in 
    if length args < 2 
    then print_help ()
    else 
      begin
	load_dirs (nth args 0) (nth args 1);
	symlink_dupes (find_dupes ());
      end;;

main ();;






(*  let asizes = fold_left (fun set -> function 
			      (A (name,stat)) -> IntSet.add stat.st_size set
			    | (B _) -> raise (Failure "should not happen"))
		 IntSet.empty alst
  and bsizes = fold_left (fun set -> function
			      (B (name,stat)) -> IntSet.add stat.st_size set
			    | (A _) -> raise (Failure "should not happen"))
		 IntSet.empty blst 
  in
    (* do we have any files exactly the same size? *)
    printf "intersection at key %Ld (%d | %d) %b is empty %b\n"
      key (IntSet.cardinal asizes) (IntSet.cardinal bsizes)
      (asizes = bsizes)
      (IntSet.is_empty (IntSet.inter asizes bsizes));
   
    map (fun e1 -> map
	   (fun e2 -> (e1,e2)) blst) alst;;*)

