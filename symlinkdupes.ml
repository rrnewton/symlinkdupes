(**
  [2005.01.31]
*)

(** TODO: In verbose mode, flush output port more often. *)

(** TODO: Read back the symlink and verify that it exists before unlinking the original. *)

(** TODO: Need to also make sure that we don't try to link one location two destinations.*)

(** TODO: Try for atomicity... Canceling the execution of the program currently causes Bad Things. *)

open Set
open Lazy
open List
open Dirtree
open Printf
open Unix
open Unix.LargeFile
open Rutils
(*open Hashtbl*)

let version = "0.2.2"

(****************************************)
module IntSet = Set.Make(Int64)

type file = (string * Unix.LargeFile.stats)
type entry = A of file
	   | B of file
(** FillLink is uncompleted... it was going to replace symlinks with full files.
    That's silly though, we should just repair symlinks and use a simple operation to fill them in. *)
type linkop = Pretend | Trash | Delete | FillLink
type checktype = NameOnly | NameAndSize | SizeOnly | SizeMod | Sparsecheck | Fullcheck (* Checksum *)

(** Trash means using the my unix "trash" command to move the file to the trash.
    Delete means using Unix unlink. *)
(** SizeMod means that two files must only have the same modtime + size to match.
    Sparsecheck checks a random subset of bytes in the file.
    Fullcheck compares the entire files (does not require same modtime). *)

(****************************************)


(* For now we just statically allocate this hash structure.  We make it pretty big.*)
let thehash = Hashtbl.create 10000;;
let actually_unlink = ref false;;
let link_mode = ref Pretend;;
let check_level = ref Fullcheck;;
let min_filesize = ref Int64.zero;;
let verbose = ref false;;
let quiet = ref false;;

let show_entry = function
  | A (name,stats) -> sprintf "A %s %Ld" name stats.st_size
  | B (name,stats) -> sprintf "B %s %Ld" name stats.st_size;;
let printhash () =
  Hashtbl.iter (fun k x -> 
		  printf " %Ld : %s \n" k (show_entry x);)
    thehash;;

let print_progress = 
  let counter = ref 0 in
    (fun () ->
       if (not !quiet) then
       if !counter >= 500
       then (counter := !counter - 500;
	     print_char '.';
	     Pervasives.flush Pervasives.stdout));;

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
    | Alink ((name, stats, path), link) -> 
	if !link_mode = FillLink
	then (Hashtbl.add thehash stats.st_size (wrap (path,stats)); 
	      print_progress (); 1)
	else 0
    | Afile (name, stats, path) -> 
	if stats.st_size >= !min_filesize
	then (Hashtbl.add thehash stats.st_size (wrap (path,stats)); 
	      print_progress (); 1)
	else 0;;

(** This takes a list of entries matching a particular key.  It
  returns a list of all A-B pairings that might represent potential
  matches. *)
(*val potential_matches : entry list -> (file,file) list*)
(* All entries in the entry list will have size equivalent to the key. *)
let potential_matches key elst =
  if !verbose then 
    begin 
      printf "Key entries: ";
      RList.print (fun x -> x)
	(map (function 
		| A (n,_) -> "A_"^n^"_"^ Int64.to_string key
		| B (n,_) -> "B_"^n^"_"^ Int64.to_string key)
	   elst);
      printf "\n";
    end;
  let alst = RList.filter_some (map (function A x -> Some x | _ -> None) elst) 
  and blst = RList.filter_some (map (function B x -> Some x | _ -> None) elst) in
    product alst blst;;
        


(** Takes two file entries and verifies they match. *)
(* This is a simple version that needs to be improved.  It should do a
   checksum.  TODO! *)
let verify_match (f1,s1) (f2,s2) = 
  (* This should always be true: *)
  if !check_level == NameOnly 
    (* This won't work yet because the hash table's by size. *)
  then Filename.basename f1 = Filename.basename f2
  else if !check_level == NameAndSize 
  then (Filename.basename f1 = Filename.basename f2 &&
        s1.st_size = s2.st_size)
  else 
    (s1.st_size = s2.st_size) &&
  (*  s1.st_ctime = s2.st_ctime*)
  match !check_level with
    | NameOnly -> failwith ""
    | NameAndSize -> failwith ""
    | SizeMod -> (s1.st_mtime = s2.st_mtime)
    | SizeOnly -> true

    | Sparsecheck -> 
	let num_samples = min (Int64.div s1.st_size (Int64.of_int 2500))
	                      (Int64.of_int 500) in
	let samples = ref (sort Int64.compare
	                     (RList.biginit num_samples 
				(fun _ -> Random.int64 s2.st_size))) 
	and in1 = openfile f1 [O_RDONLY] s1.st_perm
	and in2 = openfile f2 [O_RDONLY] s2.st_perm 
	and abuff = String.create 50
	and bbuff = String.create 50 
	and modcounter = ref 0
	and match_thusfar = ref true in
	  if !verbose then printf "Comparing files: ";
	  while !samples != [] && !match_thusfar do
	    let sample_point = hd !samples in
	      (*printf "<%Ld> " sample_point;*)
	      ignore (LargeFile.lseek in1 sample_point SEEK_SET);
	      ignore (LargeFile.lseek in2 sample_point SEEK_SET);
	      let aread = read in1 abuff 0 50
	      and bread = read in2 bbuff 0 50 in
		if aread > 0 && bread > 0 then
		  (* If we got some reading then we consider this point sampled. *)
		  samples := tl !samples;
		  for i = 0 to (min aread bread) - 1 do
		    if abuff.[i] != bbuff.[i]
		    then match_thusfar := false;		    
		  done;
		  if !verbose then 
		    (incr modcounter;
		     if !modcounter = 100 (* Every 100 compared *)
		     then (modcounter := 0; 
			   print_char '.';
			   flush Pervasives.stdout));
	  done;
	  if !verbose then printf "\n";
	  close in1;
	  close in2;
	  !match_thusfar;
	  
    | Fullcheck -> 
	let in1 = openfile f1 [O_RDONLY] s1.st_perm
	and in2 = openfile f2 [O_RDONLY] s2.st_perm 
	and abuff = String.create 17000
	and bbuff = String.create 17000
	and counter = ref Int64.zero
	and modcounter = ref 0
	and match_thusfar = ref true in
	  if !verbose then printf "Comparing files: ";
	  while !counter < s1.st_size && !match_thusfar do
	    let aread = read in1 abuff 0 17000
	    and bread = read in2 bbuff 0 17000 in
	    let thisread = min aread bread in
(*	      printf "Read: %d/%d = %d counter %Ld\n" aread bread thisread !counter;*)
	      if thisread > 0 then
		for i = 0 to thisread - 1 do
		  if abuff.[i] != bbuff.[i]
		  then match_thusfar := false;		    
		done;
	      (*ignore (LargeFile.lseek in1 (Int64.from_int thisread) SEEK_CUR);
	      ignore (LargeFile.lseek in2 (Int64.from_int thisread) SEEK_CUR);*)
	      (* Move forward by how much we successfully read. *)
	      counter := Int64.add !counter (Int64.of_int thisread);
	      (* Seek to the position for the next read.*)
	      ignore (lseek in1 !counter SEEK_SET);
	      ignore (lseek in2 !counter SEEK_SET);

	      if !verbose then 
		(modcounter := !modcounter + thisread;
		 if !modcounter > 100000 (* Every 100K bytes compared *)
		 then (modcounter := !modcounter - 100000; 
		       print_char '.';
		       flush Pervasives.stdout;));
	  done;
	  (if !verbose then printf "\n");
	  close in1;
	  close in2;
	  !match_thusfar;
;;

(* 	let in1 = open_in_bin f1
	and in2 = open_in_bin f2 in
	let counter = ref s1.st_size
	and modcounter = ref 0
	and match_thusfar = ref true in
	  printf "Comparing files: ";
	  while !counter > Int64.zero && !match_thusfar do
	    let a = input_byte in1
	    and b = input_byte in2 in
	      (* We just read off a byte: *)
	      counter := Int64.sub !counter Int64.one;
	      if a != b then match_thusfar := false;
	      incr modcounter;
	      if !modcounter = 100000 (* Every 100K compared *)
	      then (modcounter := 0; 
		    print_char '.';
		    flush Pervasives.stdout)
	  done;
	  printf "\n";
	  close_in in1;
	  close_in in2;
	  !match_thusfar;
*)


let report_match _ _ = ()

let report_link (f1,s1) (f2,s2) =
  printf "%s ->\n" f1;
  printf "  %s\n" f2;
  flush Pervasives.stdout;;

let find_dupes () =
  concat
    (map 
       (fun key -> 
	  filter (fun (x,y) -> 
		    if verify_match x y
		    then (report_match x y; true)
		    else false)
	  (potential_matches key (Hashtbl.find_all thehash key)))
       (IntSet.elements (keys thehash)));;

		    
let load_dirs a b = 
  let dira = read_adir a
  and dirb = read_adir b
  in 
(*    force_atree dira;*)
(*    force_atree dirb;*)
    let a_added = add_tree (fun x -> A x) dira 
    and b_added = add_tree (fun x -> B x) dirb in
      if !verbose then       
	begin
	  printf "This is the A tree:\n";
	  print_tree (tree_of_atree dira);
	  printf "This is the B tree:\n";
	  print_tree (tree_of_atree dirb);
	  printf "ADDED A entries: %d\n" a_added; 
	  printf "ADDED B entries: %d\n" b_added;
	  printf "Got total entries: %d\n" (Hashtbl.length thehash);
	  printf "Hash contents:\n ";
	  printhash();
	end;
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
	  report_link (f1,s1) (f2,s2);
	  match !link_mode with 
	      Pretend -> ()
	    | Trash -> 
		let cmd = ("trash " ^ Filename.quote f1) in
		  check_return cmd (system cmd);
		  symlink f2 f1
	    | Delete ->
		unlink f1;
		symlink f2 f1
	    | FillLink -> failwith "FillLink not implemented"
       )
    dupes;;


(********************************************************************)
(*
let explode_path pth =
  let loop s = 
    if dirname s = basename s ||
      basename s = ""
    then []
    else basename s :: loop (dirname s)
  in List.reverse (loop pth);;
*)
(*let repair_link*)


(********************************************************************)


let print_version () = 
  print_string "symlinkdupes: ver ";
  print_endline version;
  exit 0;;

let print_help () = 
  print_endline "Usage: symlinkdupes <sourcepath> <destpath>";
  print_endline "options are:";
  print_endline "  -version   Show the version.";
  print_endline "  -repair    Repair symlink mode. Not yet...";
  print_endline "  -h    Show this help message.";
  print_endline "  -v    Verbose output.";
  print_endline "  -q    Quiet.  No progress dots.  Still lists the links.";
  print_endline "  -c1   Use only filesize/modtime to determine equivalence.";
  print_endline "  -c2   Check a random subset of bytes to determine equivalence.";
  print_endline "  -c3   Check entire files to determine equivalence. (default)";
  print_endline "  -c0   DANGER.  Uses ONLY filesize to establish equivalence.";
  print_endline "  -cn   Uses filename and file size.";
  print_endline "  -cN   DANGER.  Uses only filename... (Not Implemented).";
  print_endline "  -m<n> Only compare files above size n kilobytes.";
  print_endline "  -t    Use 'trash' command to delete duplicates.";
  print_endline "  -f    Use rm to force deletion of duplicates.";  
  print_endline "  -p    Only pretend; print the links that would be deleted (default).";
(*  print_endline "  -c   Use ANSI color.  Green and red theme.";
  print_endline "  -b   Use ANSI color.  Blue and yellow theme.";
  print_endline "  -nc  Don't use color.  Plain ASCII.";
  print_endline "  -s   Sort output by filesize, increasing."*)
;;


(********************************************************************)
(* Main script *)

let main () = 
  (*Random.init 399;*)
  Random.self_init ();
  let args = List.filter
	       (* Process flags *)
	       (function
		  | "-h"  -> print_help (); exit 0;
		  | "-version"  -> print_version (); exit 0
		  | "-v"   -> verbose := true; false
		  | "-q"   -> quiet := true; false
		  | "-c0"  -> check_level := SizeOnly; false
		  | "-c1"  -> check_level := SizeMod; false
		  | "-c2"  -> check_level := Sparsecheck; false
		  | "-c3"  -> check_level := Fullcheck; false
		  | "-cn"  -> check_level := NameAndSize; false
(*		  | "-cN"  -> check_level := NameOnly; false*)
		  | "-p"  -> link_mode := Pretend; false
		  | "-t"  -> link_mode := Trash; false
		  | "-f"  -> link_mode := Delete; false
		  | s     -> (if String.length s > 2 && String.sub s 0 2 = "-m"
			      then (min_filesize := 
				    Int64.mul (Int64.of_int 1000)
				      (Int64.of_string (String.sub s 2 (String.length s - 2)));
			            false)
			      else true))
	       (List.tl (Array.to_list Sys.argv))
  in 
    if length args != 2 
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

