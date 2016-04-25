open Core

let get_one_index (l : int) : int = Random.int l

let get_two_indices (l : int) : int * int =
    let i = Random.int l in
    let rec not_i idx =
        let j = Random.int l in
        if j = idx then not_i idx else j
    in
    let j = not_i i in
    i, j

let neg_or_pos_one () : int =
    if Random.bool () then 1 else (-1)

let with_prob (p : float) : bool = Random.float 1. < p

let zip_fast l1 l2 = List.rev_map2 (fun a b -> (a,b)) l1 l2

let group_by_partition (acc : (int * int) list) (sv : (int * int)) : (int * int) list =
    let (s, v) = sv in
    if List.exists (fun (a,_) -> a = s) acc then
        List.map (fun (a,b) -> if a = s then (a,b+v) else (a,b)) acc
    else
        sv :: acc

(* Adapted from http://stackoverflow.com/questions/9061421/running-time-in-ocaml *)
let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in (stop -. start), res
;;

