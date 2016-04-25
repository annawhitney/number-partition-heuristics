open Core
open Helpers
open Kk

module type SOLUTION =
sig
    type soln
    type vals

    (* Create a random starting solution. *)
    val generate : int -> soln

    (* Perform a random move from one solution to a neighbor. *)
    val random_move : soln -> soln

    (* Determine residue associated with a solution. *)
    val get_residue : soln -> vals -> int
end

module StandardSoln : (SOLUTION with type soln = int array and type vals = int list) =
struct
    type soln = int array
    type vals = int list

    let generate n =
        Array.init n (fun _ -> neg_or_pos_one ())

    let random_move seq =
        let l = Array.length seq in
        let i, j = get_two_indices l in
        let () = seq.(i) <- (- seq.(i)) in
        if Random.bool () then
            let () = seq.(j) <- (- seq.(j)) in
            seq
        else seq

    let get_residue s v =
        let svs = zip_fast (Array.to_list s) v in
        let prods = List.rev_map (fun (a,b) -> a * b) svs in
        List.fold_left (+) 0 prods
end

module PrepartitionSoln : (SOLUTION with type soln = int array and type vals = int list) =
struct
    type soln = int array
    type vals = int list

    let generate n =
        Array.init n (fun _ -> get_one_index n)

    let random_move pp =
        let l = Array.length pp in
        let rec not_pi () =
            let i, j = get_two_indices l in
            if pp.(i) = j then not_pi () else i, j
        in
        let i, j = not_pi () in
        let () = pp.(i) <- j in
        pp

    let get_residue s v =
        let svs = zip_fast (Array.to_list s) v in
        let sums_by_part = List.fold_left group_by_partition [] svs in
        let _, sums = unzip sums_by_part in
        kk sums
end
