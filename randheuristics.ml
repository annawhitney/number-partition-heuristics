open Solution
open Helpers
open Core_in_channel

let soln_flag = Sys.argv.(1) ;;
let soln = match soln_flag with
        | "S" -> (module StandardSoln : SOLUTION)
        | "P" -> (module PrepartitionSoln : SOLUTION)
;;
let module Soln = (val soln : SOLUTION) ;;

let rep_random (lst : int list) : int =
    let l = List.length lst in
    let rec next_random iter best_res =
        if iter = 0 then best_res else
        let s = Soln.generate l in
        let new_res = Soln.get_residue s in
        if new_res < best_res then next_random (iter - 1) new_res
        else next_random (iter - 1) best_res
    in
    next_random 25000 (Soln.get_residue (Soln.generate l))
;;

let hill_climb (lst : int list) : int =
    let l = List.length lst in
    let rec uphill iter curr_s curr_res =
        if iter = 0 then curr_res else
        let s = Soln.random_move curr_s in
        let new_res = Soln.get_residue s in
        if new_res < curr_res then uphill (iter - 1) s new_res
        else uphill (iter - 1) curr_s curr_res
    in
    let first_s = Soln.generate l in
    uphill 25000 first_s (Soln.get_residue first_s)
;;

let sim_anneal (lst : int list) (t_cool : int -> float) : int =
    let l = List.length lst in
    let rec anneal iter curr_s curr_res =
        if iter = 0 then curr_res else
        let s = Soln.random_move curr_s in
        let new_res = Soln.get_residue s in
        if new_res < curr_res then anneal (iter - 1) s new_res
        else
            let p = exp (-. (float_of_int (new_res - curr_res)) /. (t_cool iter)) in
            if with_prob p then anneal (iter - 1) s new_res
            else anneal (iter - 1) curr_s curr_res
    in
    let first_s = Soln.generate l in
    anneal 25000 first_s (Soln.get_residue first_s)
;;

let main () =
    let filename = Sys.argv.(2) in
    let str_nums = read_lines filename in
    let nums = List.map int_of_string str_nums in

    let rr = rep_random nums in
    let hc = hill_climb nums in
    let sa = sim_anneal nums in

    Printf.printf "%i & %i & %i\n" rr hc sa
;;

main () ;;
    
