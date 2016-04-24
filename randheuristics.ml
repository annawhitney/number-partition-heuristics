open Solution

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

let main () =
    let soln_flag = Sys.argv.(1) in
    let soln = match soln_flag with
            | "S" -> (module StandardSoln : SOLUTION)
            | "P" -> (module PrepartitionSoln : SOLUTION)
    in
    let module Soln = (val soln : SOLUTION)
    in

;;

