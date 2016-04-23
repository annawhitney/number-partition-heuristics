open Solution

let rec rep_random 

let main () =
    let soln_flag = Sys.argv.(1) in
    let s = match soln_flag with
            | "S" -> (module StandardSoln : SOLUTION)
            | "P" -> (module PrepartitionSoln : SOLUTION)
    in
    let module Soln = (val s : SOLUTION)
    in


