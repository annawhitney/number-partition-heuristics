open Heap
open Core_in_channel

let kk (lst : int list) : int =
    (* Insert all ints into heap. *)
    let l = List.length lst in
    let start_hp = List.fold_left MaxIntBH.insert (MaxIntBH.empty l) lst in

    (* Apply differencing recursively. *)
    let rec kk_step (hp : MaxIntBH.heap) : int =
        let max1, hp1 = MaxIntBH.extract_min hp in
        let max2, hp2 = MaxIntBH.extract_min hp1 in

        let diff = max1 - max2 in
        if MaxIntBH.is_empty hp2 then diff
        else kk_step (MaxIntBH.insert hp2 diff)
    in
    kk_step start_hp
;;

let filename = Sys.argv.(1) ;;
let str_nums = read_lines filename ;;
let nums = List.map int_of_string str_nums ;;

let residue = kk nums ;;
Printf.printf "%i\n" residue ;;
