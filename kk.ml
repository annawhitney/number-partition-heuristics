open Heap
open Core_in_channel

let kk (lst : int list) : int =
    let l = List.length lst in

    (* Differencing no elements gives us zero. *)
    if l = 0 then 0 else

    (* Insert all ints into heap. *)
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

let main () : unit =
    let filename = Sys.argv.(1) in

    if filename = "run_tests" then
        (assert (kk [] = 0);
        assert (kk [0;0;0;0;0] = 0);
        assert (kk [10;8;7;6;5] = 2);
        assert (kk [5;6;7;8;10] = 2);
        assert (kk [33;25;53;21;58;58;37;11;26;82] = 0))
    else
        let str_nums = read_lines filename in
        let nums = List.map int_of_string str_nums in
        let residue = kk nums in
        Printf.printf "%i\n" residue
;;

main () ;;
