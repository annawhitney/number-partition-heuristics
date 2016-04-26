open Helpers
open Karmarkar_karp
open Core_in_channel

let main () : unit =
    let filename = Sys.argv.(1) in

    if filename = "run_tests" then
        (assert (kk [] = 0);
        assert (kk [0;0;0;0;0] = 0);
        assert (kk [10;8;7;6;5] = 2);
        assert (kk [5;6;7;8;10] = 2);
        assert (kk [33;25;53;21;58;58;37;11;26;82] = 0))
    else if Array.length Sys.argv > 2 then
        if Sys.argv.(2) = "time" then
        let str_nums = read_lines filename in
        let nums = List.map int_of_string str_nums in
        let t, res = time kk nums in
        Printf.printf "%i %f\n" res t
        else failwith "Extraneous arguments."
    else
        let str_nums = read_lines filename in
        let nums = List.map int_of_string str_nums in
        let residue = kk nums in
        Printf.printf "Karmarkar-Karp: %i\n" residue
;;

main () ;;
