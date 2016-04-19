open Heap

let filename = Sys.argv.(1)
let infile = open_in filename

let kk (lst : int list) : int =
    (* Insert all ints into heap. *)

    (* Apply differencing recursively. *)
    let rec kk_step (hp : IntBH.heap) 
