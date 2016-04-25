open Heap

let kk (lst : int list) : int =
    let l = List.length lst in

    (* Differencing no elements gives us zero. *)
    if l = 0 then 0 else

    (* Insert all ints into heap. *)
    let start_hp = List.fold_left MaxIntBH.insert (MaxIntBH.empty l) lst in

    (* Apply differencing recursively. *)
    let rec kk_step (hp : MaxIntBH.heap) : int =
        let max1, hp1 = MaxIntBH.extract_min hp in
        if MaxIntBH.is_empty hp1 then max1 else

        let max2, hp2 = MaxIntBH.extract_min hp1 in

        let diff = max1 - max2 in
        if MaxIntBH.is_empty hp2 then diff
        else kk_step (MaxIntBH.insert hp2 diff)
    in
    kk_step start_hp
;;


