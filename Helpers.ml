
let get_one_index (l : int) : int =
    let () = Random.self_init () in
    Random.int l

let get_two_indices (l : int) : int * int =
    (*let () = Random.self_init () in*)
    let i = Random.int l in
    let rec not_i idx =
        let j = Random.int l in
        if j = idx then not_i idx else j
    in
    let j = not_i i in
    i, j

let neg_or_pos_one () : int =
    let () = Random.self_init () in
    if Random.bool () then 1 else (-1)

let zip_fast l1 l2 = List.rev_map2 (fun a b -> (a,b)) l1 l2

let group_by_partition (acc : (int * int) list) : (int * int) list =

