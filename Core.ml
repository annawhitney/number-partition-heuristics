(* A few functions borrowed from Jane Street's Core version of the standard
 * OCaml library, since Core provides tail-recursive versions of many standard
 * functions and is not available on the nice.fas.harvard.edu machines. *)
(*****************************************************************************)
let concat_no_order l = List.fold_left (fun acc l -> List.rev_append l acc) [] l

let init n ~f =
    let rec loop i accum =
        assert (i >= 0);
        if i = 0 then accum
        else loop (i-1) (f (i-1) :: accum)
    in
    loop n []
    
let rev = function
    | [] | [_] as res -> res
    | x :: y :: rest -> List.rev_append rest [y; x]

let partition_map t ~f =
    let rec loop t fst snd =
        match t with
        | [] -> (rev fst, rev snd)
        | x :: t ->
            match f x with
            | `Fst y -> loop t (y :: fst) snd
            | `Snd y -> loop t fst (y ::
            snd)
            in
            loop t [] []

let partition_tf t ~f =
    let f x = if f x then `Fst x else `Snd x in
    partition_map t ~f

type ordering = Less | Equal | Greater 

let foldi t ~init ~f =
    let rec loop i ac =
        if i = Array.length t then
            ac
        else loop (i + 1) (f i ac t.(i))
    in
    loop 0 init

exception Finally of exn * exn

let protectx ~f x ~(finally : _ -> unit) =
    let res =
        try f x
        with exn ->
            (try finally x with final_exn -> raise (Finally (exn, final_exn)));
            raise exn
    in
    finally x;
    res

(*let unzip lst =*)
    (*let rec loop lst l1 l2 =*)
        (*match lst with*)
        (*| [] -> (List.rev l1, List.rev l2)*)
        (*| (x, y) :: tl -> loop tl (x :: l1) (y :: l2)*)
    (*in*)
    (*loop lst [] []*)

(*let transpose =*)
    (*let rec transpose_aux t rev_columns =*)
    (*match partition_map t ~f:(function [] -> `Snd () | x :: xs -> `Fst*)
    (*(x, xs)) with*)
    (*| (_ :: _, _ :: _) -> None*)
    (*| ([], _) -> Some (rev_append rev_columns [])*)
    (*| (heads_and_tails, []) ->*)
    (*let (column, trimmed_rows) = unzip*)
    (*heads_and_tails in*)
    (*transpose_aux trimmed_rows (column*)
    (*:: rev_columns)*)
    (*in*)
    (*fun t ->*)
    (*transpose_aux t []*)
(*****************************************************************************)
