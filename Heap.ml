open Core

module type HEAP =
sig
    type elt
    type heap

    val empty : int -> heap
    
    val is_empty : heap -> bool

    val insert : heap -> elt -> heap

    val extract_min : heap -> (elt * heap)
end

module type COMPARABLE =
sig
    type t
    val compare : t -> t -> ordering
    val empty_val : t
end

module BinaryHeap(C: COMPARABLE) : (HEAP with type elt=C.t) =
struct
    type elt = C.t
    type index = int
    (* The heap stores its first empty index/number of elements + 1. *)
    type heap = index * elt array

    (* 0th position of array ignored for ease of arithmetic. *)
    let empty n = (1, Array.make (n + 1) C.empty_val)

    let is_empty hp = let (i,_) = hp in i = 1

    let rec percolate_up hp idx e =
        let (i, a) = hp in
        if idx = 1 then 
            let _ = a.(idx) <- e in (idx, (i, a))
        else
            match C.compare a.(idx/2) e with
            | Less | Equal -> let _ = a.(idx) <- e in (idx, (i, a))
            | Greater ->
                    let _ = a.(idx) <- a.(idx/2) in percolate_up (i, a) (idx/2) e

    let rec percolate_down hp idx e =
        let (i, a) = hp in
        let child = 2 * idx in
        if child >= i then
            let _ = a.(idx) <- e in (i, a)
        else if child = (i - 1) then
            match C.compare a.(child) e with
            | Less | Equal -> let _ = a.(idx) <- a.(child); a.(child) <- e in (i, a)
            | Greater -> let _ = a.(idx) <- e in (i, a)
        else
            match C.compare a.(child) a.(child+1) with
            | Less ->
                    (match C.compare a.(child) e with
                    | Less ->
                            let _ = a.(idx) <- a.(child) in
                            percolate_down (i, a) child e
                    | Equal | Greater -> let _ = a.(idx) <- e in (i, a))
            | Equal | Greater ->
                    (match C.compare a.(child+1) e with
                    | Less ->
                            let _ = a.(idx) <- a.(child+1) in
                            percolate_down (i, a) (child+1) e
                    | Equal | Greater -> let _ = a.(idx) <- e in (i, a))

    let insert hp e =
        let (i, a) = hp in
        let _, h = percolate_up ((i + 1), a) i e in h

    let extract_min hp =
        let (i, a) = hp in
        let min = a.(1) in
        (min, (percolate_down ((i-1), a) 1 a.(i-1)))
end

module MaxIntComparable : (COMPARABLE with type t=int) =
struct
    type t = int

    let compare n m =
        let diff = n.wt - m.wt in
        (* Reversed to get max instead of min. *)
        if diff < 0 then Greater
        else if diff > 0 then Less
        else Equal

    let empty_val = 0
end

module MaxIntBH = BinaryHeap(MaxIntComparable) ;;
