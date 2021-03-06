(* File I/O functionality borrowed from Jane Street's Core version of the 
 * standard OCaml library (this file lives at
 * https://github.com/janestreet/core_kernel/blob/master/src/in_channel.ml)
 * since Core provides efficient versions of many standard functions and is
 * not available on the nice.fas.harvard.edu machines. *)
(*****************************************************************************)
(*module String = Core_string*)
open Core

type t = in_channel

let seek = Pervasives.LargeFile.seek_in
let pos = Pervasives.LargeFile.pos_in
let length = Pervasives.LargeFile.in_channel_length

let stdin = Pervasives.stdin

let create ?(binary = true) file =
  let flags = [Open_rdonly] in
  let flags = if binary then Open_binary :: flags else flags in
  open_in_gen flags 0o000 file
;;

let close = Pervasives.close_in

let with_file ?binary file ~f = protectx (create ?binary file) ~f ~finally:close

let may_eof f = try Some (f ()) with End_of_file -> None

let input t ~buf ~pos ~len = Pervasives.input t buf pos len
let really_input t ~buf ~pos ~len =
  may_eof (fun () -> Pervasives.really_input t buf pos len)
let input_byte t = may_eof (fun () -> Pervasives.input_byte t)
let input_char t = may_eof (fun () -> Pervasives.input_char t)
let input_binary_int t = may_eof (fun () -> Pervasives.input_binary_int t)
let unsafe_input_value t = may_eof (fun () -> Pervasives.input_value t)

let set_binary_mode = Pervasives.set_binary_mode_in

let input_all t =
  (* We use 65536 because that is the size of OCaml's IO buffers. *)
  let buf_size = 65536 in
  let buf = String.create buf_size in
  let buffer = Buffer.create buf_size in
  let rec loop () =
    let len = input t ~buf ~pos:0 ~len:(String.length buf) in
    if len > 0 then begin
      Buffer.add_substring buffer buf 0 len;
      loop ();
    end
  in
  loop ();
  Buffer.contents buffer;
;;

let input_line ?(fix_win_eol = true) t =
  match may_eof (fun () -> Pervasives.input_line t) with
  | None -> None
  | Some line ->
      let remove_trailing_return =
        fix_win_eol
        && String.length line > 0
        && line.[(String.length line) - 1] = '\r'
      in
      if remove_trailing_return then
        Some (String.sub line 0 ((String.length line) - 1))
      else
        Some line
;;

let fold_lines ?fix_win_eol t ~init ~f =
  let rec loop ac =
    match input_line ?fix_win_eol t with
    | None -> ac
    | Some line -> loop (f ac line)
  in
  loop init
;;

let input_lines ?fix_win_eol t =
  List.rev
    (fold_lines ?fix_win_eol t ~init:[] ~f:(fun lines line -> line :: lines))
;;

let iter_lines ?fix_win_eol t ~f =
  fold_lines ?fix_win_eol t ~init:() ~f:(fun () line -> f line)
;;

let read_lines fname = with_file fname ~f:input_lines

let read_all fname = with_file fname ~f:input_all
