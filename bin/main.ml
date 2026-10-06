open Graphics

(* setup IR event path *)
let ir_event = 19;;
let ir_stream_path = "/dev/input/event" ^ string_of_int ir_event;;
let () = print_endline ir_stream_path;;

(* check if event stream exists *)
if not (Sys.file_exists ir_stream_path) then
  failwith "IR event stream not found";;

(* create channel for stream *)
let ir_channel =  open_in_bin ir_stream_path;;


(* create input_event record *)
type ievt = {
    tv_sec  : int64;
    tv_usec : int64;
    etype   : int;
    code    : int;
    value   : int32;
  };;

let ievt_of_buf buffer = {
    tv_sec  = Bytes.get_int64_le buffer 0;
    tv_usec = Bytes.get_int64_le buffer 8;
    etype   = Bytes.get_int16_le buffer 16;
    code    = Bytes.get_int16_le buffer 18;
    value   = Bytes.get_int32_le buffer 20;
  };;


(*  IR blob map
 *  x (0-1023); y (0-767)
 *
 *  code   maps to
 * ------------------
 *  16     ABS_HAT0X
 *  17     ABS_HAT0Y
 *  18     ABS_HAT1X
 *  19     ABS_HAT1Y
 *  20     ABS_HAT2X
 *  21     ABS_HAT2Y
 *  22     ABS_HAT3X
 *  23     ABS_HAT3Y
 *)

type blob = {
    mutable x_pos : int;
    mutable y_pos : int;
  };;

(* [{ABS_HAT0}, {ABS_HAT1}, {ABS_HAT2}, {ABS_HAT3}] *)
let pos_map =
  [ { x_pos = -1; y_pos = -1 };
    { x_pos = -1; y_pos = -1 };
    { x_pos = -1; y_pos = -1 };
    { x_pos = -1; y_pos = -1 } ];;

let pos_of_ievt ievt =
  match ievt.code with
  | 16 -> let b = List.nth pos_map 0 in b.x_pos <- Int32.to_int ievt.value; Some ()
  | 17 -> let b = List.nth pos_map 0 in b.y_pos <- Int32.to_int ievt.value; Some ()
  | 18 -> let b = List.nth pos_map 1 in b.x_pos <- Int32.to_int ievt.value; Some ()
  | 19 -> let b = List.nth pos_map 1 in b.y_pos <- Int32.to_int ievt.value; Some ()
  | 20 -> let b = List.nth pos_map 2 in b.x_pos <- Int32.to_int ievt.value; Some ()
  | 21 -> let b = List.nth pos_map 2 in b.y_pos <- Int32.to_int ievt.value; Some ()
  | 22 -> let b = List.nth pos_map 3 in b.x_pos <- Int32.to_int ievt.value; Some ()
  | 23 -> let b = List.nth pos_map 3 in b.y_pos <- Int32.to_int ievt.value; Some ()
  | _ -> None;;

let () =
  open_graph " 1024x768";
  set_window_title "ptr_of_wii - test board";;

let print_point pmap n =
  let x = 1024 - (List.nth pmap n).x_pos
  in let y = 768 - (List.nth pmap n).y_pos
     in fill_circle x y 10;
        moveto x (y + 15);
        draw_string (Printf.sprintf "(%d, %d)" x y);;

let mid_point bloba blobb =
  let ax = 1024 - bloba.x_pos
  in let ay = 768 - bloba.y_pos
     in let bx = 1024 - blobb.x_pos
        in let by = 768 - blobb.y_pos
           in ((ax + bx) / 2, (ay + by) / 2);;

let print_mid_point mp =
  let (x, y) = mp
     in fill_circle x y 10;
        moveto x (y + 15);
        draw_string (Printf.sprintf "(%d, %d)" x y);;

(* read data from stream *)
let rec read_stream channel =
  let buffer = Bytes.create 24
  in let _ = input channel buffer 0 24
     in let evt = ievt_of_buf buffer
        in match pos_of_ievt evt with
           | None -> read_stream channel
           | Some _ ->
              clear_graph();
              set_color red;
              (* print_point pos_map 0; *)
              print_mid_point (mid_point (List.nth pos_map 0) (List.nth pos_map 1));
              (* set_color blue;
              print_point pos_map 1;
              set_color green;
              print_point pos_map 2;
              set_color yellow;
              print_point pos_map 3; *)
              read_stream channel


let _ = read_stream ir_channel;;
