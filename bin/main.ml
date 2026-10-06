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
(* 1023 is an invalid state *)
let pos_map =
  [ { x_pos = 1023; y_pos = 1023 };
    { x_pos = 1023; y_pos = 1023 };
    { x_pos = 1023; y_pos = 1023 };
    { x_pos = 1023; y_pos = 1023 } ];;

(* add position changes from event to map *)
let pos_of_ievt ievt =
  let v = Int32.to_int ievt.value
  in match ievt.code with
     | 16 -> let b = List.nth pos_map 0 in b.x_pos <- v; Some ()
     | 17 -> let b = List.nth pos_map 0 in b.y_pos <- v; Some ()
     | 18 -> let b = List.nth pos_map 1 in b.x_pos <- v; Some ()
     | 19 -> let b = List.nth pos_map 1 in b.y_pos <- v; Some ()
     | 20 -> let b = List.nth pos_map 2 in b.x_pos <- v; Some ()
     | 21 -> let b = List.nth pos_map 2 in b.y_pos <- v; Some ()
     | 22 -> let b = List.nth pos_map 3 in b.x_pos <- v; Some ()
     | 23 -> let b = List.nth pos_map 3 in b.y_pos <- v; Some ()
     | _ -> None

(* open a window for visualization *)
let () =
  open_graph " 1024x768";
  set_window_title "ptr_of_wii - test board";;

(* print coords of blob *)
let print_point pmap n =
  let x = 1024 - (List.nth pmap n).x_pos
  in let y = 768 - (List.nth pmap n).y_pos
     in fill_circle x y 10;
        moveto x (y + 15);
        draw_string (Printf.sprintf "(%d, %d)" x y);;

(* find mid point btw two blobs *)
let mid_point bloba blobb =
  let ax = 1024 - bloba.x_pos
  in let ay = 768 - bloba.y_pos
     in let bx = 1024 - blobb.x_pos
        in let by = 768 - blobb.y_pos
           in ((ax + bx) / 2, (ay + by) / 2);;

(* print mid point btw two blobs *)
let print_mid_point mp =
  let (x, y) = mp
     in fill_circle x y 10;
        moveto x (y + 15);
        draw_string (Printf.sprintf "(%d, %d)" x y);;

(* print all blob data to terminal *)
let print_raw_point () =
  Printf.printf "B1 {x: %d, y: %d} B2 {x: %d, y: %d} B3 {x: %d, y: %d} B4 {x: %d, y: %d}\n"
                         (List.nth pos_map 0).x_pos
                         (List.nth pos_map 0).y_pos
                         (List.nth pos_map 1).x_pos
                         (List.nth pos_map 1).y_pos
                         (List.nth pos_map 2).x_pos
                         (List.nth pos_map 2).y_pos
                         (List.nth pos_map 3).x_pos
                         (List.nth pos_map 3).y_pos;;

(* holds inbound blobs *)
let valid_blobs = ref [];;

(* fetch all inbound blobs *)
let get_valid_blobs () =
  valid_blobs := [];
  let f b =
    if b.x_pos <> 1023 || b.y_pos <> 1023 then
      valid_blobs := b :: !valid_blobs
  in List.iter f pos_map;;

(* calc distance between two blobs *)
let calc_dist bloba blobb =
  let sx = (blobb.x_pos - bloba.x_pos)
  in let sy = (blobb.y_pos - bloba.y_pos)
     in sqrt (float_of_int ((sx * sx) + (sy * sy)));;

(* store states of cursor, 2 main blobs and distance *)
let g_dx = ref 0;;
let g_dy = ref 0;;
let g_cursor = ref (0, 0);;
let g_blob_a = ref { x_pos = 1023; y_pos = 1023 };;
let g_blob_b = ref { x_pos = 1023; y_pos = 1023 };;

(* update visualization frame *)
let update_frame b0 b1 = g_blob_a := { x_pos = b0.x_pos; y_pos = b0.y_pos };
                         g_blob_b := { x_pos = b1.x_pos; y_pos = b1.y_pos };
                         g_dx := b1.x_pos - b0.x_pos;
                         g_dy := b1.y_pos - b0.y_pos;
                         let p_cursor = (mid_point b0 b1)
                         in g_cursor := p_cursor;
                            print_mid_point p_cursor;;
                            (* set_color blue;
                            print_point pos_map 1;
                            set_color green;
                            print_point pos_map 2;
                            set_color yellow;
                            print_point pos_map 3;; *)


(* read data from stream *)
let rec read_stream channel =
  let buffer = Bytes.create 24
  in let _ = input channel buffer 0 24
     in let evt = ievt_of_buf buffer
        in match pos_of_ievt evt with
           | Some _ -> read_stream channel (* update frame when code 0 SYN i.e., None case *)
           | None ->
              clear_graph();
              get_valid_blobs ();
              (* print_raw_point ();
              set_color red;
              print_point pos_map 0; *)
              set_color black;
              begin
              if List.length !valid_blobs >= 2 then
                let b0 = (List.nth !valid_blobs 0)
                in let b1 = (List.nth !valid_blobs 1)
                   in let (left, right) = if b0.x_pos <= b1.x_pos then (b0, b1) else (b1, b0)
                      in update_frame left right
              else if List.length !valid_blobs = 1 then
                let vb = (List.nth !valid_blobs 0)
                in let vb_to_ba = calc_dist !g_blob_a vb (* left prob *)
                   in let vb_to_bb = calc_dist !g_blob_b vb (* right prob *)
                      in let ib =
                           begin
                           if vb_to_ba < vb_to_bb then (* close to left *)
                             { x_pos = vb.x_pos + !g_dx; y_pos = vb.y_pos + !g_dy }
                           else (* close to right *)
                             { x_pos = vb.x_pos - !g_dx; y_pos = vb.y_pos - !g_dy }
                           end
                         in begin
                             if vb_to_ba < vb_to_bb then
                               update_frame vb ib
                             else
                               update_frame ib vb
                           end
              else
                print_mid_point !g_cursor
              end;
              read_stream channel


let _ = read_stream ir_channel;;
