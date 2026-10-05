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
  }

let ievt_of_buf buffer = {
    tv_sec  = Bytes.get_int64_le buffer 0;
    tv_usec = Bytes.get_int64_le buffer 8;
    etype   = Bytes.get_int16_le buffer 16;
    code    = Bytes.get_int16_le buffer 18;
    value   = Bytes.get_int32_le buffer 20;
  }

(* read data from stream *)
let rec read_stream channel =
  let buffer = Bytes.create 24
  in let _ = input channel buffer 0 24
     in let evt = ievt_of_buf buffer
        in Printf.printf "tv_sec: %Ld | tv_usec: %Ld | etype: %d | code: %d | value: %ld\n"
              evt.tv_sec
              evt.tv_usec
              evt.etype
              evt.code
              evt.value;
           read_stream channel

let _ = read_stream ir_channel;;
