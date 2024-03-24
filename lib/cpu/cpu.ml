open Stdint
open Instructions

type cpu_state =
  { registers : Registers.t
  ; mutable pc : uint16 (* Program counter *)
  ; mutable sp : uint16 (* Stack Pointer *)
  }

let create ~registers ~pc ~sp = { registers; pc; sp }

type next_pc =
  | Next
  | Jump of uint16

let execute cpu_state (inst : Instructions.instruction) =
  let set_flags = Registers.set_flags cpu_state.registers in
  let read : type a. a Instructions.arg -> a =
    fun arg ->
    match arg with
    | R r -> Registers.read_r cpu_state.registers r
    | RR rr -> Registers.read_rr cpu_state.registers rr
    | SP -> cpu_state.sp
    | _ -> failwith "Not implemented"
  in
  let write : type a. a Instructions.arg -> a -> unit =
    fun reg value ->
    match reg with
    | R r -> ignore (Registers.write_r cpu_state.registers r value)
    | RR rr -> ignore (Registers.write_rr cpu_state.registers rr value)
    | SP -> cpu_state.sp <- value
    | _ -> failwith "Not implemented"
  in
  match inst with
  | LD8 (arg1, arg2) -> write arg1 (read arg2)
  | LD16 (arg1, arg2) -> write arg1 (read arg2)
  | ADD8 (arg1, arg2) ->
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint8.to_int val1 + Uint8.to_int val2 in
    set_flags
      ~z:(result = 0)
      ~h:
        (((Uint8.to_int val1 land 0x0F) + (Uint8.to_int val2 land 0x0F))
         land 0x10
         <> 0)
      ~n:false
      ~c:(result > 0xFF)
      ();
    write arg1 (Uint8.of_int result)
  | ADD16 (arg1, arg2) ->
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint16.to_int val1 + Uint16.to_int val2 in
    set_flags
      ~z:false
      ~h:
        (((Uint16.to_int val1 land 0x0F) + (Uint16.to_int val2 land 0x0F))
         land 0x10
         <> 0)
      ~n:false
      ~c:(result > 0xFFFF)
      ();
    write arg1 (Uint16.of_int result)
  | ADDSP arg2 ->
    let val1 = Uint16.to_int (read SP) in
    let val2 = Int8.to_int arg2 in
    set_flags
      ~z:false
      ~h:((val1 land 0xF) + (val2 land 0xF) > 0xF)
      ~n:false
      ~c:((val1 land 0xFF) + (val2 land 0xFF) > 0xFF)
      ();
    write SP (Uint16.of_int (val1 + val2))
  | ADC (arg1, arg2) ->
    let c =
      if Registers.(read_flag cpu_state.registers Carry)
      then Uint8.one
      else Uint8.zero
    in
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint8.to_int val1 + Uint8.to_int val2 + Uint8.to_int c in
    set_flags
      ~z:(result = 0)
      ~h:
        (((Uint8.to_int val1 land 0xF) + (Uint8.to_int val2 land 0xF)) land 0x10
         <> 0)
      ~n:false
      ~c:(result > 0xFF)
      ();
    write arg1 (Uint8.of_int result)
  | SUB (arg1, arg2) ->
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint8.to_int val1 - Uint8.to_int val2 in
    set_flags
      ~z:(result = 0)
      ~h:(Uint8.to_int val1 land 0x0F < Uint8.to_int val2 land 0x0F)
      ~n:true
      ~c:(Uint8.to_int val1 < Uint8.to_int val2)
      ();
    write arg1 (Uint8.of_int result)
  | SBC (arg1, arg2) ->
    let c =
      if Registers.(read_flag cpu_state.registers Carry)
      then Uint8.one
      else Uint8.zero
    in
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint8.to_int val1 - (Uint8.to_int val2 + Uint8.to_int c) in
    set_flags
      ~z:(result = 0)
      ~h:(Uint8.to_int val1 land 0x0F < Uint8.to_int val2 land 0x0F + Uint8.to_int c)
      ~n:true
      ~c:((Uint8.to_int val1) < Uint8.to_int val2 + Uint8.to_int c)
      ();
    write arg1 (Uint8.of_int result)
;;
