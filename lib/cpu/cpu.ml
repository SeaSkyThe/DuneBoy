open Stdint
open Instructions

type cpu_state =
  { registers : Registers.t
  ; mutable pc : uint16 (* Program counter *)
  }

let create ~registers ~pc = { registers; pc }

let execute cpu_state (inst : Instructions.instruction) =
  let set_flags = Registers.set_flags cpu_state.registers in
  let read : type a. a Instructions.arg -> a =
    fun arg ->
    match arg with
    | R r -> Registers.read_r cpu_state.registers r
    | RR rr -> Registers.read_rr cpu_state.registers rr
    | _ -> failwith "Not implemented"
  in
  let write : type a. a Instructions.arg -> a -> unit = fun reg value ->
      match reg with
      | R r -> ignore(Registers.write_r cpu_state.registers r value)
      | RR rr -> ignore(Registers.write_rr cpu_state.registers rr value)
      | _ -> failwith "Not implemented"
  in
  match inst with
  | ADD8 (arg1, arg2) ->
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint8.to_int val1 + Uint8.to_int val2 in
    set_flags
        ~z:(result = 0)
        ~h:(((Uint8.to_int val1 land 0x0F) + (Uint8.to_int val2 land 0x0F)) land 0x10 <> 0)
        ~n:false
        ~c:(result > 0xFF) ();
    result
  | ADD16 (arg1, arg2) -> 
    let val1 = read arg1 in
    let val2 = read arg2 in
    let result = Uint16.to_int val1 + Uint16.to_int val2 in
    set_flags
        ~z:false
        ~h:(((Uint16.to_int val1 land 0x0F) + (Uint16.to_int val2 land 0x0F)) land 0x10 <> 0)
        ~n:false
        ~c:(result > 0xFFFF) ();
    result
;;
