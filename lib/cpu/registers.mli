type t =
  { mutable a : Stdint.uint8
  ; mutable b : Stdint.uint8
  ; mutable c : Stdint.uint8
  ; mutable d : Stdint.uint8
  ; mutable e : Stdint.uint8
  ; mutable f : Stdint.uint8
  ; mutable h : Stdint.uint8
  ; mutable l : Stdint.uint8
  }

(* Identifiers of the 8-bit registers *)
type r =
  | A
  | B
  | C
  | D
  | E
  | F
  | H
  | L

(* Identifiers of the 16-bit registers *)
type rr =
  | AF
  | BC
  | DE
  | HL

(* read/write functions for the above registers *)
val read_r : t -> r -> Stdint.uint8
val write_r : t -> r -> Stdint.uint8 -> t
val read_rr : t -> rr -> Stdint.uint16
val write_rr : t -> rr -> Stdint.uint16 -> t

(* Flag Registers *)

type flag =
  | Zero (* name : z *)
  | Subtract (* name : n *)
  | Half_carry (* name: h *)
  | Carry (* name: c *)

val read_flag : t -> flag -> bool
val set_flag : t -> flag -> unit
val set_flags : t -> ?c:bool -> ?h:bool -> ?n:bool -> ?z:bool -> unit -> unit
val unset_flag : t -> flag -> unit
val clear_flags: t -> unit
