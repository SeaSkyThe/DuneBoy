open Stdint

type t

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
val read_r : t -> r -> uint8
val write_r : t -> r -> uint8 -> unit
val read_rr : t -> rr -> uint16
val write_rr : t -> rr -> uint16 -> unit
