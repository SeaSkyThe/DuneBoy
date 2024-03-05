open Stdint
(* Interfaces to read/write 8-bit data - to be able to connect the gameboy modules to the BUS *)
module type S = sig
    type t
    (* reads 8-bit data from address addr *)
    val read_byte : t -> uint16 -> uint8
    (* writes 8-bit data to address addr *)
    val write_byte: t -> addr:uint16 -> data:uint8 -> unit
    (* returns true if it accepts reads/writes from addr and returns false if it can not *)
    val accepts : t -> uint16 -> bool
end
