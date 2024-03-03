open Stdint
(* Interface that provide 16-bit read/write in addition to 8-bity read/write *)
module type S = sig
    type t
    include Addressable_intf.S with type t := t
    (* 16-bit reads/writes *)
    val read_word : t -> uint16 -> uint16
    val write_word: t -> addr:uint16 -> data:uint16 -> unit
end
