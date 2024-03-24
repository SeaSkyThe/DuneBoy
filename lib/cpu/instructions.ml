open Stdint

(* Possible argument types for CPU instructions *)
type _ arg =
  | Immediate8 : uint8 arg -> uint16 arg
  | Immediate16 : uint8 arg -> uint16 arg
  | Direct8 : uint16 -> uint8 arg
  | Direct16 : uint16 -> uint16 arg
  | R : Registers.r -> uint8 arg
  | RR : Registers.rr -> uint16 arg
  | RR_indirect : Registers.rr -> uint8 arg
  | FF00_offset : uint8 -> uint8 arg
  | FF00_C : uint8 arg
  | HL_inc : uint8 arg
  | HL_dec : uint8 arg
  | SP : uint16 arg
  | SP_offset : uint8 -> uint16 arg

type instruction =
  | LD8 of Stdint.uint8 arg * Stdint.uint8 arg
  | LD16 of Stdint.uint16 arg * Stdint.uint16 arg
  | ADD8 of Stdint.uint8 arg * Stdint.uint8 arg
  | ADD16 of Stdint.uint16 arg * Stdint.uint16 arg
  | ADDSP of int8
