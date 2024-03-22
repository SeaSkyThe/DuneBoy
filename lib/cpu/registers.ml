open Stdint

(* Im re-declaring the register types because I cant make the compiler get them from mli *)
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

type t =
  { mutable a : uint8
  ; mutable b : uint8
  ; mutable c : uint8
  ; mutable d : uint8
  ; mutable e : uint8
  ; mutable f : uint8
  ; mutable h : uint8
  ; mutable l : uint8
  }

(*
   Receives the current state of registers and the wanted registers
   and returns the current value in the register
*)
let read_r t reg =
  match reg with
  | A -> t.a
  | B -> t.b
  | C -> t.c
  | D -> t.d
  | E -> t.e
  | F -> t.f
  | H -> t.h
  | L -> t.l
;;

let write_r t reg value =
  match reg with
  | A -> { t with a = value }
  | B -> { t with b = value }
  | C -> { t with c = value }
  | D -> { t with d = value }
  | E -> { t with e = value }
  | F -> { t with f = value }
  | H -> { t with h = value }
  | L -> { t with l = value }
;;

(*https://en.wikipedia.org//wiki/Bitwise_operation*)
let read_rr t rr =
  match rr with
  | AF ->
    Uint16.logor
      (Uint16.shift_left (Uint8.to_uint16 t.a) 8)
      (Uint8.to_uint16 t.f)
  | BC ->
    Uint16.logor
      (Uint16.shift_left (Uint8.to_uint16 t.b) 8)
      (Uint8.to_uint16 t.c)
  | DE ->
    Uint16.logor
      (Uint16.shift_left (Uint8.to_uint16 t.d) 8)
      (Uint8.to_uint16 t.e)
  | HL ->
    Uint16.logor
      (Uint16.shift_left (Uint8.to_uint16 t.h) 8)
      (Uint8.to_uint16 t.l)
;;

let write_rr t rr value =
  match rr with
  | AF ->
    { t with
      a = Uint8.of_int (Uint16.to_int value land 0xFF)
    ; f = Uint8.of_int (Uint16.to_int (Uint16.shift_right value 8) land 0xFF)
    }
  | BC ->
    { t with
      b = Uint8.of_int (Uint16.to_int value land 0xFF)
    ; c = Uint8.of_int (Uint16.to_int (Uint16.shift_right value 8) land 0xFF)
    }
  | DE ->
    { t with
      d = Uint8.of_int (Uint16.to_int value land 0xFF)
    ; e = Uint8.of_int (Uint16.to_int (Uint16.shift_right value 8) land 0xFF)
    }
  | HL ->
    { t with
      a = Uint8.of_int (Uint16.to_int value land 0xFF)
    ; f = Uint8.of_int (Uint16.to_int (Uint16.shift_right value 8) land 0xFF)
    }
;;

(* FLAG REGISTER F *)

type flag =
  | Zero
  | Subtract
  | Half_carry
  | Carry

let mask_0b00010000 = 0b00010000
let mask_0b00100000 = 0b00100000
let mask_0b01000000 = 0b01000000
let mask_0b10000000 = 0b10000000
let mask_0b11110000 = 0b11110000

let read_flag t flag =
  match flag with
  | Zero -> Uint8.to_int t.f land mask_0b10000000 <> 0
  | Subtract -> Uint8.to_int t.f land mask_0b01000000 <> 0
  | Half_carry -> Uint8.to_int t.f land mask_0b00100000 <> 0
  | Carry -> Uint8.to_int t.f land mask_0b00010000 <> 0
;;

let set_flag t flag =
  t.f
    <- (match flag with
        | Zero -> Uint8.of_int (Uint8.to_int t.f lor mask_0b10000000)
        | Subtract -> Uint8.of_int (Uint8.to_int t.f lor mask_0b01000000)
        | Half_carry -> Uint8.of_int (Uint8.to_int t.f lor mask_0b00100000)
        | Carry -> Uint8.of_int (Uint8.to_int t.f lor mask_0b00010000))
;;

let set_flags t ?(c = false) ?(h = false) ?(n = false) ?(z = false) () =
  t.f
    <- Uint8.of_int
         ((if c then mask_0b00010000 else 0)
          lor (if h then mask_0b00100000 else 0)
          lor (if n then mask_0b01000000 else 0)
          lor if z then mask_0b10000000 else 0)
;;

let unset_flag t flag =
  t.f
    <- (match flag with
        | Zero -> Uint8.of_int (Uint8.to_int t.f land lnot mask_0b10000000)
        | Subtract -> Uint8.of_int (Uint8.to_int t.f land lnot mask_0b01000000)
        | Half_carry -> Uint8.of_int (Uint8.to_int t.f land lnot mask_0b00100000)
        | Carry -> Uint8.of_int (Uint8.to_int t.f land lnot mask_0b00010000))
;;

let clear_flags t =
  t.f <- Uint8.of_int (Uint8.to_int t.f land lnot mask_0b11110000)
;;
