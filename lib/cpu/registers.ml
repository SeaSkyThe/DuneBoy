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
