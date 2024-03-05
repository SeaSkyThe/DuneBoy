type t

val create : bus:Bus.t -> registers:Registers.t -> t
val run_instruction : t -> int (*return the # of cycles consumed*) 
