type cpu_state

val create : registers:Registers.t -> pc: Stdint.uint16 -> sp: Stdint.uint16 -> cpu_state
val execute: cpu_state -> Instructions.instruction -> int (*return the # of cycles consumed*) 
