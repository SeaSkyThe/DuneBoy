(* CPU/timer/GPU share the same hardware clock, so they run in a sync state *)
(* The emulator is a sequencial execution loop, so to emulate that, we will use a "catch up"  method *)
(* This consists in letting the CPU execute one instruction and track how many cycles were consumed by it 
   then we just sync everything advancing the timer/GPU by the number of cycles consumed by the CPU *) 

(* let run_instructions t =  *)
(*     let mcycles = Cpu.run_instruction t.cpu in *)
(*     Timer.run t.timer ~mcycles:mcycles; *)
(*     Gpu.run t.gpu ~mcycles:mcycles *)

