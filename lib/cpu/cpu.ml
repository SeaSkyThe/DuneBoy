type t = {
    registers: Registers.t;
    bus      : Bus.t;
    mutable pc: uint16; (* Program counter *)
}

let create ~bus ~registers ~pc = {
    bus;
    registers;
    pc;
}
