open Soop_lang.Soop

(* let () = tfunc "Hello, Soop!";; *)

let _ = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."


let _ = "+++++++++++++++++++++++++++++++++>><<."
let _ = "+[>[<-[]>+[>+++>[+++++++++++>][>]-[<]>-]]++++++++++<]>>>>>>----.<<+++.<-..+++.<-.>>>.<<.+++.------.>-.<<+.<."


let _ = "--<-<<+[+[<+>--->->->-<<<]>]<<--.<++++++.<<-..<<.<+.>>.>>.<<<.+++.>>.>>-.<<<+."

let _ = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

let sooptest = "----[---->+<]>---..>-[--->+<]>--.----..+.[----->++<]>.++++++[->++<]>.-----------.+++++++++++++.-------.---------.." |> soop_of_bf

let () =
    run_soop sooptest;

