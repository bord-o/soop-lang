let hellobf = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."

let t2 = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
let t3 = "+[>[<-[]>+[>+++>[+++++++++++>][>]-[<]>-]]++++++++++<]>>>>>>----.<<+++.<-..+++.<-.>>>.<<.+++.------.>-.<<+.<."



let wk = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

type tape = {
    p: int; (* the instruction pointer *)
    t: Bytes.t; (* the tape of constant time indexable bytes *)
}

type token = 
    | INC_PTR 
    | DEC_PTR 
    | INC_BYTE 
    | DEC_BTYE 
    | GET_BYTE 
    | STORE_BYTE 
    | JUMP_FWD 
    | JUMP_BACK
    | NOP

type instruction = 
    | Inc_ptr 
    | Dec_ptr 
    | Inc_byte 
    | Dec_byte
    | Get_byte
    | Store_byte 
    | Loop of instruction list

let rec string_of_char_list chrs =
    match chrs with
    | [] -> ""
    | x::xs -> String.make 1 x ^ (string_of_char_list xs)

let char_list_of_string str = 
    let rec aux str =
        match str with
        | "" -> []
        | x -> x.[(String.length x) - 1] :: aux(String.sub x 0 ((String.length x) - 1))
    in aux str |> List.rev


let rec soopify = function
    | [] -> []
    | x::xs -> (match x with
        | '>' -> [Some("Soop")] @ soopify xs
        | '<' -> [Some("Sooop")] @ soopify xs
        | '+' -> [Some("Soooop")] @ soopify xs
        | '-' -> [Some("Sooooop")] @ soopify xs
        | '.' -> [Some("Soooooop")] @ soopify xs
        | ',' -> [Some("Sooooooop")] @ soopify xs
        | '[' -> [Some("Soooooooop")] @ soopify xs
        | ']' -> [Some("Sooooooooop")] @ soopify xs
        | _ -> [None] @ soopify xs)
    
let rec unsoop = function
    | [] -> []
    | x::xs -> (match x with
        | "Soop" -> Some('>') :: unsoop xs
        | "Sooop" -> Some('<') :: unsoop xs
        | "Soooop" -> Some('+') :: unsoop xs
        | "Sooooop" -> Some('-') :: unsoop xs
        | "Soooooop" -> Some('.') :: unsoop xs
        | "Sooooooop" -> Some(',') :: unsoop xs
        | "Soooooooop" -> Some('[') :: unsoop xs
        | "Sooooooooop" -> Some(']') :: unsoop xs
        | _ -> None :: unsoop xs)

let soop_of_bf bfpg =
    let soops = bfpg |> char_list_of_string |> soopify |> List.map (Option.value ~default:"")
    in
    List.fold_right (^) (List.map (fun soop -> soop ^" ") soops) ""
let bf_of_soop soop = 
    soop |> String.split_on_char ' ' |> unsoop |> List.map (Option.value ~default:'\x00') |> List.filter (fun x -> x <> '\000') |> string_of_char_list



let get_byte tape = print_char @@ (Bytes.get_uint8 tape.t tape.p |> Char.chr)

let inc_ptr tape = 
    { tape with p = (tape.p + 1) }

let dec_ptr tape = 
    { tape with p = (tape.p - 1) }

let store_byte tape = 
    let inchar = read_line () in
    let stored_char = 
        match inchar with
        | "" -> '\x00' 
        | str ->  str.[0]
    in
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p (Char.code stored_char);
    { tape with t = new_bytes }


let inc_byte tape = 
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p ((Bytes.get_uint8 tape.t tape.p) + 1);
    { tape with t = new_bytes }

let dec_byte tape = 
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p ((Bytes.get_uint8 tape.t tape.p) - 1);
    { tape with t = new_bytes }

let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail


let rec exec t ops = List.fold_left exec_op t ops

and exec_op t op =
    match op with
        | Inc_ptr -> inc_ptr t
        | Dec_ptr -> dec_ptr t
        | Inc_byte -> inc_byte t
        | Dec_byte -> dec_byte t
        | Get_byte -> get_byte t; t
        | Store_byte -> store_byte t
        | Loop(new_ops) -> exec_loop t new_ops

and exec_loop t ops = 
    (* loop until current data is 0 *)
    if Bytes.get t.t t.p = '\x00' then
        t
    else
        exec_loop (exec t ops) ops
    (*
    | (_, JUMP_BACK::rest) -> if depth = 0 then (acc, rest)
        else get_loop(acc, rest) (depth - 1)
    | (_, x::rest) -> if x = JUMP_FWD then get_loop(x::acc, rest) (depth + 1)
        else get_loop(x::acc, rest) depth
    | _ -> (acc, [])
    *)

let t3_mod = "+[>[<->+[>+++>[+++++++++++>][>]-[<]>-]]++++++++++<]>>>>>>----.<<+++.<-..+++.<-.>>>.<<.+++.------.>-.<<+.<."

let rec get_loop (acc, tokens) depth =
    match (acc, tokens) with

    | (_, x::rest) ->
            (match x with
            | JUMP_BACK -> if depth = 0 then (acc, rest) else get_loop(JUMP_BACK::acc, rest) (depth - 1)
            | JUMP_FWD -> get_loop(JUMP_FWD::acc, rest) (depth + 1)
            | _ -> get_loop(x::acc, rest) depth)
            
    | _ -> (acc, [])

let rec parse tokens = 
    match tokens with
    | [] -> []
    | t::rest -> match t with
        | INC_PTR -> [Inc_ptr] @ parse rest
        | DEC_PTR -> [Dec_ptr] @ parse rest
        | INC_BYTE -> [Inc_byte] @ parse rest
        | DEC_BTYE -> [Dec_byte] @ parse rest
        | GET_BYTE -> [Get_byte] @ parse rest
        | STORE_BYTE -> [Store_byte] @ parse rest
        | JUMP_FWD -> (
            let (inner, remaining) = get_loop ([], rest) 0 in
            [Loop(parse @@ List.rev inner)] @ parse remaining
        )
        (* these two shouldnt happen as nops should be filtered and all of he 
           control flow logic is handled inside of JUMP_FWD *)
        | JUMP_BACK -> failwith "shouldnt be reached ']'"
        | NOP -> failwith "nop reached"
        

let rec tokenize_list chrs =
    (* you should really filter out nops *)
    let tokenize chr = 
        (match chr with
        | '>' -> Some(INC_PTR)
        | '<' -> Some(DEC_PTR)
        | '+' -> Some(INC_BYTE)
        | '-' -> Some(DEC_BTYE)
        | '.' -> Some(GET_BYTE)
        | ',' -> Some(STORE_BYTE)
        | '[' -> Some(JUMP_FWD)
        | ']' -> Some(JUMP_BACK)
        | _ -> None)
    in 
    match chrs with
    | [] -> []
    | [x] -> [Option.value (tokenize x) ~default:NOP]
    | x::xs -> Option.value (tokenize x) ~default:NOP :: tokenize_list xs


let token_arr_of_list tl = 
    Array.init (List.length tl) (fun i-> List.nth tl i)

let char_arr_of_string str = 
    let chrs = Array.make (String.length str) '\x00' in
    (match str with
    | "" -> ()
    | x -> String.iteri (fun i c -> Array.set chrs i c) x);
    chrs

let tfunc word = print_endline word

let construct_tape ?(number_of_bytes = 1001) () =
    (* given a number of bytes, the pointer will be in the middle 
       of the bytes, allowing for backward and forward movement of pointer *)

    let new_bytes = Bytes.make number_of_bytes '\x00' in
    {
        p = if ((number_of_bytes mod 2) = 1) then ((number_of_bytes / 2) + 1) else (number_of_bytes / 2);
        t = new_bytes;
    }

let get_byte tape = print_char @@ ( (Bytes.get_uint8 tape.t tape.p) |> Char.chr)

let inc_ptr tape = 
    { tape with p = (tape.p + 1) }

let dec_ptr tape = 
    { tape with p = (tape.p - 1) }

let store_byte tape = 
    let inchar = read_line () in
    let stored_char = 
        match inchar with
        | "" -> '\x00' 
        | str ->  str.[0]
    in
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p (Char.code stored_char);
    { tape with t = new_bytes }


let inc_byte tape = 
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p ((Bytes.get_uint8 tape.t tape.p) + 1);
    { tape with t = new_bytes }

let dec_byte tape = 
    let new_bytes = Bytes.copy tape.t in
    Bytes.set_uint8 new_bytes tape.p ((Bytes.get_uint8 tape.t tape.p) - 1);
    { tape with t = new_bytes }


(* wrapper functions for utop testing *)

let run program = 
    let intape = construct_tape ~number_of_bytes: 500 () in
    let ops = program |> char_list_of_string |> tokenize_list |> parse in
    ignore @@ exec intape ops;
    ()

let run_soop soop = 
    let prog = bf_of_soop soop in
    run prog


let parse_of_string = function
    | s -> s |> char_list_of_string |> tokenize_list |> parse
