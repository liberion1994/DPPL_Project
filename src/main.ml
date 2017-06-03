(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | VarBind(tyT) -> VarBind(tyT)
  | TmAbbBind(t,None) -> TmAbbBind(t, Some(typecheck ctx emptyLocks t))
  | TmAbbBind(t,Some(tyT)) ->
     let tyT' = typecheck ctx emptyLocks t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT))
     else error fi "Type of binding does not match declared type"
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(tyT)

let prbindingty ctx b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TmAbbBind(t, tyT_opt) -> pr ": ";
     (match tyT_opt with
         None -> printty ctx (typecheck ctx emptyLocks t)
       | Some(tyT) -> printty ctx tyT)
  | TyAbbBind(tyT) -> pr ":: *"

let rec process_command (ctx,locks) cmd = match cmd with
  | Eval(fi,t) -> 
        let tyT = typecheck ctx emptyLocks t in
        let t' = eval ctx t in
        printtm_ATerm true ctx t'; 
        print_break 1 2;
        pr ": ";
        printty ctx tyT;
        force_newline();
        (ctx,locks)
  | Bind(fi,x,bind) -> 
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      pr x; pr " "; prbindingty ctx bind'; force_newline();
      let ctx' = addbinding ctx x bind' in
      let _ = shiftstore 1 in (ctx',locks)


let process_file f (ctx,locks) =
  alreadyImported := f :: !alreadyImported;
  let cmds,_ = parseFile f ctx in
  let g (ctx,locks) c =  
    open_hvbox 0;
    let results = process_command (ctx,locks) c in
    print_flush();
    results
  in
    List.fold_left g (ctx,locks) cmds

let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile (emptycontext, emptyLocks) in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res


(*
open Thread
open Array

let buf = ref 0

let func () = let threads = Array.make 10 () in 
    Array.map (fun _ -> Thread.create (fun _ -> 
        let tmp = !buf in
            Thread.delay 0.000001;
            buf := tmp + 1
    ) ()) threads

    
let () = let _ = Array.map (fun a -> 
    join a; 
    print_int (Thread.id a); 
    print_string "\t"; 
    print_int !buf; 
    print_newline ()
) (func ()) in 
print_string "final result\t";
print_int !buf; 
*)
(*
let c = Event.new_channel ();;
let f () =
   let ids = string_of_int (Thread.id (Thread.self ())) 
   in print_string ("-------- before  -------" ^ ids) ; print_newline() ;
      let e = Event.receive c 
      in print_string ("-------- during  -------" ^ ids) ; print_newline() ;
         let v = Event.sync e 
         in print_string (v ^ " " ^ ids ^ " ") ; 
            print_string ("-------- after  -------" ^ ids) ; print_newline() ;;

let g () =
   let ids = string_of_int (Thread.id (Thread.self ())) 
   in print_string ("Start of " ^ ids ^ "\n");
      let e2 = Event.send c "hello" 
      in Event.sync e2 ;
         print_string ("End of " ^ ids) ;
         print_newline () ;;

let t1,t2,t3 = Thread.create f (), Thread.create f (), Thread.create g ();;
Thread.delay 1.0;;
*)
