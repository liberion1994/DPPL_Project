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

let checkbinding fi (ctx,locks) b = match b with
    NameBind -> (NameBind,locks)
  | VarBind(tyT) -> (VarBind(tyT),locks)
  | TmAbbBind(t,None) -> 
      let tyT = typecheck (ctx,emptyPermissions,locks,".") t in (match tyT with 
          TyLock(ls) -> (TmAbbBind(t, Some(tyT)), foldlockset addLock ls locks)
        | _ -> (TmAbbBind(t, Some(tyT)),locks))
  | TmAbbBind(t,Some(tyT)) ->
      let tyT' = typecheck (ctx,emptyPermissions,locks,".") t in
      if subtype ctx tyT' tyT then (match tyT with 
          TyLock(ls) -> (TmAbbBind(t, Some(tyT)), foldlockset addLock ls locks)
        | _ -> (TmAbbBind(t, Some(tyT)),locks))
      else error fi "Type of binding does not match declared type"
  | TyVarBind -> (TyVarBind,locks)
  | TyAbbBind(tyT) -> (TyAbbBind(tyT),locks)

let prbindingty (ctx,locks) b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TmAbbBind(t, tyT_opt) -> pr ": ";
      (match tyT_opt with
          None -> printty ctx (typecheck (ctx,emptyPermissions,locks,".") t)
        | Some(tyT) -> printty ctx tyT)
  | TyAbbBind(tyT) -> pr ":: *"

let rec process_command (ctx,locks) cmd = match cmd with
  | Eval(fi,t) -> 
      let tyT = typecheck (ctx,emptyPermissions,locks,".") t in
      let t' = eval (ctx,locks) t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      (ctx,locks)
  | Bind(fi,x,bind) -> 
      let bind,locks' = checkbinding fi (ctx,locks) bind in
      let bind' = evalbinding (ctx,locks) bind in
      pr x; pr " "; prbindingty (ctx,locks') bind'; force_newline();
      let ctx' = addbinding ctx x bind' in
      let _ = shiftstore 1 in (ctx',locks')


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
