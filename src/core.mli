(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : context -> term -> ty
val subtype : context -> ty -> ty -> bool
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
val shiftstore : int -> unit
val eval : context -> term -> term
val evalbinding : context -> binding -> binding
