(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

type permissions
val emptyPermissions : permissions

type locks
val emptyLocks : locks
val addLock : string -> locks -> locks


val typecheck : (context * permissions * locks) -> term -> ty
val subtype : context -> ty -> ty -> bool
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
val shiftstore : int -> unit
val eval : (context * locks) -> term -> term
val evalbinding : (context * locks) -> binding -> binding

module StringMap : Map.S with type key = string
