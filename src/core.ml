open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmTag(_,l,t1,_) -> isval ctx t1
  | TmString _  -> true
  | TmUnit(_)  -> true
  | TmLoc(_,_) -> true
  | TmFloat _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | TmThread(_,_,_) -> true
  | TmLock(_,_) -> true
  | _ -> false

type store = term list  
(* each store read/write will delay the current thread for some time, 
which is to make the behavior of unlocked memory access unpredictable 
and therefore makes the race condition occur *)
let store = ref []
let extendstore v = 
    let ret = List.length !store in 
    let _ = store := List.append !store [v] in ret
let lookuploc l = let _ = Thread.delay 0.00001 in List.nth !store l
let updatestore n v =
  let _ = Thread.delay 0.00001 in
  let rec f s = match s with 
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
    store := f (n,!store)
let shiftstore i = 
    store := List.map (fun t -> termShift i t) !store 



module StringSet = Set.Make(String)
type permissions = StringSet.t
let emptyPermissions = StringSet.empty
let addPermission = StringSet.add
let isPermissionsEmpty = StringSet.is_empty
let existPermission = StringSet.mem



module StringMap = Map.Make (String)
type locks = Mutex.t StringMap.t
let emptyLocks : locks = StringMap.empty
let addLock name locks = StringMap.add name (Mutex.create ()) locks
let existLock = StringMap.mem
let getLock x locks =
  let mtx = StringMap.find x locks in let _ =  Mutex.lock mtx in mtx
let releaseLock = Mutex.unlock

exception NoRuleApplies

let rec eval1 (ctx,locks) t = match t with
    TmApp(fi,TmAbs(_,x,tyT11,t12,ls),v2) when isval ctx v2 ->
      (termSubstTop v2 t12,locks)
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2',locks' = eval1 (ctx,locks) t2 in
      (TmApp(fi, v1, t2'),locks')
  | TmApp(fi,t1,t2) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmApp(fi, t1', t2),locks')
  | TmIf(_,TmTrue(_),t2,t3) ->
      (t2,locks)
  | TmIf(_,TmFalse(_),t2,t3) ->
      (t3,locks)
  | TmIf(fi,t1,t2,t3) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmIf(fi, t1', t2, t3),locks')
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      let locks' = match v1 with 
          TmLock(fi,ls) -> foldlockset addLock ls locks
        | _ -> locks in
      (termSubstTop v1 t2,locks')
  | TmLet(fi,x,t1,t2) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmLet(fi, x, t1', t2),locks')
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12,ls) -> (termSubstTop t t12,locks)
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1',locks' = eval1 (ctx,locks) t1
      in (TmFix(fi,t1'),locks')
  | TmRecord(fi,fields) ->
      let rec evalafield (l,locks) = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest',locks' = evalafield (rest,locks) in
          ((l,vi)::rest',locks')
      | (l,ti)::rest -> 
          let ti',locks' = eval1 (ctx,locks) ti in
          ((l, ti')::rest,locks')
      in let fields',locks' = evalafield (fields,locks) in
      (TmRecord(fi, fields'),locks')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try (List.assoc l fields,locks)
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmProj(fi, t1', l),locks')
  | TmTag(fi,l,t1,tyT) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmTag(fi, l, t1',tyT),locks')
  | TmCase(fi,TmTag(_,li,v11,_),branches) when isval ctx v11->
      (try 
         let (x,body) = List.assoc li branches in
         (termSubstTop v11 body,locks)
       with Not_found -> raise NoRuleApplies)
  | TmCase(fi,t1,branches) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmCase(fi, t1', branches),locks')
  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
      (v1,locks)
  | TmAscribe(fi,t1,tyT) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmAscribe(fi,t1',tyT),locks')
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> (t,locks) 
        | _ -> raise NoRuleApplies)
  | TmRef(fi,t1,t2) ->
      if not (isval ctx t1) then
        let t1',locks' = eval1 (ctx,locks) t1
        in (TmRef(fi,t1',t2),locks')
      else
        let l = extendstore t1 in
        (TmLoc(dummyinfo,l),locks)
  | TmDeref(fi,t1) ->
      if not (isval ctx t1) then
        let t1',locks' = eval1 (ctx,locks) t1
        in (TmDeref(fi,t1'),locks')
      else (match t1 with
            TmLoc(_,l) -> (lookuploc l,locks)
          | _ -> raise NoRuleApplies)
  | TmAssign(fi,t1,t2) ->
      if not (isval ctx t1) then
        let t1',locks' = eval1 (ctx,locks) t1
        in (TmAssign(fi,t1',t2),locks')
      else if not (isval ctx t2) then
        let t2',locks' = eval1 (ctx,locks) t2
        in (TmAssign(fi,t1,t2'),locks')
      else (match t1 with
            TmLoc(_,l) -> let _ = updatestore l t2 in (TmUnit(dummyinfo),locks)
          | _ -> raise NoRuleApplies)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      (TmFloat(fi, f1 *. f2),locks)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2',locks' = eval1 (ctx,locks) t2 in
      (TmTimesfloat(fi,t1,t2'),locks')
  | TmTimesfloat(fi,t1,t2) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmTimesfloat(fi,t1',t2),locks')
  | TmSucc(fi,t1) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmSucc(fi, t1'),locks')
  | TmPred(_,TmZero(_)) ->
      (TmZero(dummyinfo),locks)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      (nv1,locks)
  | TmPred(fi,t1) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmPred(fi, t1'),locks')
  | TmIsZero(_,TmZero(_)) ->
      (TmTrue(dummyinfo),locks)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      (TmFalse(dummyinfo),locks)
  | TmIsZero(fi,t1) ->
      let t1',locks' = eval1 (ctx,locks) t1 in
      (TmIsZero(fi, t1'),locks')
  | TmFork(fi,t1) ->
      let channel = Event.new_channel () in
      let th = Thread.create (fun _ ->
        try let t1' = eval (ctx,locks) t1 in
          (* should communicate to set store here *)
          let e = Event.send channel t1' in 
          let _ = Event.sync e in ()
        with NoRuleApplies -> 
          let _ = print_string "No rules apply in forked thread" in
          let e = Event.send channel t1 in 
          let _ = Event.sync e in ()) () in
      (TmThread(fi,th,channel),locks)
  | TmWait(_,TmThread(fi,thread,channel)) ->
      let e = Event.receive channel in
      (Event.sync e,locks)
  | TmWait(fi,t1) ->
      let t1',locks' = eval1 (ctx,locks) t1 in (TmWait(fi,t1'),locks')
  | TmTid(fi) ->
      let rec f n = match n with
              0 -> TmZero(fi)
            | n -> TmSucc(fi, f (n-1))
          in (f (Thread.id (Thread.self ())),locks)
  | TmSync(fi,v1,t2) when isval ctx v1 ->
      (match v1 with 
          TmLock(_,ls) ->
            let mtxs = maplockset (fun name -> getLock name locks) ls in
            let ret = eval (ctx,locks) t2 in 
            let _ = List.iter releaseLock mtxs in (ret,locks)
        | _ -> raise NoRuleApplies)
  | TmSync(fi,t1,t2) ->
      let t1',locks' = eval1 (ctx,locks) t1 in (TmSync(fi,t1',t2),locks')
  | _ -> 
      raise NoRuleApplies
and eval (ctx,locks) t =
  try let t',locks' = eval1 (ctx,locks) t
      in eval (ctx,locks') t'
  with NoRuleApplies -> t

(* ------------------------   SUBTYPING  ------------------------ *)

let evalbinding (ctx,locks) b = match b with
    TmAbbBind(t,tyT) ->
      let t' = eval (ctx,locks) t in 
      TmAbbBind(t',tyT)
  | bind -> bind

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyTop,TyTop) -> true
  | (TyBot,TyBot) -> true
  | (TyArr(tyS1,tyS2,l1),TyArr(tyT1,tyT2,l2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2) && locksetequal l1 l2
  | (TyString,TyString) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyFloat,TyFloat) -> true
  | (TyUnit,TyUnit) -> true
  | (TyRef(tyT1,l1),TyRef(tyT2,l2)) -> locksetequal l1 l2 && tyeqv ctx tyT1 tyT2
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TyRecord(fields1),TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyVariant(fields1),TyVariant(fields2)) ->
       (List.length fields1 = List.length fields2)
       && List.for_all2
            (fun (li1,tyTi1) (li2,tyTi2) ->
               (li1=li2) && tyeqv ctx tyTi1 tyTi2)
            fields1 fields2
  | (TyThread(tyT1),TyThread(tyT2)) -> tyeqv ctx tyT1 tyT2
  | (TyLock(a), TyLock(b)) -> locksetequal a b
  | _ -> false

let rec subtype ctx tyS tyT =
   tyeqv ctx tyS tyT ||
   let tyS = simplifyty ctx tyS in
   let tyT = simplifyty ctx tyT in
   match (tyS,tyT) with
     (_,TyTop) -> 
       true
   | (TyBot,_) -> 
       true
   | (TyArr(tyS1,tyS2,l1),TyArr(tyT1,tyT2,l2)) ->
       (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2) && sublockset l2 l1
   | (TyRecord(fS), TyRecord(fT)) ->
       List.for_all
         (fun (li,tyTi) -> 
            try let tySi = List.assoc li fS in
                subtype ctx tySi tyTi
            with Not_found -> false)
         fT
   | (TyVariant(fS), TyVariant(fT)) ->
       List.for_all
         (fun (li,tySi) -> 
            try let tyTi = List.assoc li fT in
                subtype ctx tySi tyTi
            with Not_found -> false)
         fS
   | (TyRef(tyT1,l1),TyRef(tyT2,l2)) ->
       subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 && sublockset l2 l1
   | (TyLock(l1),TyLock(l2)) -> 
       sublockset l1 l2
   | (_,_) -> 
       false

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT else 
  if subtype ctx tyT tyS then tyS else
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let commonLabels = 
        List.find_all (fun l -> List.mem l labelsT) labelsS in
      let commonFields = 
        List.map (fun li -> 
                    let tySi = List.assoc li fS in
                    let tyTi = List.assoc li fT in
                    (li, join ctx tySi tyTi))
                 commonLabels in
      TyRecord(commonFields)
  | (TyArr(tyS1,tyS2,l1),TyArr(tyT1,tyT2,l2)) ->
      TyArr(meet ctx  tyS1 tyT1, join ctx tyS2 tyT2,unionlockset l1 l2)
  | (TyRef(tyT1,l1),TyRef(tyT2,l2)) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(tyT1,unionlockset l1 l2)
        else TyTop
  | (TyLock(a), TyLock(b)) ->
      TyLock(interlockset a b)
  | _ -> 
      TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS else 
  if subtype ctx tyT tyS then tyT else 
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let allLabels = 
        List.append
          labelsS 
          (List.find_all 
            (fun l -> not (List.mem l labelsS)) labelsT) in
      let allFields = 
        List.map (fun li -> 
                    if List.mem li allLabels then
                      let tySi = List.assoc li fS in
                      let tyTi = List.assoc li fT in
                      (li, meet ctx tySi tyTi)
                    else if List.mem li labelsS then
                      (li, List.assoc li fS)
                    else
                      (li, List.assoc li fT))
                 allLabels in
      TyRecord(allFields)
  | (TyArr(tyS1,tyS2,l1),TyArr(tyT1,tyT2,l2)) ->
      TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2,interlockset l1 l2)
  | (TyRef(tyT1,l1),TyRef(tyT2,l2)) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(tyT1,interlockset l1 l2)
        else TyBot
  | (TyLock(a), TyLock(b)) ->
      TyLock(unionlockset a b)
  | _ -> 
      TyBot

(* ------------------------   TYPING  ------------------------ *)

let rec typecheck (ctx,permissions,locks) t =
  match t with
    TmInert(fi,tyT) ->
      tyT
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2,ls) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let permissions' = foldlockset addPermission ls permissions in
      let tyT2 = typecheck (ctx',permissions',locks) t2 in
      TyArr(tyT1, typeShift (-1) tyT2,ls)
  | TmApp(fi,t1,t2) ->
  (*todo typing application的时候需要知道t1需要的外部锁，然后check有没有在手里*)
      let tyT1 = typecheck (ctx,permissions,locks) t1 in
      let tyT2 = typecheck (ctx,permissions,locks) t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12,ls) ->
            if subtype ctx tyT2 tyT11 then 
              let unheld = foldlockset (fun l r -> 
                if existPermission l permissions then r
                else (r ^ " " ^ l)) ls "" in
              if unheld = "" then tyT12
              else error fi ("Application without holding the lock:" ^ unheld)
            else error fi "parameter type mismatch" 
        | TyBot -> TyBot
        | _ -> error fi "arrow type expected")
  | TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
      if subtype ctx (typecheck (ctx,permissions,locks) t1) TyBool then
        join ctx (typecheck (ctx,permissions,locks) t2) (typecheck (ctx,permissions,locks) t3)
      else error fi "guard of conditional not a boolean"
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typecheck (ctx,permissions,locks) t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in
     let locks' = (match tyT1 with 
          TyLock(ls) -> foldlockset addLock ls locks
        | _ -> locks) in
     typeShift (-1) (typecheck (ctx',permissions,locks') t2)
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li,ti) -> (li, typecheck (ctx,permissions,locks) ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typecheck (ctx,permissions,locks) t1) with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | TyBot -> TyBot
        | _ -> error fi "Expected record type")
  | TmCase(fi, t, cases) ->
      (match simplifyty ctx (typecheck (ctx,permissions,locks) t) with
         TyVariant(fieldtys) ->
           List.iter
             (fun (li,(xi,ti)) ->
                try let _ = List.assoc li fieldtys in ()
                with Not_found -> error fi ("label "^li^" not in type"))
             cases;
           let casetypes =
             List.map (fun (li,(xi,ti)) ->
                         let tyTi =
                           try List.assoc li fieldtys
                           with Not_found ->
                             error fi ("label "^li^" not found") in
                         let ctx' = addbinding ctx xi (VarBind(tyTi)) in
                         typeShift (-1) (typecheck (ctx',permissions,locks) ti))
                      cases in
           List.fold_left (join ctx) TyBot casetypes
        | TyBot -> TyBot
        | _ -> error fi "Expected variant type")
  | TmFix(fi, t1) ->
      let tyT1 = typecheck (ctx,permissions,locks) t1 in
      (match simplifyty ctx tyT1 with
           TyArr(tyT11,tyT12,ls) ->
             if subtype ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")
  | TmTag(fi, li, ti, tyT) ->
      (match simplifyty ctx tyT with
          TyVariant(fieldtys) ->
            (try
               let tyTiExpected = List.assoc li fieldtys in
               let tyTi = typecheck (ctx,permissions,locks) ti in
               if subtype ctx tyTi tyTiExpected
                 then tyT
                 else error fi "field does not have expected type"
             with Not_found -> error fi ("label "^li^" not found"))
        | _ -> error fi "Annotation is not a variant type")
  | TmAscribe(fi,t1,tyT) ->
     if subtype ctx (typecheck (ctx,permissions,locks) t1) tyT then
       tyT
     else
       error fi "body of as-term does not have the expected type"
  | TmString _ -> TyString
  | TmUnit(fi) -> TyUnit
  | TmRef(fi,t1,l1) ->
      let unbound = foldlockset (fun l r -> 
        if existLock l locks then r 
        else (r ^ " " ^ l)) l1 "" in
      if unbound = "" then TyRef(typecheck (ctx,permissions,locks) t1,l1) 
      else error fi ("ref locked by unbound lock:" ^ unbound)
  | TmLoc(fi,l) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmDeref(fi,t1) ->
      (match simplifyty ctx (typecheck (ctx,permissions,locks) t1) with
          TyRef(tyT1,l1) -> 
            let unheld = foldlockset (fun l r -> 
                if existPermission l permissions then r
                else (r ^ " " ^ l)) l1 "" in
            if unheld = "" then tyT1
            else error fi ("Access to memory without holding the lock:" ^ unheld)
        | TyBot -> TyBot
        | _ -> error fi "argument of ! is not a Ref")
  | TmAssign(fi,t1,t2) ->
      (*todo add permission constraints*)
      (match simplifyty ctx (typecheck (ctx,permissions,locks) t1) with
          TyRef(tyT1,l1) ->
            if subtype ctx (typecheck (ctx,permissions,locks) t2) tyT1 then
              let unheld = foldlockset (fun l r -> 
                if existPermission l permissions then r
                else (r ^ " " ^ l)) l1 "" in
              if unheld = "" then tyT1
              else error fi ("Access to memory without holding the lock:" ^ unheld)
            else
              error fi "arguments of := are incompatible"
        | TyBot -> let _ = typecheck (ctx,permissions,locks) t2 in TyBot
        | _ -> error fi "argument of ! is not a Ref")
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi,t1,t2) ->
      if subtype ctx (typecheck (ctx,permissions,locks) t1) TyFloat
      && subtype ctx (typecheck (ctx,permissions,locks) t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if subtype ctx (typecheck (ctx,permissions,locks) t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if subtype ctx (typecheck (ctx,permissions,locks) t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if subtype ctx (typecheck (ctx,permissions,locks) t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmFork(fi,t1) ->
      if isPermissionsEmpty permissions then
      let tyT1 = typecheck (ctx,permissions,locks) t1 in TyThread(tyT1)
      else error fi "fork when there are un-released lock(s)"
  | TmWait(fi,t1) ->
      let tyT1 = typecheck (ctx,permissions,locks) t1 in
      (match tyT1 with
          TyThread(tyT11) -> tyT11
        | _ -> error fi "thread type expected")
  | TmThread(fi,_,_) ->
      error fi "no thread term should be explicit declared"
  | TmTid(fi) -> TyNat
  | TmSync(fi,t1,t2) ->
      let tyT1 = typecheck (ctx,permissions,locks) t1 in 
        (match tyT1 with 
            TyLock(ls) -> 
              let alreadyheld = foldlockset (fun l r -> 
                if existPermission l permissions then (r ^ " " ^ l)
                else r) ls "" in
              if alreadyheld = "" then 
                let permissions' = foldlockset addPermission ls permissions in typecheck (ctx,permissions',locks) t2
              else
                error fi ("try to acquire lock already held (which may cause dead lock):" ^ alreadyheld)
          | _ -> error fi "sync with none-lock type")
  | TmLock(fi,t1) -> (TyLock(t1))

