/* Examples for testing */

/*

 lambda x:Bot. x;
 lambda x:Bot. x x; 

 
lambda x:<a:Bool,b:Bool>. x;


lambda x:Top. x;
 (lambda x:Top. x) (lambda x:Top. x);
(lambda x:Top->Top. x) (lambda x:Top. x);


(lambda r:{x:Top->Top}. r.x r.x) 
  {x=lambda z:Top.z, y=lambda z:Top.z}; 


"hello";

unit;

lambda x:A. x;

let x=true in x;

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


if true then {x=true,y=false,a=false} else {y=false,x={},b=false};

timesfloat 2.0 3.14159;




lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

T = Nat->Nat;
lambda f:T. lambda x:Nat. f (f x);


/* 18.11.1 1 */
CounterRep = {x:Ref Nat};
SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit};
setCounterClass = 
  lambda r:CounterRep.
    lambda self:Unit->SetCounter.
      lambda _:Unit.
        {get = lambda _:Unit. !(r.x),
          set = lambda i:Nat. r.x := i,
          inc = lambda _:Unit. (self unit).set (succ ((self unit).get unit))};
newSetCounter = lambda _:Unit. let r = {x = ref 1} in fix(setCounterClass r) unit;

InstrCounterRep = {x:Ref Nat, a:Ref Nat};
InstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat};
instrCounterClass = 
  lambda r:InstrCounterRep.
    lambda self:Unit->InstrCounter.
      lambda _:Unit.
        let super = setCounterClass r self unit in
          {get = lambda _:Unit. (r.a := succ(!(r.a)); super.get unit),
            set = lambda i:Nat. (r.a := succ(!(r.a)); super.set i),
            inc = super.inc,
            accesses = lambda _:Unit. !(r.a)};
newInstrCounter = lambda _:Unit. let r = {x = ref 1, a = ref 0} in fix(instrCounterClass r) unit;

counter1 = newInstrCounter unit;
counter1.get unit;
counter1.inc unit;
counter1.get unit;
counter1.accesses unit;
counter1.set 11;
counter1.get unit;
counter1.accesses unit;

/* 18.11.1 2 */
ResetCounterRep = {x:Ref Nat, a:Ref Nat};
ResetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, reset:Unit->Unit};
resetCounterClass = 
  lambda r:ResetCounterRep.
    lambda self:Unit->ResetCounter.
      lambda _:Unit.
        let super = instrCounterClass r self unit in
          {get = super.get,
            set = super.set,
            inc = super.inc,
            accesses = super.accesses,
            reset = lambda _:Unit. r.x := 1};
newResetCounter = lambda _:Unit. let r = {x = ref 1, a = ref 0} in fix(resetCounterClass r) unit;

counter2 = newResetCounter unit;
counter2.inc unit;
counter2.get unit;
counter2.reset unit;
counter2.get unit;

/* 18.11.1 3 */
BackupCounterRep = {x:Ref Nat, a:Ref Nat, b:Ref Nat};
BackupCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, reset:Unit->Unit, backup:Unit->Unit};
backupCounterClass = 
  lambda r:BackupCounterRep.
    lambda self:Unit->BackupCounter.
      lambda _:Unit.
        let super = resetCounterClass r self unit in
          {get = super.get,
            set = super.set,
            inc = super.inc,
            accesses = super.accesses,
            reset = lambda _:Unit. r.x := !r.b,
            backup = lambda _:Unit. r.b := !r.x};
newBackupCounter = lambda _:Unit. let r = {x = ref 1, a = ref 0, b = ref 0} in fix(backupCounterClass r) unit;

counter3 = newBackupCounter unit;
counter3.inc unit;
counter3.get unit;
counter3.backup unit;
counter3.inc unit;
counter3.get unit;
counter3.reset unit;
counter3.get unit;

/* 18.6.1 */
DecCounterRep = {x:Ref Nat, a:Ref Nat};
DecCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, reset:Unit->Unit, dec:Unit->Unit};
decCounterClass = 
  lambda r:DecCounterRep.
    lambda self:Unit->DecCounter.
      lambda _:Unit.
        let super = resetCounterClass r self unit in
          {get = super.get,
            set = super.set,
            inc = super.inc,
            accesses = super.accesses,
            reset = super.reset,
            dec = lambda _:Unit. r.x := pred (!r.x)};
newDecCounter = lambda _:Unit. let r = {x = ref 1, a = ref 0} in fix(decCounterClass r) unit;

counter4 = newDecCounter unit;
counter4.inc unit;
counter4.get unit;
counter4.reset unit;
counter4.get unit;
counter4.inc unit;
counter4.get unit;
counter4.dec unit;
counter4.get unit;

/* 18.7.1 */
Backup2CounterRep = {x:Ref Nat, a:Ref Nat, b:Ref Nat, b2:Ref Nat};
Backup2Counter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, 
                  reset:Unit->Unit, backup:Unit->Unit, reset2:Unit->Unit, backup2:Unit->Unit};
backup2CounterClass = 
  lambda r:Backup2CounterRep.
    lambda self:Unit->Backup2Counter.
      lambda _:Unit.
        let super = backupCounterClass r self unit in
          {get = super.get,
            set = super.set,
            inc = super.inc,
            accesses = super.accesses,
            reset = super.reset,
            backup = super.backup,
            reset2 = lambda _:Unit. r.x := !r.b2,
            backup2 = lambda _:Unit. r.b2 := !r.x};
newBackup2Counter = lambda _:Unit. let r = {x = ref 1, a = ref 0, b = ref 0, b2 = ref 0} in fix(backup2CounterClass r) unit;

counter5 = newBackup2Counter unit;
counter5.inc unit;
counter5.get unit;
counter5.backup unit;
counter5.inc unit;
counter5.get unit;
counter5.reset unit;
counter5.get unit;

counter5.inc unit;
counter5.backup2 unit;
counter5.get unit;
counter5.inc unit;
counter5.inc unit;
counter5.get unit;
counter5.reset2 unit;
counter5.get unit;

*/

buf = ref 0;

func = 
  lambda _:Unit. 
    let tmp = succ (!buf) in
      let _ = (buf := tmp) in
        tmp;
      
t = fork func unit;
wait t;