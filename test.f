x = Lock X;

/* which will behave like
  X = Lock;
  x : X;
  x = new_lock;
*/


buf1 = ref 0 locked by X;


synchronized x in !buf1;

/*

checkParity = fix (lambda f:Nat->Bool->Bool. lambda n:Nat. lambda even:Bool.
  if iszero n then even
  else f (pred n) (if even then false else true));

isEven = lambda n:Nat. checkParity n true;

targetFunc = lambda _:Unit. 
  if isEven tid then 
    let tmp1 = succ (!buf1) in
    let _ = (buf1 := tmp1) in
    let tmp2 = pred (!buf2) in
    let _ = (buf2 := tmp2) in 
    {res1=tmp1,res2=tmp2}
  else
    let tmp1 = pred (!buf2) in
    let _ = (buf2 := tmp1) in 
    let tmp2 = succ (!buf1) in
    let _ = (buf1 := tmp2) in
    {res1=tmp2,res2=tmp1};

t1 = fork { targetFunc unit };
t2 = fork { targetFunc unit };
t3 = fork { targetFunc unit };
t4 = fork { targetFunc unit };
t5 = fork { targetFunc unit };
t6 = fork { targetFunc unit };
t7 = fork { targetFunc unit };
t8 = fork { targetFunc unit };
t9 = fork { targetFunc unit };
t10 = fork { targetFunc unit };

wait t1;
wait t2;
wait t3;
wait t4;
wait t5;
wait t6;
wait t7;
wait t8;
wait t9;
wait t10;

!buf1;
!buf2;

*/