

/* 单线程测试，用于测试Typing */


if true then new Lock<T1,T2> else new Lock<T1,T3>;
/* lock<T1,T2> : Lock<T1> */

let l1 = new Lock<T1,T2> in
let l2 = new Lock<T1,T3> in
let t1 = ref<T1,T2> 0 in
let t2 = ref<T1,T3> 0 in
if true then t1 else t2;
/* <loc #0> : Ref<T1,T2,T3> Nat */

let a = new Lock<T1,T2> in
synchronized a in !(ref<T1> 0);
/* 多加锁可以正常运行 */

let a = new Lock<T1> in
  synchronized a in 
    synchronized a in 
      !(ref<T1> 0);
/* 会死锁，嵌套取同一个锁 */

let x = new Lock<X> in
let y = new Lock<X> in
let buf1 = ref<X> 0 in
let flag = true in
  synchronized if flag then x else y in buf1 := 1;
/* 正确，因为x和y都对应锁X */


let x = new Lock<X> in
let y = new Lock<Y> in
let buf1 = ref<X> 0 in
let flag = true in
  synchronized if flag then x else y in 0;
/* 正确，因为没有访问内存，并且两个锁join依旧是锁 */


let x = new Lock<X> in
let buf1 = ref<Z> 0 in
let flag = true in
  synchronized x in !buf1;
/* 报错，因为锁Z没有被bind */

let x = new Lock<X> in
let y = new Lock<Y> in
let buf1 = ref<X> 0 in
let flag = true in
  synchronized if flag then x else y in buf1 := 1;
/* 报错，因为Lock<X>和Lock<Y>的join为Lock<empty>，因此访问buf1的时候有可能没有拿到锁X */



/* ------------------------------------------------------------ */

/*
/* 多线程测试，用于测试Evaluating */


x = new Lock<X>;
y = new Lock<Y>;

buf1 = ref<X> 0;
buf2 = ref<Y> 100;

checkParity = fix (lambda f:Nat->Bool->Bool. lambda n:Nat. lambda even:Bool.
  if iszero n then even
  else f (pred n) (if even then false else true));

isEven = lambda n:Nat. checkParity n true;

/*
targetFunc = lambda _:Unit.
  if isEven tid then
    synchronized x in
      synchronized y in
        let tmp1 = succ (!buf1) in
        let _ = (buf1 := tmp1) in
        let tmp2 = pred (!buf2) in
        let _ = (buf2 := tmp2) in
          {res1=tmp1,res2=tmp2}
  else
    synchronized x in
      synchronized y in
        let tmp1 = pred (!buf2) in
        let _ = (buf2 := tmp1) in
        let tmp2 = succ (!buf1) in
        let _ = (buf1 := tmp2) in
          {res1=tmp2,res2=tmp1};
/* 这个函数正常工作 */
*/

targetFunc = lambda _:Unit.
  if isEven tid then
    synchronized x in 
      let tmp1 = succ (!buf1) in
      let _ = (buf1 := tmp1) in
      synchronized y in
        let tmp2 = pred (!buf2) in
        let _ = (buf2 := tmp2) in
          {res1=tmp1,res2=tmp2}
  else
    synchronized y in
      let tmp1 = pred (!buf2) in
      let _ = (buf2 := tmp1) in
      synchronized x in
        let tmp2 = succ (!buf1) in
        let _ = (buf1 := tmp2) in
          {res1=tmp2,res2=tmp1};
/* 这个函数会死锁 */


/*
t = synchronized x in fork { targetFunc unit };
/* 报错，持有锁的情况下fork */
*/

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

synchronized x in !buf1;
synchronized y in !buf2;

*/