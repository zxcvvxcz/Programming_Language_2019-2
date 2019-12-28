type exp = X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec calculate : exp -> float = fun e ->
    match e with
    | X -> raise FreeVariable
    | INT i -> float_of_int i
    | REAL r -> r
    | ADD (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
                    else calculate e1 +. calculate e2
    | SUB (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
                    else calculate e1 -. calculate e2
    | MUL (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
                    else calculate e1 *. calculate e2 
    | DIV (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
                    else calculate e1 /. calculate e2
    | SIGMA (e1, e2, e3) -> 
        let rec sigm : int * exp -> float = fun (x1, x2) -> (* x2에 X가 있으면 정수 x1을 대입하는 함수 *)
            match x2 with
                | X -> float_of_int x1
                | INT i -> float_of_int i
                | REAL r -> r
                | ADD (s1, s2) -> sigm (x1, s1) +. sigm (x1, s2)
                | SUB (s1, s2) -> sigm (x1, s1) -. sigm (x1, s2)
                | MUL (s1, s2) -> sigm (x1, s1) *. sigm (x1, s2)
                | DIV (s1, s2) -> sigm (x1, s1) /. sigm (x1, s2)
                | SIGMA (s1, s2, s3) -> calculate (SIGMA (REAL (sigm (x1, s1)), REAL (sigm (x1, s2)), s3))
                | INTEGRAL (s1, s2, s3) -> calculate (INTEGRAL (REAL (sigm (x1, s1)), REAL (sigm (x1, s2)), s3))
        in
        if calculate e1 > calculate e2 then 0.
        else sigm ((int_of_float (calculate e1)), e3) +. (calculate (SIGMA (ADD(e1, INT 1), e2, e3)))
    
    | INTEGRAL (e1, e2, e3) ->
        let rec integral : float * exp -> float = fun (x1, x2) -> match x2 with
            | X -> x1
            | INT i -> float_of_int i
            | REAL r -> r
            | ADD (s1, s2) -> integral (x1, s1) +. integral (x1, s2)
            | SUB (s1, s2) -> integral (x1, s1) -. integral (x1, s2)
            | MUL (s1, s2) -> integral (x1, s1) *. integral (x1, s2)
            | DIV (s1, s2) -> integral (x1, s1) /. integral (x1, s2)
            | SIGMA (s1, s2, s3) -> calculate (SIGMA (REAL (integral (x1, s1)), REAL (integral (x1, s2)), s3))
            | INTEGRAL (s1, s2, s3) -> calculate (INTEGRAL (REAL (integral (x1, s1)), REAL (integral (x1, s2)), s3))
        in
        if calculate e1 -. calculate e2 < 0.1 && calculate e1 -. calculate e2 > -0.1 
            then 0.
        else if calculate e1 < calculate e2
            then integral ((calculate e1), e3) *. 0.1 +. calculate (INTEGRAL (ADD(e1, REAL 0.1), e2, e3))
        else -. integral ((calculate e1), e3) *. 0.1 -. calculate (INTEGRAL (SUB(e1, REAL 0.1), e2, e3))
       
(*let x = calculate X
let i = (calculate (INT 1))
let r = (calculate (REAL 1.))
let a0 = calculate (ADD (X, X))
let a1 = calculate (ADD (INT 1, X))
let a2 = calculate (ADD (X, REAL 1.))
let a3 = calculate (ADD (INT 1, INT 1))
let a4 = calculate (ADD (REAL 1., INT 5))
let a5 = calculate (ADD (INT (-1), REAL 5.))
let s0 = calculate (SUB (X, X))
let s1 = calculate (SUB (INT 1, X))
let s2 = calculate (SUB (X, REAL 1.))
let s3 = calculate (SUB (INT 1, INT 1))
let s4 = calculate (SUB (REAL 1., INT 5))
let s5 = calculate (SUB (INT (-1), REAL 5.))

let m0 = calculate (MUL (X, X))
let m1 = calculate (MUL (INT 1, X))
let m2 = calculate (MUL (X, REAL 1.))
let m3 = calculate (MUL (INT 1, INT 1))
let m4 = calculate (MUL (REAL 1., INT 5))
let m5 = calculate (MUL (INT (-1), REAL 5.))
let m6 = calculate (MUL (INT (-1), REAL 0.))
let d0 = calculate (DIV (X, X))
let d1 = calculate (DIV (INT 1, X))
let d2 = calculate (DIV (X, REAL 1.))
let d3 = calculate (DIV (INT 1, INT 1))
let d4 = calculate (DIV (REAL 1., INT 5))
let d5 = calculate (DIV (INT (-1), REAL 5.))
let d6 = calculate (DIV (REAL (0.), INT 3))
let sig0 = calculate (SIGMA (X, X, X))
let sig1 = calculate (SIGMA (INT 0, INT 10, X))
let sig2 = calculate (SIGMA (INT 0, INT 10, INT 1))
let sig3 = calculate (SIGMA (INT 0, REAL 10., SUB(MUL(X, X), REAL 1.)))
let sig4 = calculate (SIGMA (REAL 0.1, INT 10, SUB(MUL(X, X), REAL 2.)))
let sig5 = calculate (SIGMA (REAL 1., REAL 10., SIGMA(INT 1, X, X)))
let sig6 = calculate (SIGMA (REAL 1., REAL 10., INTEGRAL(INT 1, X, X)))
let itg0 = calculate (INTEGRAL (X, X, X))
let itg1 = calculate (INTEGRAL (INT 0, INT 10, X))
let itg2 = calculate (INTEGRAL (INT 0, INT 10, INT 1))
let itg3 = calculate (INTEGRAL (INT 0, REAL 10., SUB(MUL(X, X), REAL 1.)))
let itg4 = calculate (INTEGRAL (REAL 0.1, INT 10, SUB(MUL(X, X), REAL 2.)))
let itg5 = calculate (INTEGRAL (REAL 1., REAL 10., SIGMA(INT 1, X, X)))
let itg6 = calculate (INTEGRAL (REAL 1., REAL 10., INTEGRAL(INT 1, X, X)))
*)