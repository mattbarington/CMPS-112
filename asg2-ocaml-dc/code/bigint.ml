(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str =
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec cmp' list1 list2 stat = match (list1, list2, stat) with
        | [], [], _         -> stat
        | list1, [], _      -> 1
        | [], list2, _      -> -1
        | car1::cdr1, car2::cdr2, _ ->
          if car1 = car2
          then cmp' cdr1 cdr2 stat
          else cmp' cdr1 cdr2 (car1 - car2)

    (* let rec canon number = match (number) with
        | [] -> []
        | car::cdr -> if car = 0 then canon cdr else car::cdr

    let canonicalize number =  reverse (canon (reverse number)) *)

    let canonicalize number =
        let rec canon list = match list with
            | []    ->  []
            | [0]   ->  []
            | car::cdr ->
                let cdr' = canon cdr in match car, cdr' with
                  | 0, [] -> []
                  | car, cdr' ->  car::cdr'
        in canon number

    let cmp (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        cmp' value1 value2 0

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | [], [], carry   -> []
        | list1, [], 0   -> list1
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = (car1 - car2 - carry)
          in if diff < 0
            then (diff + 10) :: sub' cdr1 cdr2 1
            else diff :: sub' cdr1 cdr2 0

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (neg1, neg2) with
        | Pos, Neg      ->
            if (cmp' value1 value2 0) >= 0
            then Bigint (Pos, canonicalize (sub' value1 value2 0))
            else Bigint (Neg, canonicalize (sub' value2 value1 0))
        | Neg, Pos      ->
            if (cmp' value1 value2 0) <= 0
            then Bigint (Pos, canonicalize (sub' value2 value1 0))
            else Bigint (Neg, canonicalize (sub' value1 value2 0))
        | Pos, Pos -> Bigint (Pos, add' value1 value2 0)
        | Neg, Neg -> Bigint (Neg, add' value1 value2 0)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (neg1, neg2) with
        | Pos, Neg    -> Bigint (Pos, add' value1 value2 0)
        | Neg, Pos    -> Bigint (Neg, add' value1 value2 0)
        | Pos, Pos    ->
            if (cmp' value1 value2 0) >= 0
            then Bigint (Pos, canonicalize (sub' value1 value2 0))
            else Bigint (Neg, canonicalize (sub' value2 value1 0))
        | Neg, Neg    ->
            if (cmp' value2 value1 0) >= 0
            then Bigint (Pos, canonicalize (sub' value2 value1 0))
            else Bigint (Neg, canonicalize (sub' value1 value2 0))

    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') =
        if (cmp' powerof2 multiplier 0) > 0
        then multiplier, []
        else let remainder, product =
            mul' (multiplier, double powerof2, double multiplicand')
            in
            if (cmp' remainder powerof2 0) < 0
            then remainder, product
            else canonicalize (sub' remainder powerof2 0),
              (add' multiplicand' product 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if cmp' value1 value2 0 > 0
        then let _,product = mul' (value2, [1], value1) in
            if neg1 = neg2
            then Bigint (Pos, product)
            else Bigint (Neg, product)
        else let _,product = mul' (value1, [1], value2) in
            if neg1 = neg2
            then Bigint (Pos, product)
            else Bigint (Neg, product)

    let rec divrem' (dividend, powerof2, divisor') =
        if (cmp' divisor' dividend 0) > 0
        then [], dividend
        else let quotient, remainder =
             divrem' (dividend, double powerof2, double divisor')
         in  if (cmp' remainder divisor' 0) < 0
             then quotient, remainder
             else (add' quotient powerof2 0),
              (canonicalize (sub' remainder divisor' 0))

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let quotient, _ = divrem (dividend, divisor)
        in if neg1 = neg2
            then Bigint (Pos, quotient)
            else Bigint (Neg, quotient)

    let rem (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        if (cmp' dividend divisor 0) < 0
        then Bigint (neg1, dividend)
        else
          let _, remainder = divrem (dividend, divisor)
          in if (cmp' [] remainder 0) = 0 then zero
          else
          match (neg1, neg2) with
          | Pos, Pos    ->  Bigint (Pos, remainder)
          | Neg, Neg    ->  Bigint (Neg, remainder)
          | Pos, Neg    ->  Bigint (Pos ,remainder)
          | Neg, Pos    ->  Bigint (Neg ,remainder)

    let odd number = let head = car number in head mod 2 = 1

    let even number = let head = car number in head mod 2 = 0

    let rec power' (base, expt, result) = match expt with
        | [0]                -> result
        | []                 -> result
        | expt when odd expt ->
            let _, baseresult = mul' (base, [1], result)
                and exptmin1 = canonicalize (sub' expt [1] 0)
                in power' (base, exptmin1, baseresult)
        | expt               ->
            let exptdiv2, _ = divrem' (expt, [1], [2])
                and _, basebase = mul' (base, [1], base)
                in power' (basebase, exptdiv2, result)

    let pow (Bigint (neg1, base)) (Bigint (neg2, exponent)) =
        if neg2 = Neg then zero else
        match (neg1, exponent) with
        | _, exponent when (cmp' exponent [0] 0) = 0 ->
            Bigint (Pos, [1])
        | Neg, exponent when odd exponent   ->
            Bigint (Neg, power' (base, exponent, [1]))
        | Neg, exponent when even exponent  ->
            Bigint (Pos, power' (base, exponent, [1]))
        | _, _                              ->
            Bigint (Pos, power' (base, exponent, [1]))

end
