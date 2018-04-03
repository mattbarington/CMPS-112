let rec mul’ (multiplier, powerof2, multiplicand’) =
 if powerof2 > multiplier
 then multiplier, 0
 else let remainder, product =
 mul’ (multiplier, double powerof2, double multiplicand’)
 in if remainder < powerof2
 then remainder, product
 else remainder - powerof2, product + multiplicand’