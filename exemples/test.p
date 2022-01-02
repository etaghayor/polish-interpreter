n :=  1
IF 0 > 1
  READ a
  PRINT a
ELSE
  COMMENT PRINT a
  READ a
PRINT a
a := 2
b := + n a
b := / n 0
c := - b b
d := / 0 b
COMMENT this block bugs:
IF n > 0
  n := -4
ELSE
  n := / 2 1
t := / 2 n
a := - a n
mi := -5
WHILE mi <= 0
  COMMENT mi := + mi 1
mii := -5
WHILE mii < 0
  mii := + mii 1
