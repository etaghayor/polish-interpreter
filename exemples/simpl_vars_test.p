COMMENT x and n not initialized in this file
res := + + 0 4 * x + 3 5
COMMENT Only ELSE should be printed here
IF 5 < 0
  res := - 0 n
ELSE
  res := n
COMMENT This WHILE should be ignored
WHILE 2 > 5
  PRINT 3
COMMENT ELSE should be ignored
IF 5 > 0
  res := - 0 n
ELSE
  res := n
WHILE 7 > 5
  PRINT 3
PRINT x
