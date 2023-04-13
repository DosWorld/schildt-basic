PRINT "This program demostrates all commands."
FOR X = 1 TO 100
PRINT X, X/2; X, X*X
NEXT
GOSUB 300
PRINT "Hello"
INPUT H
IF H<11 THEN GOTO 200
PRINT 12-4/2
PRINT 100
200 A = 100/2
IF A>10 THEN PRINT "This is ok"
PRINT A
PRINT A+34
INPUT "H=",H
PRINT H
INPUT "Y=",y
PRINT H+Y
END
300 PRINT "This is a subroutine"
    RETURN