	PLEASE NOTE that this program checks the routine 1910 and calculates
	its standard deviation; the output values should quickly converge to
	around #10000 if that routine correctly follows the manual.
	DO :3 <- #0
	PLEASE DO .6 <- #0
	DO COME FROM (20)
	DO .1 <- #1200
	DO (1910) NEXT
	DO .1 <- #600
(5)     DO (1010) NEXT
	DO .1 <- .2
	DO .2 <- #600
	DO (1010) NEXT
	DO COME FROM '.3~#2048'$#3
	DO .1 <- .3
	PLEASE DO .2 <- .3
	DO (1530) NEXT
	DO :2 <- :3
	DO (1500) NEXT
	DO .1 <- .6
	PLEASE DO (1020) NEXT
	DO .6 <- .1
	DO :1 <- :3
	DO :2 <- .1
	PLEASE DO (1550) NEXT
	DO READ OUT :3
	DO :3 <- :1
(20)	PLEASE DO NOTHING
