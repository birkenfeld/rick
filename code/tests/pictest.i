PLEASE NOTE This continuously copies PORTA to PORTB
	    (PORTA is always input, PORTB is always output)
	DO .2 <- #0
	DO .1 <- #65280
	PLEASE COME FROM (11)
	DO (1520) NEXT
	DO PIN :1
	DO .1 <- :1~#255
	PLEASE DO .2 <- #257
	DO (1030) NEXT
(11)	DO .1 <- .3
