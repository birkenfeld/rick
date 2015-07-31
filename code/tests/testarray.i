        PLEASE DO ,1 <- #3 BY #3
        DO ;2 <- #6
        DO ,2 <- #6
	DO .1 <- #1
	DO .2 <- #1
	DO ,1 SUB .2.1 <- #1
	DO ;2 SUB #5 <- #1
	DO .1 <- "?',1 SUB .2.1'$#1"~#3
	DO .2 <- ,1 SUB #1 ;2 SUB #5
	DO .1 <- .2~,1 SUB #1 #1
	DO STASH .1 + ,2 + ;2 + ,1 + ,3
	DO ,1 <- #500
	DO ,1 SUB #1 <- #112
	PLEASE DO READ OUT ,1 SUB .2
	PLEASE RETRIEVE ,2 + ,1 + ,3
	DO .1 <- #?1
	DO .1 <- .?1
	PLEASE DO READ OUT ,1 SUB #1 #1
	DO .1 <- .2$,&1 SUB #1 #1
	PLEASE DO READ OUT .1
	DO .1 <- .2$,V1 SUB #1 #1
	PLEASE DO READ OUT .1

	PLEASE GIVE UP
