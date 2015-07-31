
DO (10) NEXT

(10)    PLEASE FORGET #1
        DO WRITE IN .1
        DO (2000) NEXT
        DO READ OUT .1
        DO GIVE UP

(20)    PLEASE FORGET #1
        DO WRITE IN .1
        DO (22) NEXT
(21)    DO (2010) NEXT
        DO READ OUT .1
(22)    PLEASE FORGET #1
        DO (21) NEXT
        DO READ OUT .1
        DO GIVE UP

(30)    PLEASE FORGET #1
        DO WRITE IN .1
        DO WRITE IN .2
        DO (2020) NEXT
        DO READ OUT .1
        DO GIVE UP



(2010)  PLEASE ABSTAIN FROM (2004)
(2000)  DO STASH .2
        DO .2 <- #1
        DO (2001) NEXT
(2001)  PLEASE FORGET #1
        DO .1 <- '?.1$.2'~'#0$#65535'
        DO (2002) NEXT
        DO .2 <- !2$#0'~'#32767$#1'
        DO (2001) NEXT
(2003)  PLEASE RESUME "?!1~.2'$#1"~#3
(2002)  DO (2003) NEXT
        PLEASE RETRIEVE .2
(2004)	PLEASE RESUME #2
	PLEASE DO REINSTATE (2004)
	PLEASE RESUME '?"!1~.1'~#1"$#2'~#6

(2020)  PLEASE STASH .2 + .3
	DO (1021) NEXT



        (1020)  DO STASH .2 + .3
                DO .2 <- #1
                PLEASE DO (1021) NEXT
        (1021)  DO FORGET #1
                DO .3 <- "?!1~.2'$#1"~#3
                PLEASE DO .1 <- '?.1$.2'~'#0$#65535'
                DO (1022) NEXT
                DO .2 <- !2$#0'~'#32767$#1'
                DO (1021) NEXT
        (1023)  PLEASE RESUME .3
        (1022)  DO (1023) NEXT
                PLEASE RETRIEVE .2 + .3
                PLEASE RESUME #2
