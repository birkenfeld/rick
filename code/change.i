        DO WRITE IN .4
        DO .5 <- #5
        DO .6 <- #0
        DO (300) NEXT
        DO READ OUT .3
        PLEASE GIVE UP

(100)   DO (110) NEXT
        DO .2 <- #50
        PLEASE RESUME #1
(110)   DO (120) NEXT
        DO .2 <- #25
        PLEASE RESUME #2
(120)   DO (130) NEXT
        DO .2 <- #10
        PLEASE RESUME #3
(130)   DO (140) NEXT
        DO .2 <- #5
        PLEASE RESUME #4
(140)   DO (150) NEXT
        DO .2 <- #1
        PLEASE RESUME #5
(150)   DO RESUME .5

(200)   DO (201) NEXT
        DO .3 <- #1
        DO RESUME #1
(202)   PLEASE RESUME '?"!4~.4'~#1"$#2'~#3
(201)   DO (202) NEXT
        DO (203) NEXT
        DO .3 <- #0
        DO RESUME #2
(204)   PLEASE RESUME '?"'&"?!4~#32768'$#1"$".5~.5"'~#1"$#2'~#3
(203)   DO (204) NEXT
        PLEASE FORGET #2
        DO .1 <- .4
        DO (100) NEXT
        DO (1010) NEXT
        PLEASE STASH .4
        DO .4 <- .3
        DO (200) NEXT
        PLEASE RETRIEVE .4
        PLEASE STASH .5 + .6
        DO .6 <- .3
        DO .1 <- .5
        DO (2000) NEXT
        DO .5 <- .1
        DO (200) NEXT
        DO .1 <- .3
        DO .2 <- .6
        PLEASE RETRIEVE .5 + .6
        DO (1000) NEXT
        PLEASE RESUME #1

        PLEASE COME FROM (310)
(300)   DO (301) NEXT
        DO .1 <- .6
        DO (1020) NEXT
        DO .3 <- .1
        DO RESUME #1
(302)   PLEASE RESUME '?"!4~.4'~#1"$#2'~#3
(301)   DO (302) NEXT
        DO (303) NEXT
        DO .3 <- .6
        DO RESUME #2
(304)   PLEASE RESUME '?"'&"?!4~#32768'$#1"$".5~.5"'~#1"$#2'~#3
(303)   DO (304) NEXT
        PLEASE FORGET #2
        DO .1 <- .5
        DO (2000) NEXT
        PLEASE STASH .4 + .5
        DO .5 <- .1
        DO (300) NEXT
        PLEASE RETRIEVE .4 + .5
        DO .6 <- .3
        DO .1 <- .4
        DO (100) NEXT
        DO (1010) NEXT
(310)   DO .4 <- .3

(2000)  PLEASE STASH .2
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
(2004)  PLEASE RESUME #2
