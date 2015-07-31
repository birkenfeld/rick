(2010)  PLEASE ABSTAIN FROM (2004)
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
(2004)	PLEASE RESUME #2
	DO REINSTATE (2004)
	PLEASE RESUME '?"!1~.1'~#1"$#2'~#6
(2020)  PLEASE STASH .2 + .3
	DO (1021) NEXT
(2030)	DO STASH .1 + .5
	DO .3 <- #0
	DO .5 <- '?"!2~.2'~#1"$#1'~#3
	PLEASE DO (2031) NEXT
	DO .4 <- #1
	PLEASE DO (2033) NEXT
(2033)	DO FORGET #1
	DO .5 <- '?".2~#32768"$#2'~#3
	DO (2032) NEXT
	DO .2 <- !2$#0'~'#32767$#1'
	PLEASE DO .4 <- !4$#0'~'#32767$#1'
	DO (2033) NEXT
(2032)	DO (1001) NEXT
(2036)	PLEASE FORGET #1
        DO .5 <- '?.1$.2'~'#0$#65535'
        DO .5 <- '?"'&"!2~.5'~'"?'?.5~.5'$#32768"~"#0$#65535"'"$
                 ".5~.5"'~#1"$#2'~#3
	DO (2034) NEXT
	DO .5 <- .3
	DO (1010) NEXT
	PLEASE DO .1 <- .3
        DO .3 <- 'V.4$.5'~'#0$#65535'
	DO (2035) NEXT
(2034)	PLEASE DO (1001) NEXT
(2035)	DO FORGET #1
	DO .5 <- "?'.4~#1'$#2"~#3
	DO (2031) NEXT
	DO .2 <- .2~#65534
	DO .4 <- .4~#65534
	PLEASE DO (2036) NEXT
(2031)	DO (1001) NEXT
	PLEASE DO .4 <- .1
	DO RETRIEVE .1 + .5
	PLEASE RESUME #2
(2040)  PLEASE STASH .4
	DO ABSTAIN FROM (2047)
(2049)  DO STASH .2 + .5 + :2 + :3 + :4 + :5 + :6
	PLEASE DO .4 <- #0
	DO :5 <- .1
	DO :6 <- #1
	DO (2044) NEXT
	DO .4 <- '?.4$#1'~#3
(2047)	DO (2048) NEXT
	DO .5 <- .4
	DO (2046) NEXT
	DO (1999) NEXT
(2046)  DO (1001) NEXT
	PLEASE REINSTATE (2047)
	PLEASE RETRIEVE .4
(2048)	DO :1 <- :6
	DO RETRIEVE .2 + .5 + :2 + :3 + :4 + :5 + :6
	PLEASE RESUME #2
(2044)  DO (2045) NEXT
(2045)	PLEASE FORGET #1
	DO .5 <- "?!2~#1'$#1"~#3
	DO (2041) NEXT
	DO :3 <- :6~"#65535$#65534"
	DO .5 <- '?"':3~:3'~#1"$#1'~#3
	DO (2042) NEXT
	PLEASE DO :1 <- :6
	PLEASE DO :2 <- :5
	DO (1549) NEXT
	DO :6 <- :3
	DO .4 <- 'V.4$":4~#2"'~#1
	DO (2043) NEXT
(2042)  DO (1001) NEXT
	PLEASE FORGET #1
	DO :6 <- :5
	DO (2043) NEXT
(2041)  DO (1001) NEXT
(2043)	PLEASE FORGET #1
	DO .2 <- .2~#65534
	DO .5 <- '?"!2~.2'~#1"$#2'~#3
	DO (1001) NEXT
	PLEASE DO :1 <- :5
	PLEASE DO :2 <- :5
	DO (1549) NEXT
	DO :5 <- :3
	DO .4 <- 'V.4$":4~#2"'~#1
	DO (2045) NEXT
