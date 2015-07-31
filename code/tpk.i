        PLEASE ABSTAIN FROM (29733)
(29733) DON KNUTH'S IMPLEMENTATION OF TPK IN INTERCAL
                  (C) MARCH 2003, NOVEMBER 2010
        REFERENCE --- THE EARLY HISTORY OF PROGRAMMING
        LANGUAGES, BY D E KNUTH AND L TRABB PARDO

        NOTA BENE: THE INPUT AND OUTPUT DATA ARE SCALED
        DECIMAL NUMBERS WITH SIX DIGITS TO THE RIGHT OF
        THE DECIMAL POINT; THUS AN INPUT OF
           THREE ONE FOUR ONE FIVE NINE THREE
        DENOTES 3.141593, AND THAT VALUE WOULD BE OUTPUT AS
           ______
           MMMCXLMDXCIII

        PLEASE NOTICE THAT VARIABLE NAMES AND SUBROUTINE NAMES USE
        THE 5-BIT TELEPRINTER CODE IN LETTER-SHIFT MODE, NAMELY
  / E @ A : S I U 1/4 D R J N F C K T Z L W H Y P Q O B G " M X V $
        (WHICH ALAN TURING ADVISED EVERY PROGRAMMER TO LEARN)

        PLEASE (6534) NEXT
        DO ;29 <- #2
        DO ;3 <- #11 BY #2
        PLEASE DO .6 <- #0
        DO (1) NEXT
(1)     PLEASE DO FORGET #1
        DO WRITE IN :1        
        DO (22919) NEXT
        PLEASE .1 <- .6
        PLEASE DO (1020) NEXT
        DO .11 <- .1
        DO ;3 SUB .11 #1 <- ;1 SUB #1
        DO ;3 SUB .11 #2 <- ;1 SUB #2
        DO .1 <- #10 
        DO (29904) NEXT
        DO (1) NEXT
        DO REINSTATE NEXTING
        PLEASE DO (2) NEXT
(2)     DO FORGET #1
        DO .984 <- #0
        PLEASE .1 <- .6
        PLEASE DO (1020) NEXT
        DO .11 <- .1
        DO ;29 SUB #1 <- ;3 SUB .11 #1
        DO ;29 SUB #2 <- ;3 SUB .11 #2
        PLEASE DO (13) NEXT
        DO (15478) NEXT
        DO (15320) NEXT 
        DO :2 <- #6528$#32544
        PLEASE DO (23438) NEXT
        DO :1 <- #31640$#20792
        DO REMEMBER :1
        DO READ OUT .6 + :1
        DO .1 <- #0
        DO ABSTAIN FROM (711)
        PLEASE DO (29904) NEXT
        DO (2) NEXT
        DO GIVE UP

        PLEASE USE THE FOLLOWING FUNCTION, WHICH SETS ;1 <- F(;X)
        WHERE ;29 AND ;1 ARE EXTENDED FIXED-POINT NUMBERS
        (THAT IS, THEY ARE VECTORS WITH TWO COMPONENTS,
             #1=INTEGER PART, #2=FRACTION PART)
(13)    DO STASH ;2
        DO ;1 SUB #1 <- ;29 SUB #1
        DO ;1 SUB #2 <- ;29 SUB #2
        PLEASE STASH ;1
        PLEASE DO ;2 SUB #1 <- ;29 SUB #1
        DO ;2 SUB #2 <- ;29 SUB #2
        DO (30300) NEXT
        DO (30300) NEXT
        DO ;2 SUB #1 <- #5
        DO ;2 SUB #2 <- #0
        PLEASE DO (30300) NEXT
        PLEASE DO ;2 SUB #1 <- ;1 SUB #1
        DO ;2 SUB #2 <- ;1 SUB #2
        PLEASE RETRIEVE ;1
        DO (30499) NEXT
        PLEASE DO (30218) NEXT
        DO (29987) NEXT
        DO RETRIEVE ;2
        PLEASE RESUME #1

        DO NOTHING BUT BASIC SUBROUTINES FROM HERE ON
        -------------
        FIRST THERE ARE ROUTINES FOR EXTENDED ARITHMETIC
        (DOUBLE-DOUBLE PRECISION), WHICH CONSISTS OF
        TWO 32-BIT NUMBERS WITH A BINARY POINT BETWEEN THEM
        
        TO GET STARTED, DO (INI) FIRST; IT DEFINES BASIC ARRAYS
(6534)  DO ;1 <- #3
        DO ;2 <- #2
        PLEASE RESUME #1

        DON'T FORGET TO TEST FOR OVERFLOW AFTER A SERIES OF
        EXTENDED ARITHMETIC OPERATIONS:
        THE (OVC) ROUTINE SETS :1 TO THE MAX VALUE IF .OV IS 1
        ... SO YOU'D BETTER SET .OV TO 0 PERIODICALLY
(15320) PLEASE DO (2000) NEXT
        DO :1 <- #65535$#65535
        PLEASE RESUME #1
(2000)  PLEASE DO (2001) NEXT
        PLEASE RESUME #2
(2001)  DO RESUME '?.984$#1'~#3

        DOING (ADY) NEXT SETS ;1 <- ;1+;2+.C~2 AND .C <- CARRY+1
        DOING (ADZ) NEXT SETS ;1 <- ;1+;2 AND .C <- CARRY+1
(17699) PLEASE .14 <- #1
(21795) DO STASH :1 + :2 + :3 + :4
        DO :1 <- ;1 SUB #2
        PLEASE DO :2 <- .14~#2
        PLEASE DO (1509) NEXT
        DO .14 <- :4~#2
        DO :1 <- :3
        PLEASE DO :2 <- ;2 SUB #2
        DO (1509) NEXT
        DO ;1 SUB #2 <- :3
        DO :1 <- ;1 SUB #1
        DO :2 <- "V.14$':4~#2'"~#1
        DO (1509) NEXT
        DO .14 <- :4
        DO :1 <- :3
        DO :2 <- ;2 SUB #1
        DO (1509) NEXT
        DO ;1 SUB #1 <- :3
        DO .14 <- '?:4$.14'~#6
        PLEASE RETRIEVE :1 + :2 + :3 + :4
        PLEASE RESUME #1

        DOING (ADX) NEXT SETS ;1 <- ;1+;2 AND TRACKS OVERFLOW
        DOING (ABX) NEXT SETS ;1 <- ABS(;1)
(29987) PLEASE STASH .14
        DO (17699) NEXT
        DO .984 <- "V.984$!14~#2'"~#1
        PLEASE RETRIEVE .14
(30499) DO RESUME #1

        DOING (SUX) NEXT SETS ;1 <- ;1-;2 AND TRACKS OVERFLOW
(29925) PLEASE STASH ;2 + .14
        DO ;2 SUB #1 <- '"?'";2 SUB #1"~"#65535$#0"'$#65535"~"#0$#65535"'$
                        '"?'";2 SUB #1"~"#0$#65535"'$#65535"~"#0$#65535"'
        DO ;2 SUB #2 <- '"?'";2 SUB #2"~"#65535$#0"'$#65535"~"#0$#65535"'$
                        '"?'";2 SUB #2"~"#0$#65535"'$#65535"~"#0$#65535"'
        DO .14 <- #2
        DO (21795) NEXT
        PLEASE DO .984 <- "V.984$!14~#1'"~#1
        PLEASE RETRIEVE ;2 + .14
        DO RESUME #1

        DOING (SHY) NEXT SETS ;2 <- :3 * 2^16 AND CLOBBERS :3
(22149) DO ;2 SUB #1 <- :3 ~ '#65280$#65280'
        DO :3 <- '"':3~#43690'$#0"~"#65535$#1"' $
                 '"':3~#21845'$#0"~"#65535$#1"'
        PLEASE :3 <- ":3~'#511$#1'" $ ":3~'#1$#511'"
        PLEASE :3 <- ":3~'#1023$#3'" $ ":3~'#3$#1023'"
        DO ;2 SUB #2 <- ":3~'#4095$#15'" $ ":3~'#15$#4095'"                 
        PLEASE RESUME #1

        DOING (MLY) NEXT SETS ;1 <- :1 * :2 / 2^32
(22108) PLEASE STASH :1 + :2 + :3 + ;2 + .14
        DO :1 <- :1 ~ #65535
        DO :2 <- :2 ~ #65535
        DO (1540) NEXT
        DO ;1 SUB #2 <- :3
        DO ;1 SUB #1 <- #0
        PLEASE RETRIEVE :1
        PLEASE STASH :1
        DO :1 <- :1 ~ '#65280$#65280'
        DO (1540) NEXT
        DO (22149) NEXT 
        DO (17699) NEXT
        PLEASE RETRIEVE :1 + :2
        PLEASE STASH :1 + :2
        DO :1 <- :1 ~ #65535
        DO :2 <- :2 ~ '#65280$#65280'
        DO (1540) NEXT 
        DO (22149) NEXT
        DO (17699) NEXT
        PLEASE RETRIEVE :1
        PLEASE STASH :1
        DO :1 <- :1 ~ '#65280$#65280'
        DO (1540) NEXT
        DO ;2 SUB #2 <- #0
        DO ;2 SUB #1 <- :3
        DO (17699) NEXT
        PLEASE RETRIEVE :1 + :2 + :3 + ;2 + .14
        PLEASE RESUME #1

        DOING (MLZ) NEXT SETS ;1 <- ;1 + (:1 * :2 / 2^32), TRACKING OVERFLOW
(18012) PLEASE STASH ;1 + ;2
        DO (22108) NEXT
        PLEASE ;2 SUB #1 <- ;1 SUB #1
        PLEASE ;2 SUB #2 <- ;1 SUB #2
        PLEASE RETRIEVE ;1
        DO (29987) NEXT
        PLEASE RETRIEVE ;2
        PLEASE RESUME #1

        DOING (MLX) NEXT SETS ;1 <- ;1*;2 AND TRACKS OVERFLOW
(30300) PLEASE STASH :1 + :2 + :3 + :4 + ;1 + ;2
        DO :1 <- ;1 SUB #2
        DO :2 <- ;2 SUB #2
        DO (22108) NEXT
        DO :1 <- ;1 SUB #1
        DO :2 <- ';1 SUB #2' ~ '#32768$#0'
        DO (1500) NEXT
        PLEASE RETRIEVE ;1
        DO :4 <- ;1 SUB #2
        DO :1 <- ;1 SUB #1
        DO :2 <- ;2 SUB #1
        DO (22108) NEXT
        PLEASE DO .984 <- "V.984$' "';1 SUB #1'~';1 SUB #1'" ~#1'" ~ #1
        DO ;1 SUB #1 <- ;1 SUB #2
        DO ;1 SUB #2 <- :3
        DO :2 <- ;2 SUB #2
        DO (18012) NEXT
        DO :1 <- :4
        DO :2 <- ;2 SUB #1
        DO (18012) NEXT
        PLEASE RETRIEVE :1 + :2 + :3 + :4 + ;2
        PLEASE RESUME #1

        DOING (RTX) NEXT SETS ;1 <- SQRT(;1)
(30218) PLEASE STASH .6 + ;2 + ;3 + ;4 + :1 + :2 + .1 + .2 + .3 + .14 + .984
        DO ;4 <- #2
        DO ;3 <- #2
        DO .6 <- #1
        DO ;3 SUB #1 <- #0
        DO ;3 SUB #2 <- #0
        DO ;4 SUB #1 <- #65535$#65535
        DO ;4 SUB #2 <- ;4 SUB #1
        DO :1 <- ;1 SUB #1
        DO :2 <- ;1 SUB #2
        PLEASE (2003) NEXT
(2002)  DO ;2 SUB #1 <- ;1 SUB #1
        DO ;2 SUB #2 <- ;1 SUB #2
        DO (17699) NEXT
        DO ;2 SUB #1 <- ;1 SUB #1
        DO ;2 SUB #2 <- ;1 SUB #2
        DO (17699) NEXT
        PLEASE DO ;2 SUB #1 <- #0
        PLEASE DO ;2 SUB #2 <- .2
        DO (17699) NEXT
        PLEASE DO ;2 SUB #2 <- .3
        DO (29925) NEXT
        PLEASE DO ;3 SUB #1 <- ;1 SUB #1
        PLEASE DO ;3 SUB #2 <- ;1 SUB #2
        PLEASE DO ;1 SUB #1 <- ;4 SUB #1
        PLEASE DO ;1 SUB #2 <- ;4 SUB #2
        DO ;2 SUB #1 <- ;1 SUB #1
        DO ;2 SUB #2 <- ;1 SUB #2
        DO (17699) NEXT
        DO ;2 SUB #1 <- #0
        PLEASE DO ;2 SUB #2 <- "?#1$.3"~#1
        DO (17699) NEXT
        PLEASE DO ;4 SUB #1 <- ;1 SUB #1
        PLEASE DO ;4 SUB #2 <- ;1 SUB #2
        DO .1 <- ":1 ~ '#21845$#0'" $
         '"?':1~"#10922$#1"' $ '"?':1~#1'$':2~"#32768$#0"'"~#1'" ~#21845'
        DO .2 <- ":1 ~ '#0$#21845'" $
         '"?':1~"#0$#10923"' $ '"?':1~#1'$':2~"#0$#32768"'"~#1'" ~#21845'
        DO :1 <- .1 $ .2
        DO .1 <- ":2 ~ '#21845$#0'" $
         '"&':2~"#10922$#1"' $ #65534" ~ #21845'
        DO .2 <- ":2 ~ '#0$#21845'" $
         '"&':2~"#0$#10923"' $ #65534" ~ #21845'
        DO :2 <- .1 $ .2
(2003)  DO .2 <- :1 ~ '#32768$#32768'
        DO ;1 SUB #1 <- ;3 SUB #1
        DO ;1 SUB #2 <- ;3 SUB #2
        DO ;2 SUB #1 <- ;4 SUB #1
        DO ;2 SUB #2 <- ;4 SUB #2
        DO .14 <- #2
        DO (21795) NEXT
        DO .3 <- ' " '"';1 SUB #1'~';1 SUB #1'"~#1' $
                     '"';1 SUB #2'~';1 SUB #2'"~#1' " $ .2' ~ #15
        DO .3 <- '&"!3~.3'~#1" $ .14' ~ #2
        PLEASE DO (2004) NEXT
        DO ;1 SUB #1 <- ;3 SUB #1
        DO ;1 SUB #2 <- ;3 SUB #2
        DO (2005) NEXT
(2004)  DO (2006) NEXT
(2005)  PLEASE FORGET #1
        PLEASE DO .1 <- #49
        DO (29904) NEXT
        DO (2002) NEXT
        PLEASE REINSTATE NEXTING
        DO ;2 SUB #1 <- ;4 SUB #1
        DO ;2 SUB #2 <- ;4 SUB #2
        DO ;1 SUB #1 <- #0
        DO ;1 SUB #2 <- #0
        DO (29925) NEXT
        DO ;2 SUB #1 <- #0
        DO ;2 SUB #2 <- "?.3$#1"~#1
        DO (29925) NEXT
        PLEASE RETRIEVE .6 + ;2 + ;3 + ;4 + :1 + :2 + .1 + .2 + .3 + .14 + .984
        PLEASE RESUME #50
(2006)  PLEASE RESUME "?.3$#2"~#3

        DOING (UNP) NEXT SETS ;1 <- :1 / 1000000
        (WHICH IS ESSENTIALLY DECIMAL TO BINARY CONVERSION)
(22919) PLEASE STASH :1 + :2 + :3 + :4 + .3
        DO :2 <- #784 $ #904
        PLEASE DO (1550) NEXT
        PLEASE DO ;1 SUB #1 <- :3
        DO :1 <- :3
        DO (1540) NEXT
        DO RETRIEVE :1
        PLEASE STASH :1
        DO :2 <- :3
        PLEASE DO (1510) NEXT
        PLEASE DO :4 <- #32768 $ #0
        DO ;2 SUB #1 <- #0
        DO ;2 SUB #2 <- #0
        DO :1 <- :3
        PLEASE DO (2008) NEXT
(2007)  DON'T RESUME #1
        DO :1 <- ;1 SUB #2
        DO .3 <- :4~#1
        DO (2009) NEXT
        DO :4 <- :4~'#65535$#65534'
(2008)  DO :2 <- :1
        PLEASE DO (1500) NEXT
        DO :1 <- :3
        DO ;1 SUB #2 <- :3
        DO :2 <- #48576
        PLEASE DO (1500) NEXT
        DO .3 <- :3~'#0$#1024'
        DO ;1 SUB '"?.3$#2"~#6' <- ':3~"#65535$#0"' $
                 '"&'":3~'#0$#65535'"$#64511'" ~ "#0$#65535"'
        DO :2 <- ;2 SUB '"?.3$#1"~#3'
        DO :2 <- "'V":2~'#65535$#0'"$":4~'#65535$#0'"' ~ '#0$#65535'" $
                 "'V":2~'#0$#65535'"$":4~'#0$#65535'"' ~ '#0$#65535'"
        PLEASE DO ;2 SUB '"?.3$#1"~#3' <- :2
        PLEASE DO (2007) NEXT
        DO ;1 SUB #2 <- ;2 SUB #2
        PLEASE ABSTAIN FROM (2007)
        PLEASE RETRIEVE :1 + :2 + :3 + :4 + .3
        PLEASE RESUME #34
(2009)  PLEASE DO (2006) NEXT
        DO REINSTATE (2007)
        PLEASE FORGET #2
        DO (2008) NEXT

        DOING (PAK) NEXT SETS :1 <- 1000000 * ;1, TRACKING OVERFLOW
        (WHICH IS ESSENTIALLY BINARY TO DECIMAL CONVERSION)
(15478) PLEASE STASH ;1 + ;2 + :2 + :3 + :4
        DO ;2 SUB #1 <- #784 $ #904
        DO ;2 SUB #2 <- #0
        DO (30300) NEXT
        DO :1 <- ;1 SUB #1
        DO :2 <- ';1 SUB #2' ~ '#32768$#0'
        DO (1509) NEXT
        DO .984 <- "V.984$':4~#2'"~#1
        PLEASE RETRIEVE ;1 + ;2 + :2 + :3 + :4
        PLEASE RESUME #1

        DON'T STOP READING YET: TWO IMPORTANT UTILITY ROUTINES REMAIN

        ---------- UTILITIES ----------------------------------------       

        DOING (CMP) NEXT IMMOBILIZES :1 IF :1 < :2
        HERE I USE A SLICK TRICK FROM THE ORIGINAL INTERCAL DIVISION ROUTINE
(23438) PLEASE STASH .3 + :1
        DO :1 <- ' "? ':1~"#65535$#0"' $ ':2~"#65535$#0"' " ~ "#0$#65535"' $
                 ' "? ':1~"#0$#65535"' $ ':2~"#0$#65535"' " ~ "#0$#65535"'
        DO .3 <- ':2~:1' ~
            " ' "? '"?:1~:1"~"#65535$#0"' $ #32768"~"#0$#65535" ' $
                 '"?:1~:1"~"#0$#65535"' "
        PLEASE RETRIEVE :1
        DO (2010) NEXT
        DO (2011) NEXT
(2010)  DO (2006) NEXT
        PLEASE IGNORE :1
(2011)  DO RETRIEVE .3
        DO RESUME #2

        DOING (TIX) NEXT IS INTENDED TO SIMPLIFY LOOPS ON THE VARIABLE .I
        IF .I = .1, NEXTING IS TURNED OFF
        OTHERWISE .I IS INCREASED BY +1 OR -1, WHERE THE
        INCREMENT IS -1 IF (UP) HAS JUST BEEN ABSTAINED FROM
(29904) PLEASE STASH .1 + .2 + .3 + .4
        DO .3 <- "'"?.6$.1"~#21845' ~ '"?.6$.1"~#21845'" ~ #1
        DO (2012) NEXT
        PLEASE ABSTAIN FROM NEXTING
        PLEASE RETRIEVE .1 + .2 + .3 + .4
        PLEASE RESUME #1
(2012)  DO (2006) NEXT
        DO .1 <- .6
(711)   DO (2013) NEXT
        PLEASE REINSTATE (711)
        DO .2 <- #1
        DO (1010) NEXT
        DO .6 <- .3
        DO (2014) NEXT
(2013)  DO (1020) NEXT
        DO .6 <- .1
(2014)  PLEASE RETRIEVE .1 + .2 + .3 + .4
        DO RESUME #3

        PLEASE NOTIFY THE AUTHOR IF YOU'VE BEEN ABLE TO
        UNDERSTAND ALL OF THIS; BUT PLEASE DON'T SEND EMAIL

        FINAL PUZZLE: WHAT IS SO INTERESTING ABOUT 885205232?
