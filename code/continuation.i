	PLEASE NOTE

Copyright (C) 2008 Alex Smith

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

	PLEASE NOTE Creating the continuation statements
	DO CREATE (8200) GET CONTINUATION IN .1 GETTING .2
	DO CREATE (8200) GET CONTINUATION IN ,1 SUB #1 GETTING .2
	DO CREATE (8200) GET CONTINUATION IN .1 GETTING ,2 SUB #2
	DO CREATE (8200) GET CONTINUATION IN ,1 SUB #1 GETTING ,2 SUB #2
	DO CREATE (8210) CONTINUE WITH .1 SENDING .2
	DO CREATE (8210) CONTINUE WITH .1 SENDING .2~#2
	DO CREATE (8210) CONTINUE WITH .1 SENDING ,2 SUB #2
	DO CREATE (8210) CONTINUE WITH .1~#1 SENDING .2
	DO CREATE (8210) CONTINUE WITH .1~#1 SENDING .2~#2
	DO CREATE (8210) CONTINUE WITH .1~#1 SENDING ,2 SUB #2
	DO CREATE (8210) CONTINUE WITH ,1 SUB #1 SENDING .2
	DO CREATE (8210) CONTINUE WITH ,1 SUB #1 SENDING .2~#2
	DO CREATE (8210) CONTINUE WITH ,1 SUB #1 SENDING ,2 SUB #2
	DO CREATE (8205) KILL ALL THREADS
	PLEASE NOTE Initialising the counting tracker thread
	PLEASE DO .1 <- #0
(8201)	DO NOTHING

	PLEASE NOTE Definitions of the continuation statements
(8200)	DO STASH .1
	PLEASE DO (8280) NEXT
	PLEASE DO (1020) NEXT
	PLEASE DO (8270) NEXT
	PLEASE NOTE It's possible for .1 to equal :1601, so we
		    have to be careful about operating on the
		    arguments while stashed. So we use the
		    temporary global variable .8200.
	DO .8200 <- .1
	DO RETRIEVE .1
	DO :1601 <- .8200
	DO (8202) NEXT
	DO RESUME #1
(8202)	PLEASE NOTE Fork threads, one dormant, one alive
	DO COME FROM (8202)
	DO RESUME #1
(8213) 	DO RESUME '?".1~.1"$#2'~#3
(8212)	DO (8213) NEXT
	DO RETRIEVE .1 PLEASE NOTE Restoring .1's stash
	DO COME FROM (8202)
	PLEASE FORGET #1
(8211)	DO COME FROM (8211)
	PLEASE NOTE We count how many threads are in 8275 using the
		    abstention count of 8214 and block until they're
		    all done.
	PLEASE DO ABSTAIN #1 FROM (8214)
	DO STASH .1
	DO (8275) NEXT
	DO REINSTATE (8214)
	DO COME FROM (8215)
(8216)	DO NOTHING
(8215)	DO NOTHING
(8214) 	DO COME FROM (8216)
	PLEASE DO REINSTATE (8211)
	PLEASE NOTE If .8200 matches .1, we activate. This uses
		    a NEXT/NEXT/RESUME-style check because it's
		    done on multiple threads at once and because
		    a computed COME FROM would slow down the
		    program even further.
	DO .1 <- '?.8200$.1'~'#0$#65535'
	DO (8212) NEXT
	PLEASE NOTE that if this point is reached, we've reached
		    the point where unfreezing the thread is
		    necessary. First receive one datum from
		    8277 so we can send something back in the
		    continuation (this involves signaling using
		    8217), then the other thread kills itself and
		    we make a new continuation numbered the same
		    as the old one, and continue by doubleresuming
		    (so that an unfreeze can be told apart from an
		    initial call). As this is a CREATEd statement,
		    it's important to unstash :1601 and :1602
		    ourselves when doubleresuming, as the runtime
		    only does it correctly on a single resume.
	PLEASE DO ABSTAIN FROM (8217)
	DO (8260) NEXT
	DO (8276) NEXT
	DO .8201 <- .1
	DO RETRIEVE .1 PLEASE NOTE Restoring .1's stash
 	DO (8202) NEXT PLEASE NOTE .8200 is still correct!
	DO :1602 <- .8201
	PLEASE DO :1601 <- .8200
	DO RETRIEVE :1601
	DO RETRIEVE :1602
	DO RESUME #2

(8210)	PLEASE NOTE Here we use the timing guarantees. Once we
		    ABSTAIN FROM 8211, all the threads looping
		    there become unblocked simultaneously, so
		    all of them abstain 8214 (which is 2-3 cycles)
		    before any of them REINSTATES 8211 (which is
		    way lower down, after the call to 8275), so
		    we can guarantee that all the threads get a
		    chance to check to see if they match.
	DO .8210 <- .1
	PLEASE DO .1 <- :1601
	DO (8250) NEXT
	DO (8270) NEXT
	PLEASE DO ABSTAIN FROM (8211)
(8217)	DO COME FROM (8217) AGAIN
	PLEASE NOTE This double assignment is actually correct,
		    because :1602 is overloaded.
	PLEASE DO .1 <- .8210
	DO .1 <- :1602
	PLEASE DO (8277) NEXT
	PLEASE GIVE UP

	PLEASE NOTE A communications channel thread.
		    This acts like one 16-bit variable, where
	       8250 stashes the variable
	       8260 retrieves the variable
	       8270 copies it from .1
	   and 8280 copies it into .1

	       8275 is a mutex version of 8280 so that multiple
		    threads can try to view .1 at once and only
		    one will get it at a time.
	       8299 is a spinlock that traps until the current
		    operation is completed, which is needed as
		    part of the implementation of this, and then
		    double-resumes

(8299)	DO COME FROM (8299) AGAIN
	PLEASE DO RESUME #2

(8250)	DO REINSTATE (8251)
	PLEASE DO (8299) NEXT

(8260)	DO REINSTATE (8261)
	PLEASE DO (8299) NEXT

(8270)	DO REINSTATE (8271)
	DO (8277) NEXT
	PLEASE DO (8299) NEXT

(8280)	DO REINSTATE (8281)
	DO (8276) NEXT
	PLEASE DO (8299) NEXT

(8275)	DO COME FROM (8274)
(8274) 	DO (8273) NEXT ONCE
(8273) 	DO (8280) NEXT
	PLEASE DO REINSTATE (8274)
	DO RESUME #2

	PLEASE NOTE main part of the communications channel thread
	DO COME FROM (8201)
	PLEASE DO COME FROM (8259)
(8252)  DO NOTHING
(8262)	DO NOTHING
(8272)	DO NOTHING
(8282)  DO NOTHING
(8259)	PLEASE DO NOTHING

(8251)	DON'T NEXT FROM (8252) AGAIN
	DO STASH .1
	PLEASE DO ABSTAIN FROM (8299)
	DO RESUME #1

(8261)	DON'T NEXT FROM (8262) AGAIN
	DO RETRIEVE .1
	DO ABSTAIN FROM (8299)
	DO RESUME #1

(8271)	DON'T NEXT FROM (8272) AGAIN
	DO (8276) NEXT
	DO ABSTAIN FROM (8299)
	PLEASE DO RESUME #1

(8281)	DON'T NEXT FROM (8282) AGAIN
	DO (8277) NEXT
	DO ABSTAIN FROM (8299)
	PLEASE DO RESUME #1


	PLEASE NOTE 16-bit communications channel
		8276 will block until data is sent
		8277 will send the data
		data transferred from .1 to .1

(8276)	DO COME FROM (8276) AGAIN
	DO .1 <- #65535
(8283)	DO .1 <- '?.1$#    1'~'#0$#65535' AGAIN
(8284)	DO .1 <- '?.1$#    2'~'#0$#65535' AGAIN
(8285)	DO .1 <- '?.1$#    4'~'#0$#65535' AGAIN
(8286)	DO .1 <- '?.1$#    8'~'#0$#65535' AGAIN
(8287)	DO .1 <- '?.1$#   16'~'#0$#65535' AGAIN
(8288)	DO .1 <- '?.1$#   32'~'#0$#65535' AGAIN
(8289)	DO .1 <- '?.1$#   64'~'#0$#65535' AGAIN
(8290)	DO .1 <- '?.1$#  128'~'#0$#65535' AGAIN
(8291)	DO .1 <- '?.1$#  256'~'#0$#65535' AGAIN
(8292)	DO .1 <- '?.1$#  512'~'#0$#65535' AGAIN
(8293)	DO .1 <- '?.1$# 1024'~'#0$#65535' AGAIN
(8294)	DO .1 <- '?.1$# 2048'~'#0$#65535' AGAIN
(8295)	DO .1 <- '?.1$# 4096'~'#0$#65535' AGAIN
(8296)	DO .1 <- '?.1$# 8192'~'#0$#65535' AGAIN
(8297)	DO .1 <- '?.1$#16384'~'#0$#65535' AGAIN
(8298)	DO .1 <- '?.1$#32768'~'#0$#65535' AGAIN
	DO ABSTAIN FROM (8278)
	DO RESUME #1

(8277)	DO ABSTAIN .1~#    1 FROM (8283)
	DO ABSTAIN .1~#    2 FROM (8284)
	DO ABSTAIN .1~#    4 FROM (8285)
	DO ABSTAIN .1~#    8 FROM (8286)
	DO ABSTAIN .1~#   16 FROM (8287)
	DO ABSTAIN .1~#   32 FROM (8288)
	DO ABSTAIN .1~#   64 FROM (8289)
	DO ABSTAIN .1~#  128 FROM (8290)
	DO ABSTAIN .1~#  256 FROM (8291)
	DO ABSTAIN .1~#  512 FROM (8292)
	DO ABSTAIN .1~# 1024 FROM (8293)
	DO ABSTAIN .1~# 2048 FROM (8294)
	DO ABSTAIN .1~# 4096 FROM (8295)
	DO ABSTAIN .1~# 8192 FROM (8296)
	DO ABSTAIN .1~#16384 FROM (8297)
	DO ABSTAIN .1~#32768 FROM (8298)
	DO ABSTAIN FROM (8276)
(8278)	PLEASE DO COME FROM (8278) AGAIN
	PLEASE DO RESUME #1

	PLEASE NOTE Code for killing all threads. This
		    is achieved by hoping none of them are
		    in syslib (which is after the end
		    of the program), and abstaining from
		    all flow control operations in the
		    program, all operations that hold up
		    execution, and all operations that
		    could error. Unfortunately, this
		    can be affected by ONCE/AGAIN, so
		    each thread will twice more repeat
		    the abstention in the hope of catching
		    other threads as it comes past to quit.
		    Not perfect, but hopefully good enough.
                    (Of course, a simple E000 would be
		    better at this job, but it's an error
		    and non-erroring solutions are better.)
	PLEASE DO REINSTATE (8205) AGAIN
	PLEASE DO REINSTATE (8206) AGAIN
(8205)	PLEASE DO ABSTAIN FROM ABSTAINING + REINSTATING +
	CALCULATING + NEXTING + COMING FROM + RESUMING +
	FORGETTING + STASHING + RETRIEVING + WRITING IN +
	READING OUT + COMMENT AGAIN
	DON'T DO ANYTHING FOR A BIT
	PLEASE NOTE THAT THIS IS MORE TIME WASTING
(8206)	PLEASE DO ABSTAIN FROM ABSTAINING + REINSTATING +
	CALCULATING + NEXTING + COMING FROM + RESUMING +
	FORGETTING + STASHING + RETRIEVING + WRITING IN +
	READING OUT + COMMENT AGAIN
	PLEASE GIVE UP

	PLEASE NOTE The main program.
	DO COME FROM (8201)
	DO .2 <- #0
	DO (8) NEXT
(9)  	DO GET CONTINUATION IN .1 GETTING .2
	DO (10) NEXT
	DO (7) NEXT
(8)	DO (9) NEXT
(7)	DO FORGET #1
	PLEASE DO READ OUT .2
	DO CONTINUE WITH .1 SENDING #3 ONCE
	DO CONTINUE WITH .1 SENDING #4 ONCE
	DO CONTINUE WITH .1 SENDING #5 ONCE
	DO CONTINUE WITH .1 SENDING #6 ONCE
	DO CONTINUE WITH .1 SENDING #7 ONCE
	DO .2 <- .1 ONCE
	DO (11) NEXT
(12)  	DO GET CONTINUATION IN .3 GETTING .2
	DO READ OUT #31 + .1 + .2 + .3
	DO CONTINUE WITH .3 SENDING .2
(11)  	DO (12) NEXT
	PLEASE DO READ OUT #30 + .1 + .2 + .3
	PLEASE DO CONTINUE WITH .2 SENDING .3
	PLEASE KILL ALL THREADS

(10)	DO .2 <- #1
	DO READ OUT #10
	DO CONTINUE WITH .1 SENDING #2
        DO RESUME #1
