# -------------------------------------------------------------------------------------------------
# Rick, a Rust intercal compiler.  Save your souls!
#
# Copyright (c) 2015 Georg Brandl
#
# This program is free software; you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program;
# if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# -------------------------------------------------------------------------------------------------

import os
import sys
import difflib
from os import path
from subprocess import Popen, PIPE


def run_test(testname, testcode, compiled):
    stdin = b''
    if path.isfile(testname + '.tst'):
        with open(testname + '.tst', 'rb') as stdinfile:
            stdin = stdinfile.read()
    with open(testname + '.chk', 'rb') as stdoutfile:
        stdout = stdoutfile.read()

    def check(proc):
        real_stdout, _ = proc.communicate(stdin)
        if proc.returncode != 0:
            print('*** ERROR: process returned status %d' % proc.returncode)
            raise RuntimeError
        # remove cargo's "Running" line
        real_stdout = real_stdout[real_stdout.index('\n') + 1:]
        if real_stdout != stdout:
            print('*** ERROR: standard output does not match check file')
            print(''.join(difflib.unified_diff(stdout.splitlines(True),
                                               real_stdout.splitlines(True))))
            raise RuntimeError

    print('')
    print('>>> Test: ' + testname)
    print('  > Step 1: interpreted')
    check(Popen(['cargo', 'run', '--', '-i', testcode],
                stdin=PIPE, stdout=PIPE))

    print('  > Step 2: interpreted + optimized')
    check(Popen(['cargo', 'run', '--', '-io', testcode],
                stdin=PIPE, stdout=PIPE))

    if compiled:
        print('  > Step 3: compiled + optimized')
        if os.system('cargo run -- -o %s > /dev/null' % testcode) != 0:
            print('*** ERROR: compilation failed')
            raise RuntimeError
        check(Popen([testcode[:-2]], stdin=PIPE, stdout=PIPE))

    print ('  - passed')


def main():
    long_flag = '--long' in sys.argv
    print('Building...')
    if os.system('cargo build') != 0:
        return 2
    print('Running tests, please wait...')
    passed = 0
    total = 0
    for root, dirs, files in os.walk('code'):
        dirs.sort()
        for fn in sorted(files):
            if not fn.endswith('.chk'):
                continue
            testname = path.join(root, fn)[:-4]
            testcode = testname + '.i'
            # special case
            if fn.startswith('fft-'):
                testcode = path.join(root, 'fft.i')
            if not path.isfile(testcode):
                print('')
                print('*** WARNING: found %s.chk, but not %s' % (testname, testcode))
                continue
            total += 1
            try:
                run_test(testname, testcode, long_flag)
                passed += 1
            except RuntimeError:
                pass
    print('')
    print('RESULT: %d/%d tests passed' % (passed, total))
    return 0 if passed == total else 1


if __name__ == '__main__':
    sys.exit(main())
