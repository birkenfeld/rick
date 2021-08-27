# -------------------------------------------------------------------------------------------------
# Rick, a Rust intercal compiler.  Save your souls!
#
# Copyright (c) 2015-2021 Georg Brandl
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
import time
import difflib
from os import path
from subprocess import Popen, PIPE, STDOUT

already_compiled = set()


def run_test(testname, testcode, compiled):
    stdin = b''
    if path.isfile(testname + '.tst'):
        with open(testname + '.tst', 'rb') as stdinfile:
            stdin = stdinfile.read()
    with open(testname + '.chk', 'r') as stdoutfile:
        stdout = stdoutfile.read()

    def check(proc, remove_cargo):
        real_stdout, _ = proc.communicate(stdin)
        real_stdout = real_stdout.decode()
        # remove cargo's "Running" line
        if remove_cargo:
            errindex = real_stdout.find('An unknown error occurred')
            if errindex == -1:
                errindex = real_stdout.find('error: Process didn\'t exit successfully')
            if errindex > -1:
                real_stdout = real_stdout[:errindex]
        if real_stdout != stdout:
            print('*** ERROR: standard output does not match check file')
            print(''.join(difflib.unified_diff(stdout.splitlines(True),
                                               real_stdout.splitlines(True))))
            raise RuntimeError

    print('')
    print('>>> Test: ' + testname)
    print('  > Step 1: interpreted')
    check(Popen(['cargo', 'run', '--release', '-q', '--', '-Rbi', testcode],
                stdin=PIPE, stdout=PIPE, stderr=STDOUT), True)

    print('  > Step 2: interpreted + optimized')
    check(Popen(['cargo', 'run', '--release', '-q', '--', '-Rbio', testcode],
                stdin=PIPE, stdout=PIPE, stderr=STDOUT), True)

    if compiled:
        print('  > Step 3: compiled + optimized')
        if testcode not in already_compiled:
            if os.system('cargo run --release -q -- -RFbo %s > /dev/null' % testcode) != 0:
                print('*** ERROR: compilation failed')
                raise RuntimeError
            already_compiled.add(testcode)
        check(Popen([testcode[:-2]], stdin=PIPE, stdout=PIPE, stderr=STDOUT),
              False)


def main():
    start = time.time()
    compile_flag = '--nocompile' not in sys.argv
    skip_flag = '--all' not in sys.argv
    tests = [path.splitext(test.replace('/', os.sep))[0]
             for test in sys.argv[1:] if not test.startswith('-')]
    print('Building...')
    if os.system('cargo build --release') != 0:
        return 2
    print('Running tests, please wait...')
    passed = 0
    total = 0
    failed = []
    for root, dirs, files in os.walk('code'):
        dirs.sort()
        for fn in sorted(files):
            if not fn.endswith('.chk'):
                continue
            if skip_flag and fn.startswith(('fft-', 'flonck', 'unlambda')):
                continue
            testname = path.join(root, fn)[:-4]
            if tests and testname not in tests:
                continue
            testcode = testname + '.i'
            # special case
            if fn.startswith('fft-'):
                testcode = path.join(root, 'fft.i')
            elif fn.startswith('life-'):
                testcode = path.join(root, 'life2.i')
            if not path.isfile(testcode):
                print('')
                print('*** WARNING: found %s.chk, but not %s' % (testname, testcode))
                continue
            total += 1
            try:
                t1 = time.time()
                run_test(testname, testcode, compile_flag)
                t2 = time.time()
                passed += 1
                print('--- passed  (%5.2f sec)' % (t2 - t1))
            except RuntimeError:
                failed.append(testname)
    end = time.time()
    print('')
    print('RESULT: %d/%d tests passed  (%6.2f sec)' % (passed, total, end - start))
    if failed:
        print('Failed:')
        for testname in failed:
            print('    ' + testname)
    return 0 if passed == total else 1


if __name__ == '__main__':
    sys.exit(main())
