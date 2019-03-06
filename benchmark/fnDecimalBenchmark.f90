! Copyright 2019 Khang Hoang Nguyen
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files
! (the "Software"), to deal in the Software without restriction,
! including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software,
! and to permit persons to whom the Software is furnished to do so,
! subject to the following conditions
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

program fnDecimalBenchmark
    implicit none
    call int32
    call int64
    call int128    
end program

subroutine int32()
    use fnDecimalUtil
    implicit none
    integer(k_int32)             ::  i, errorInt, resultValue, bCase
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=11)            ::  str
    character(len=50)            ::  str2
    character(len=50)            ::  str3
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "-2147483648"
    str2 = "                    -2147483648"
    str3 = "-2147483648"

    print *, "##### fnDecimalUtil and read() benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt32(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start


    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt32TrueError(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 2, parse number with space on both side. String " ,str2, " with len " , len(str2)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str2, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt32(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt32TrueError(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 3, parse number with space on right side. String " ,str3, " with len " , len(str3)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str3, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt32(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt32TrueError(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt32TrueError benchmark = ",f6.3," seconds.")',finish-start
    print *, lf, lf, lf
    print *, "##### fnDecimalUtil and read() benchmark. #####"
    print *, lf, lf, lf
end subroutine

subroutine int64()
    use fnDecimalUtil64
    implicit none
    integer(k_int32)             ::  i, errorInt, bCase
    integer(k_int64)             ::  resultValue
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=20)            ::  str
    character(len=100)           ::  str2
    character(len=100)           ::  str3
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "-9223372036854775808"
    str2 = "                    -9223372036854775808"
    str3 = "-9223372036854775808"

    print *, "##### fnDecimalUtil64 and read() benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt64(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt64TrueError(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 2, parse number with space on both side. String " ,str2, " with len " , len(str2)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str2, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt64(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt64TrueError(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 3, parse number with space on right side. String " ,str3, " with len " , len(str3)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str3, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt64(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt64TrueError(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt64TrueError benchmark = ",f6.3," seconds.")',finish-start
    print *, lf, lf, lf
    print *, "##### end fnDecimalUtil64 and read() benchmark. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128()
    use fnDecimalUtil128
    implicit none
    integer(k_int32)             ::  i, errorInt, bCase
    integer(k_int128)            ::  resultValue
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=40)            ::  str
    character(len=100)           ::  str2
    character(len=100)           ::  str3
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "-170141183460469231731687303715884105728"
    str2 = "                    -170141183460469231731687303715884105728"
    str3 = "-170141183460469231731687303715884105728"

    print *, "##### fnDecimalUtil128 and read() benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt128(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt128TrueError(str, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 2, parse number with space on both side. String " ,str2, " with len " , len(str2)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str2, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt128(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt128TrueError(str2, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128TrueError benchmark = ",f6.3," seconds.")',finish-start

    print *, lf, lf, lf
    print *, "# Case 3, parse number with space on right side. String " ,str3, " with len " , len(str3)
    print *, "# Each method will run ", bCase, " times.", lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            read (str3, *) resultValue
            i = i + 1
        end do
    call cpu_time(finish)
    print '("read() benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            error = decToInt128(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            errorInt = decToInt128TrueError(str3, resultValue)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("dectoInt128TrueError benchmark = ",f6.3," seconds.")',finish-start
    print *, lf, lf, lf
    print *, "##### end fnDecimalUtil128 and read() benchmark. #####"
    print *, lf, lf, lf
end subroutine
