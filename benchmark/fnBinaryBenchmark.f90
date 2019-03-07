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

program fnBinaryBenchmark
    implicit none
    call int32
    call int64
    call int128    
end program

subroutine int32()
    use fnBinaryUtil
    implicit none
    integer(k_int32)             ::  i, errorInt, resultValue, bCase
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=32)            ::  str
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "111111111111111111111111111111"

    print *, "##### fnBinaryUil benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf
    print *, lf
    
    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt32(str, resultValue, error)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("binaryToInt32 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt32TrueError(str, resultValue, errorInt)
            i = i + 1
        end do
    call cpu_time(finish)
    print '("binaryToInt32TrueError benchmark = ",f6.3," seconds.")',finish-start
    
    print *, lf
    print *, "##### End fnBinaryUil benchmark. #####"
    print *, lf, lf, lf
end subroutine

subroutine int64()
    use fnBinaryUtil64
    implicit none
    integer(k_int32)             ::  i, errorInt, bCase
    integer(k_int64)             ::  resultValue
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=64)            ::  str
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "111111111111111111111111111111111111111111111111111111111111"

    print *, "##### fnBinaryUtil64 benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf
    print *, lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt64(str, resultValue, error)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("binaryToInt64 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt64TrueError(str, resultValue, errorInt)
            i = i + 1
        end do
    call cpu_time(finish)
    print '("binaryToInt64TrueError benchmark = ",f6.3," seconds.")',finish-start
    
    print *, lf
    print *, "##### End fnBinaryUtil64 benchmark. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128()
    use fnBinaryUtil128
    implicit none
    integer(k_int32)             ::  i, errorInt, bCase
    integer(k_int128)            ::  resultValue
    character(len=2), parameter  ::  lf = char(10) // char(13)
    character(len=128)           ::  str
    logical                      ::  error
    real                         ::  start, finish
    resultValue = 0
    bCase = 10000000
    str = "111111111111111111111111111111111111111111111111111111111111"

    print *, "##### fnBinaryUtil128 benchmark. #####"
    print *, "# Case 1, parse number with exact string length. String " ,str, " with len " , len(str)
    print *, "# Each method will run ", bCase, " times.", lf
    print *, lf

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt128(str, resultValue, error)
            i = i + 1
        end do 
    call cpu_time(finish)
    print '("binaryToInt128 benchmark = ",f6.3," seconds.")',finish-start

    i = 0
    call cpu_time(start)
        do while ( i < bCase )
            call binaryToInt128TrueError(str, resultValue, errorInt)
            i = i + 1
        end do
    call cpu_time(finish)
    print '("binaryToInt128TrueError benchmark = ",f6.3," seconds.")',finish-start
    
    print *, lf
    print *, "##### End fnBinaryUtil128 benchmark. #####"
    print *, lf, lf, lf
end subroutine


