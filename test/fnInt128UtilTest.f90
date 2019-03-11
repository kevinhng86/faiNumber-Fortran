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
program fnInt128UtilTest
    implicit none
    logical  ::  error
    
    call int128ToBinaryAsUTest(error)
    if ( error ) stop 1
    call int128ToBinaryTest(error)
    if ( error ) stop 1
    call int128ToOctalAsUTest(error)
    if ( error ) stop 1
    call int128ToOctalTest(error)
    if ( error ) stop 1
    call int128ToHexAsUTest(error)
    if ( error ) stop 1
    call int128ToHexTest(error)
    if ( error ) stop 1
end program

subroutine int128ToBinaryAsUTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)     ::  error
    character(len=2) , parameter       ::  lf = char(10) // char(13)
    character(len=*) , parameter       ::  testName = "int128ToBinaryAsUTest" 
    character(len=128), dimension(10)  ::  expected
    character(len=128)                 ::  str
    integer(k_int128) , dimension(10)  ::  tCase
    integer(k_int32)                   ::  i
    error = .TRUE.
    
    tCase(1) = 0_k_int128 
    expected(1)  = "                                                    " &
               //  "                                                                           0"
    tCase(2) = 1_k_int128
    expected(2)  = "                                                    " & 
               //  "                                                                           1"
    tCase(3) = 587943_k_int128
    expected(3)  = "                                                    " &
               //  "                                                        10001111100010100111"
    tCase(4) = 231731687303715884105727_k_int128
    expected(4)  = "                                                  11" & 
               //  "0001000100100011001100101111010010111010000101101010111111111111111111111111"
    tCase(5) = -587487458214742874214_k_int128
    expected(5)  = "1111111111111111111111111111111111111111111111111111" & 
               //  "1111111000000010011011111001010001010110010101001001000110111010011110011010"
    tCase(6) = 170141183460469231731687303715884105727_k_int128
    expected(6)  = " 111111111111111111111111111111111111111111111111111" & 
               //  "1111111111111111111111111111111111111111111111111111111111111111111111111111"
    tCase(7) = -1_k_int128 
    expected(7)  = "1111111111111111111111111111111111111111111111111111" & 
               // "1111111111111111111111111111111111111111111111111111111111111111111111111111"
    tCase(8) = -2_k_int128
    expected(8)  = "1111111111111111111111111111111111111111111111111111" & 
               //  "1111111111111111111111111111111111111111111111111111111111111111111111111110"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128
    expected(9)  = "1000000000000000000000000000000000000000000000000000" &
               //  "0000000000000000000000000000000000000000000000000000000000000000000000000000"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 
    expected(10) = "1000000000000000000000000000000000000000000000000000" & 
               //  "0000000000000000000000000000000000000000000000000000000000000000000000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToBinaryAsU(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128ToBinaryTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)      ::  error
    character(len=2) , parameter        ::  lf = char(10) // char(13)
    character(len=*) , parameter        ::  testName = "int128ToBinaryTest" 
    character(len=129), dimension(10)   ::  expected
    character(len=129)                  ::  str
    integer(k_int128) , dimension(10)   ::  tCase
    integer(k_int32)                    ::  i
    error = .TRUE.
    
    tCase(1) = 0_k_int128 
    expected(1) =  "                                                    " &
               //  "                                                                            0"
    tCase(2) = 1_k_int128 
    expected(2) =  "                                                    " &
               //  "                                                                            1"
    tCase(3) = 231731687303715884105727_k_int128
    expected(3) =  "                                                   1" &
               //  "10001000100100011001100101111010010111010000101101010111111111111111111111111"    
    tCase(4) = 170141183460469231731687303715884105727_k_int128
    expected(4) =  "  11111111111111111111111111111111111111111111111111" &
               //  "11111111111111111111111111111111111111111111111111111111111111111111111111111"
    tCase(5) = -1_k_int128
    expected(5) =  "                                                    " &
               //  "                                                                           -1"
    tCase(6) = -2_k_int128 
    expected(6) =  "                                                    " & 
               //  "                                                                          -10"
    tCase(7) = -587487458214742874214_k_int128
    expected(7) =  "                                                    " &
               //  "       -111111101100100000110101110101001101010110110111001000101100001100110"
    tCase(8) = -74872014874254748795411_k_int128
    expected(8) =  "                                                    " &
               //  "-1111110110101101000111101111011001110011001011010101011110011100001000010011"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 
    expected(9) =  " -11111111111111111111111111111111111111111111111111" & 
               //  "11111111111111111111111111111111111111111111111111111111111111111111111111111"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128 
    expected(10) = "-100000000000000000000000000000000000000000000000000" &
               //  "00000000000000000000000000000000000000000000000000000000000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToBinary(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128ToOctalAsUTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)     ::  error
    character(len=2) , parameter       ::  lf = char(10) // char(13)
    character(len=*) , parameter       ::  testName = "int128ToOctalAsUTest" 
    character(len=43), dimension(10)   ::  expected
    character(len=43)                  ::  str
    integer(k_int128) , dimension(10)  ::  tCase
    integer(k_int32)                   ::  i
    error = .TRUE.
    tCase(1) = 0_k_int128 
    expected(1)  = "                                          0"
    tCase(2) = 1_k_int128
    expected(2)  = "                                          1"
    tCase(3) = 587943_k_int128
    expected(3)  = "                                    2174247"
    tCase(4) = 231731687303715884105727_k_int128
    expected(4)  = "                 61044314572272055277777777" 
    tCase(5) = -587487458214742874214_k_int128
    expected(5)  = "3777777777777777777700233712126251106723632"
    tCase(6) = 170141183460469231731687303715884105727_k_int128
    expected(6)  = "1777777777777777777777777777777777777777777"
    tCase(7) = -1_k_int128 
    expected(7)  = "3777777777777777777777777777777777777777777"
    tCase(8) = -2_k_int128
    expected(8)  = "3777777777777777777777777777777777777777776"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128
    expected(9)  = "2000000000000000000000000000000000000000000"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 
    expected(10) = "2000000000000000000000000000000000000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToOctalAsU(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128ToOctalTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)      ::  error
    character(len=2) , parameter        ::  lf = char(10) // char(13)
    character(len=*) , parameter        ::  testName = "int128ToOctalTest" 
    character(len=44), dimension(10)    ::  expected
    character(len=44)                   ::  str
    integer(k_int128), dimension(10)    ::  tCase
    integer(k_int32)                    ::  i
    error = .TRUE.
    
    tCase(1) = 0_k_int128 
    expected(1) =  "                                           0"
    tCase(2) = 1_k_int128 
    expected(2) =  "                                           1"
    tCase(3) = 231731687303715884105727_k_int128
    expected(3) =  "                  61044314572272055277777777"
    tCase(4) = 170141183460469231731687303715884105727_k_int128
    expected(4) =  " 1777777777777777777777777777777777777777777"
    tCase(5) = -1_k_int128
    expected(5) =  "                                          -1"
    tCase(6) = -2_k_int128 
    expected(6) =  "                                          -2"
    tCase(7) = -587487458214742874214_k_int128
    expected(7) =  "                    -77544065651526671054146"
    tCase(8) = -74872014874254748795411_k_int128
    expected(8) =  "                 -17665507573163132536341023"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 
    expected(9) =  "-1777777777777777777777777777777777777777777"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128 
    expected(10) = "-2000000000000000000000000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToOctal(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128ToHexAsUTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)     ::  error
    character(len=2) , parameter       ::  lf = char(10) // char(13)
    character(len=*) , parameter       ::  testName = "int128ToHexAsUTest" 
    character(len=32), dimension(10)   ::  expected
    character(len=32)                  ::  str
    integer(k_int128) , dimension(10)  ::  tCase
    integer(k_int32)                   ::  i
    error = .TRUE.
    
    tCase(1) = 0_k_int128 
    expected(1)  = "                               0"
    tCase(2) = 1_k_int128
    expected(2)  = "                               1"
    tCase(3) = 587943_k_int128
    expected(3)  = "                           8f8a7"
    tCase(4) = 231731687303715884105727_k_int128
    expected(4)  = "            3112332f4ba16affffff"
    tCase(5) = -587487458214742874214_k_int128
    expected(5)  = "ffffffffffffffe026f94565491ba79a"
    tCase(6) = 170141183460469231731687303715884105727_k_int128
    expected(6)  = "7fffffffffffffffffffffffffffffff"
    tCase(7) = -1_k_int128
    expected(7)  = "ffffffffffffffffffffffffffffffff"
    tCase(8) = -2_k_int128
    expected(8)  = "fffffffffffffffffffffffffffffffe"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128
    expected(9)  = "80000000000000000000000000000000"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 
    expected(10) = "80000000000000000000000000000001"

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToHexAsU(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine int128ToHexTest(error)
    use fnInt128Util
    implicit none
    logical          , intent(out)      ::  error
    character(len=2) , parameter        ::  lf = char(10) // char(13)
    character(len=*) , parameter        ::  testName = "int128ToHexTest" 
    character(len=33), dimension(10)    ::  expected
    character(len=33)                   ::  str
    integer(k_int128) , dimension(10)   ::  tCase
    integer(k_int32)                    ::  i
    error = .TRUE.
    
    tCase(1) = 0_k_int128 
    expected(1) =  "                                0"
    tCase(2) = 1_k_int128 
    expected(2) =  "                                1"
    tCase(3) = 231731687303715884105727_k_int128
    expected(3) =  "             3112332f4ba16affffff"
    tCase(4) = 170141183460469231731687303715884105727_k_int128
    expected(4) =  " 7fffffffffffffffffffffffffffffff"
    tCase(5) = -1_k_int128
    expected(5) =  "                               -1"
    tCase(6) = -2_k_int128 
    expected(6) =  "                               -2"
    tCase(7) = -587487458214742874214_k_int128
    expected(7) =  "              -1fd906ba9ab6e45866"
    tCase(8) = -74872014874254748795411_k_int128
    expected(8) =  "             -fdad1ef6732d579c213"
    tCase(9) = -170141183460469231731687303715884105727_k_int128 
    expected(9) =  "-7fffffffffffffffffffffffffffffff"
    tCase(10) = -170141183460469231731687303715884105727_k_int128 - 1_k_int128 
    expected(10) = "-80000000000000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int128ToHex(tCase(i))
        if ( str /= expected(i) ) then
            print *, "---Unexpected result. Got '", str, "'."
            print *, "---Expected '", expected(i), "'."
            return
        end if
        
        print *, "+++Got expected '", str, "'."
        print *, lf
        i = i + 1
    end do
    print *, lf, lf
   
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

pure subroutine fnInt128UtilTestPure()
    use fnInt128Util
    implicit none
    character(len=129)  ::  str
 
    str = int128ToBinaryAsU(0_k_int128)
    str = int128ToBinary(0_k_int128)
    str = int128ToOctalAsU(0_k_int128)
    str = int128ToOctal(0_k_int128)
    str = int128ToHexAsU(0_k_int128)
    str = int128ToHex(0_k_int128)
end subroutine
