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
program fnInt32UtilTest
    use fnConsts
    implicit none
    logical  ::  error
    
    call int32ToBinaryAsUTest(error)
    if ( error ) stop
    call int32ToBinaryTest(error)
    if ( error ) stop
    call int32ToOctalAsUTest(error)
    if ( error ) stop
    call int32ToOctalTest(error)
    if ( error ) stop
    call int32ToHexAsUTest(error)
    if ( error ) stop
    call int32ToHexTest(error)
    if ( error ) stop
end program

subroutine int32ToBinaryAsUTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToBinaryAsUTest" 
    character(len=32), dimension(10)  ::  expected
    character(len=32)                 ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =               "                               0"
    tCase(2) = 1 ; expected(2) =               "                               1"
    tCase(3) = 10 ; expected(3) =              "                            1010"
    tCase(4) = 3548 ; expected(4) =            "                    110111011100"
    tCase(5) = -58585 ; expected(5) =          "11111111111111110001101100100111"
    tCase(6) = 2147483647 ; expected(6) =      " 1111111111111111111111111111111"
    tCase(7) = -1 ; expected(7) =              "11111111111111111111111111111111"
    tCase(8) = -2 ; expected(8) =              "11111111111111111111111111111110"
    tCase(9) = -2147483647 - 1 ; expected(9) = "10000000000000000000000000000000"
    tCase(10) = -2147483647 ; expected(10) =   "10000000000000000000000000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToBinaryAsU(tCase(i))
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

subroutine int32ToBinaryTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToBinaryTest" 
    character(len=33), dimension(10)  ::  expected
    character(len=33)                 ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                 "                                0"
    tCase(2) = 1 ; expected(2) =                 "                                1"
    tCase(3) = 3548 ; expected(3) =              "                     110111011100"
    tCase(4) = 2147483647 ; expected(4) =        "  1111111111111111111111111111111"
    tCase(5) = -1 ; expected(5) =                "                               -1"
    tCase(6) = -2 ; expected(6) =                "                              -10"
    tCase(7) = -10 ; expected(7) =               "                            -1010"
    tCase(8) = -58585 ; expected(8) =            "                -1110010011011001"
    tCase(9) = -2147483647 ; expected(9) =       " -1111111111111111111111111111111"
    tCase(10) = -2147483647 - 1 ; expected(10) = "-10000000000000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToBinary(tCase(i))
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

subroutine int32ToOctalAsUTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToOctalAsUTest" 
    character(len=11), dimension(10)  ::  expected
    character(len=11)                 ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =               "          0"
    tCase(2) = 1 ; expected(2) =               "          1"
    tCase(3) = 10 ; expected(3) =              "         12"
    tCase(4) = 3548 ; expected(4) =            "       6734"
    tCase(5) = -58585 ; expected(5) =          "37777615447"
    tCase(6) = 2147483647 ; expected(6) =      "17777777777"
    tCase(7) = -1 ; expected(7) =              "37777777777"
    tCase(8) = -2 ; expected(8) =              "37777777776"
    tCase(9) = -2147483647 - 1 ; expected(9) = "20000000000"
    tCase(10) = -2147483647 ; expected(10) =   "20000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToOctalAsU(tCase(i))
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

subroutine int32ToOctalTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToOctalTest" 
    character(len=12), dimension(10)  ::  expected
    character(len=12)                 ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                 "           0"
    tCase(2) = 1 ; expected(2) =                 "           1"
    tCase(3) = 3548 ; expected(3) =              "        6734"
    tCase(4) = 2147483647 ; expected(4) =        " 17777777777"
    tCase(5) = -1 ; expected(5) =                "          -1"
    tCase(6) = -2 ; expected(6) =                "          -2"
    tCase(7) = -10 ; expected(7) =               "         -12"
    tCase(8) = -58585 ; expected(8) =            "     -162331"
    tCase(9) = -2147483647 ; expected(9) =       "-17777777777"
    tCase(10) = -2147483647 - 1 ; expected(10) = "-20000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToOctal(tCase(i))
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

subroutine int32ToHexAsUTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToHexAsUTest" 
    character(len=8), dimension(10)   ::  expected
    character(len=8)                  ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =               "       0"
    tCase(2) = 1 ; expected(2) =               "       1"
    tCase(3) = 10 ; expected(3) =              "       a"
    tCase(4) = 3548 ; expected(4) =            "     ddc"
    tCase(5) = -58585 ; expected(5) =          "ffff1b27"
    tCase(6) = 2147483647 ; expected(6) =      "7fffffff"
    tCase(7) = -1 ; expected(7) =              "ffffffff"
    tCase(8) = -2 ; expected(8) =              "fffffffe"
    tCase(9) = -2147483647 - 1 ; expected(9) = "80000000"
    tCase(10) = -2147483647 ; expected(10) =   "80000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToHexAsU(tCase(i))
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

subroutine int32ToHexTest(error)
    use fnInt32Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int32ToHexTest" 
    character(len=9), dimension(10)  ::  expected
    character(len=9)                 ::  str
    integer(k_int32) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                 "        0"
    tCase(2) = 1 ; expected(2) =                 "        1"
    tCase(3) = 3548 ; expected(3) =              "      ddc"
    tCase(4) = 2147483647 ; expected(4) =        " 7fffffff"
    tCase(5) = -1 ; expected(5) =                "       -1"
    tCase(6) = -2 ; expected(6) =                "       -2"
    tCase(7) = -10 ; expected(7) =               "       -a"
    tCase(8) = -58585 ; expected(8) =            "    -e4d9"
    tCase(9) = -2147483647 ; expected(9) =       "-7fffffff"
    tCase(10) = -2147483647 - 1 ; expected(10) = "-80000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int32ToHex(tCase(i))
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

pure subroutine fnInt32UtilTestPure()
    use fnInt32Util
    implicit none
    character(len=33)  ::  str
 
    str = int32ToBinaryAsU(0)
    str = int32ToBinary(0)
    str = int32ToOctalAsU(0)
    str = int32ToOctal(0)
    str = int32ToHexAsU(0)
    str = int32ToHex(0)
end subroutine
