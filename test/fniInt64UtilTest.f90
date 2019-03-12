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
program fniInt64UtilTest
    implicit none
    logical  ::  error
    
    call int64ToBinaryAsUTest(error)
    if ( error ) stop 1
    call int64ToBinaryTest(error)
    if ( error ) stop 1
    call int64ToOctalAsUTest(error)
    if ( error ) stop 1
    call int64ToOctalTest(error)
    if ( error ) stop 1
    call int64ToHexAsUTest(error)
    if ( error ) stop 1
    call int64ToHexTest(error)
    if ( error ) stop 1
end program

subroutine int64ToBinaryAsUTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToBinaryAsUTest" 
    character(len=64), dimension(13)  ::  expected
    character(len=64)                 ::  str
    integer(k_int64) , dimension(13)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                                  "                                                               0"
    tCase(2) = 1 ; expected(2) =                                  "                                                               1"
    tCase(3) = 10874 ; expected(3) =                              "                                                  10101001111010"
    tCase(4) = 4294967296_k_int64 ; expected(4) =                 "                               100000000000000000000000000000000"
    tCase(5) = 4294967294_k_int64 ; expected(5) =                 "                                11111111111111111111111111111110"
    tCase(6) = 754218745985_k_int64 ; expected(6) =               "                        1010111110011010111100001100000010000001"
    tCase(7) = -58748745821_k_int64 ; expected(7) =               "1111111111111111111111111111001001010010010011010100011110100011"
    tCase(8) = 9223372036854775807_k_int64 ; expected(8) =        " 111111111111111111111111111111111111111111111111111111111111111"
    tCase(9) = -1 ; expected(9) =                                 "1111111111111111111111111111111111111111111111111111111111111111"
    tCase(10) = -2 ; expected(10) =                               "1111111111111111111111111111111111111111111111111111111111111110"
    tCase(11) = -9223372036854775807_k_int64 - 1 ; expected(11) = "1000000000000000000000000000000000000000000000000000000000000000"
    tCase(12) = -9223372036854775807_k_int64 ; expected(12) =     "1000000000000000000000000000000000000000000000000000000000000001"
    tCase(13) = 9141386507638288912_k_int64 ; expected(13) =      " 111111011011100101110101001100001110110010101000011001000010000"
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToBinaryAsU(tCase(i))
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

subroutine int64ToBinaryTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToBinaryTest" 
    character(len=65), dimension(13)  ::  expected
    character(len=65)                 ::  str
    integer(k_int64) , dimension(13)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) = &
    "                                                                0"
    tCase(2) = 1 ; expected(2) = &
    "                                                                1"
    tCase(3) = 4294967296_k_int64 ; expected(3) = &
    "                                100000000000000000000000000000000"
    tCase(4) = 85874487412_k_int64 ; expected(4) = &
    "                            1001111111110100001001011000001110100"
    tCase(5) = 9223372036854775807_k_int64 ; expected(5) = &
    "  111111111111111111111111111111111111111111111111111111111111111"
    tCase(6) = 9141386507638288912_k_int64 ; expected(6) = &
    "  111111011011100101110101001100001110110010101000011001000010000"
    tCase(7) = -1 ; expected(7) = &
    "                                                               -1"
    tCase(8) = -2 ; expected(8) = &
    "                                                              -10"
    tCase(9) = -4294967294_k_int64 ; expected(9) = &
    "                                -11111111111111111111111111111110"
    tCase(10) = -9874542478_k_int64 ; expected(10) = &
    "                              -1001001100100100011000111110001110"
    tCase(11) = -587412577777_k_int64 ; expected(11) = &
    "                        -1000100011000100100001001001110111110001"
    tCase(12) = -9223372036854775807_k_int64 ; expected(12) = &
    " -111111111111111111111111111111111111111111111111111111111111111"
    tCase(13) = -9223372036854775807_k_int64 - 1 ; expected(13) = &
    "-1000000000000000000000000000000000000000000000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToBinary(tCase(i))
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

subroutine int64ToOctalAsUTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToOctalAsUTest" 
    character(len=22), dimension(10)  ::  expected
    character(len=22)                 ::  str
    integer(k_int64) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                                "                     0"
    tCase(2) = 1 ; expected(2) =                                "                     1"
    tCase(3) = 10874 ; expected(3) =                            "                 25172"
    tCase(4) = 754218745985_k_int64 ; expected(4) =             "        12763274140201"
    tCase(5) = -58748745821_k_int64 ; expected(5) =             "1777777777112223243643"
    tCase(6) = 9223372036854775807_k_int64 ; expected(6) =      " 777777777777777777777"
    tCase(7) = -1 ; expected(7) =                               "1777777777777777777777"
    tCase(8) = -2 ; expected(8) =                               "1777777777777777777776"
    tCase(9) = -9223372036854775807_k_int64 - 1 ; expected(9) = "1000000000000000000000"
    tCase(10) = -9223372036854775807_k_int64 ; expected(10) =   "1000000000000000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToOctalAsU(tCase(i))
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

subroutine int64ToOctalTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToOctalTest" 
    character(len=23), dimension(10)  ::  expected
    character(len=23)                 ::  str
    integer(k_int64) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                                   "                      0"
    tCase(2) = 1 ; expected(2) =                                   "                      1"
    tCase(3) = 85874487412_k_int64 ; expected(3) =                 "          1177641130164"
    tCase(4) = 9223372036854775807_k_int64 ; expected(4) =         "  777777777777777777777"
    tCase(5) = -1 ; expected(5) =                                  "                     -1"
    tCase(6) = -2 ; expected(6) =                                  "                     -2"
    tCase(7) = -9874542478_k_int64 ; expected(7) =                 "          -111444307616"
    tCase(8) = -587412577777_k_int64 ; expected(8) =               "        -10430441116761"
    tCase(9) = -9223372036854775807_k_int64 ; expected(9) =        " -777777777777777777777"
    tCase(10) = -9223372036854775807_k_int64 - 1 ; expected(10) =  "-1000000000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToOctal(tCase(i))
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

subroutine int64ToHexAsUTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToHexAsUTest" 
    character(len=16), dimension(10)  ::  expected
    character(len=16)                 ::  str
    integer(k_int64) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                                "               0"
    tCase(2) = 1 ; expected(2) =                                "               1"
    tCase(3) = 10874 ; expected(3) =                            "            2a7a"
    tCase(4) = 754218745985_k_int64 ; expected(4) =             "      af9af0c081"
    tCase(5) = -58748745821_k_int64 ; expected(5) =             "fffffff2524d47a3"
    tCase(6) = 9223372036854775807_k_int64 ; expected(6) =      "7fffffffffffffff"
    tCase(7) = -1 ; expected(7) =                               "ffffffffffffffff"
    tCase(8) = -2 ; expected(8) =                               "fffffffffffffffe"
    tCase(9) = -9223372036854775807_k_int64 - 1 ; expected(9) = "8000000000000000"
    tCase(10) = -9223372036854775807_k_int64 ; expected(10) =   "8000000000000001"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToHexAsU(tCase(i))
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

subroutine int64ToHexTest(error)
    use fniInt64Util
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "int64ToHexTest" 
    character(len=17), dimension(10)  ::  expected
    character(len=17)                 ::  str
    integer(k_int64) , dimension(10)  ::  tCase
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    tCase(1) = 0 ; expected(1) =                                   "                0"
    tCase(2) = 1 ; expected(2) =                                   "                1"
    tCase(3) = 85874487412_k_int64 ; expected(3) =                 "       13fe84b074"
    tCase(4) = 9223372036854775807_k_int64 ; expected(4) =         " 7fffffffffffffff"
    tCase(5) = -1 ; expected(5) =                                  "               -1"
    tCase(6) = -2 ; expected(6) =                                  "               -2"
    tCase(7) = -9874542478_k_int64 ; expected(7) =                 "       -24c918f8e"
    tCase(8) = -587412577777_k_int64 ; expected(8) =               "      -88c4849df1"
    tCase(9) = -9223372036854775807_k_int64 ; expected(9) =        "-7fffffffffffffff"
    tCase(10) = -9223372036854775807_k_int64 - 1 ; expected(10) =  "-8000000000000000"
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
 
    i = 1
    do while ( i <= 10 ) 
        print *, "Test case '", i, "'. Test value '", tCase(i), "'."

        str = int64ToHex(tCase(i))
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
