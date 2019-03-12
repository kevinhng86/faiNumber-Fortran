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
program fniNumberStringUtilTest
    implicit none
    logical  ::  error

    call isIntegerTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isUnsignedIntegerTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isBinaryTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isUnsignedBinaryTest(error);
    if ( error .eqv. .TRUE. ) stop 1
    call isOctalTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isUnsignedOctalTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isHexTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isUnsignedHexTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isBaseTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call isUnsignedBaseTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call assumeIsOddTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call assumeIsEvenTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call assumeCompareTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call assumeCompareAllBaseTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call fnStringCompareTest(error)
    if ( error .eqv. .TRUE. ) stop 1
    call fnStringCompareAsBaseTest(error)
    if ( error .eqv. .TRUE. ) stop 1
end program

subroutine isIntegerTest(error)
    use fniNumberStringUtil
    implicit none
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isIntegerTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf 
    
    print *, "# TEST 1: Test true."
    if ( isInteger("00123456789") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "00123456789"
        return
    end if
    if ( isInteger("    00123456789     ") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "    00123456789     "
        return
    end if
    if ( isInteger("+9") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+9"
        return
    end if
    if ( isInteger("-9") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+9"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isInteger(" 1  1 ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " 1  1 "
        return
    end if
    if ( isInteger("/") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isInteger(":") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    if ( isInteger("a") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "a"
        return
    end if
    if ( isInteger(char(0)) .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Empty Signs.
    if ( isInteger("   ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty"
        return
    end if
    if ( isInteger("") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isInteger("+") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+"
        return
    end if
    if ( isInteger("-") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-"
        return
    end if
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isUnsignedIntegerTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isUnsignedIntegerTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf
 
    print *, "# TEST 1: Test true."    
    if ( isUnsignedInteger("0123456789") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "0123456789"
        return
    end if
    if ( isUnsignedInteger("     0123456789      ") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "     0123456789      "
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isUnsignedInteger(" 1 1 ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", " 1 1 "
        return
    end if
    if ( isUnsignedInteger("/") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isUnsignedInteger(":") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", ":"
        return
    end if
    if ( isUnsignedInteger("a") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "a"
        return
    end if
    if ( isUnsignedInteger(char(0)) .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Signs.
    if ( isUnsignedInteger("   ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "Empty"
        return
    end if
    if ( isUnsignedInteger("") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isUnsignedInteger("+0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "+0"
        return
    end if   
    if ( isUnsignedInteger("-0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "-0"
        return
    end if   
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isBinaryTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isBinaryTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf

    print *, "# TEST 1: Test true."
    if ( isBinary("01") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "01"
        return
    end if
    if ( isBinary(" 01  ") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", " 01  "
        return
    end if
    if ( isBinary("+0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+0"
        return
    end if
    if ( isBinary("-0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "-0"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isBinary(" 0  0 ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " 0  0 "
        return
    end if
    if ( isBinary("/") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isBinary("2") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "2"
        return
    end if
    if ( isBinary(char(0)) .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Empty Signs.
    if ( isBinary("") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isBinary("   ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty"
        return
    end if
    if ( isBinary("+") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+"
        return
    end if
    if ( isBinary("-") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-"
        return
    end if
    print *, "# TEST 2 Passed."

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isUnsignedBinaryTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isUnsignedBinaryTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf
 
    print *, "# TEST 1: Test true."    
    if ( isUnsignedBinary("01") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "01"
        return
    end if
    if ( isUnsignedBinary("     01  ") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "     01  "
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isUnsignedBinary(" 1 1 ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", " 1 1 "
        return
    end if
    if ( isUnsignedBinary("/") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isUnsignedBinary("2") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "2"
        return
    end if
    if ( isUnsignedBinary(char(0)) .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Signs.
    if ( isUnsignedBinary("   ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "Empty"
        return
    end if
    if ( isUnsignedBinary("") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isUnsignedBinary("+0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "+0"
        return
    end if   
    if ( isUnsignedBinary("-0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "-0"
        return
    end if   
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isOctalTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isOctalTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf

    print *, "# TEST 1: Test true."
    if ( isOctal("01234567") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "01234567"
        return
    end if
    if ( isOctal(" 01234567  ") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", " 01234567  "
        return
    end if
    if ( isOctal("+0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+0"
        return
    end if
    if ( isOctal("-0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "-0"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isOctal(" 0  0 ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " 0  0 "
        return
    end if
    if ( isOctal("/") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isOctal("8") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "8"
        return
    end if
    if ( isOctal(char(0)) .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Empty Signs.
    if ( isOctal("") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isOctal("   ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty"
        return
    end if
    if ( isOctal("+") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+"
        return
    end if
    if ( isOctal("-") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-"
        return
    end if
    print *, "# TEST 2 Passed."

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isUnsignedOctalTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isUnsignedOctalTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf
 
    print *, "# TEST 1: Test true."    
    if ( isUnsignedOctal("01234567") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "01234567"
        return
    end if
    if ( isUnsignedOctal("     01234567  ") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "     01234567  "
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isUnsignedOctal(" 1 1 ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", " 1 1 "
        return
    end if
    if ( isUnsignedOctal("/") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isUnsignedOctal("8") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "8"
        return
    end if
    if ( isUnsignedOctal(char(0)) .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Signs.
    if ( isUnsignedOctal("   ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "Empty"
        return
    end if
    if ( isUnsignedOctal("") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isUnsignedOctal("+0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "+0"
        return
    end if   
    if ( isUnsignedOctal("-0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "-0"
        return
    end if   
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isHexTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isHexTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf

    print *, "# TEST 1: Test true."
    if ( isHex("0123456789abcdefABCDEF") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0123456789abcdefABCDEF"
        return
    end if
    if ( isHex(" 0123456789abcdefABCDEF  ") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", " 0123456789abcdefABCDEF  "
        return
    end if
    if ( isHex("+0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+0"
        return
    end if
    if ( isHex("-0") .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "-0"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isHex(" 0  0 ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " 0  0 "
        return
    end if
    if ( isHex("/") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isHex(":") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    if ( isHex("@") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "@"
        return
    end if
    if ( isHex("G") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "G"
        return
    end if
    if ( isHex("`") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "`"
        return
    end if
    if ( isHex("g") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "g"
        return
    end if
    if ( isHex(char(0)) .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Empty Signs.
    if ( isHex("") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isHex("   ") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty"
        return
    end if
    if ( isHex("+") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+"
        return
    end if
    if ( isHex("-") .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-"
        return
    end if
    print *, "# TEST 2 Passed."

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isUnsignedHexTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isUnsignedHexTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf
 
    print *, "# TEST 1: Test true."    
    if ( isUnsignedHex("0123456789abcdefABCDEF") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "0123456789abcdefABCDEF"
        return
    end if
    if ( isUnsignedHex("     0123456789abcdefABCDEF  ") .neqv. .TRUE. ) then 
        print *, "Test 1 fail at ", "     0123456789abcdefABCDEF  "
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf , lf

    print *, "# TEST 2: Test false."
    if ( isUnsignedHex(" 1 1 ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", " 1 1 "
        return
    end if
    if ( isUnsignedHex("/") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "/"
        return
    end if
    if ( isUnsignedHex(":") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", ":"
        return
    end if
    if ( isUnsignedHex("@") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "@"
        return
    end if
    if ( isUnsignedHex("G") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "G"
        return
    end if
    if ( isUnsignedHex("`") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "`"
        return
    end if
    if ( isUnsignedHex("g") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "g"
        return
    end if
    if ( isUnsignedHex(char(0)) .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "null char"
        return
    end if
    ! Empty String & Signs.
    if ( isUnsignedHex("   ") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "Empty"
        return
    end if
    if ( isUnsignedHex("") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    if ( isUnsignedHex("+0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "+0"
        return
    end if   
    if ( isUnsignedHex("-0") .neqv. .FALSE. ) then 
        print *, "Test 2 fail at ", "-0"
        return
    end if   
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isBaseTest(error)
    use fniNumberStringUtil
    implicit none
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isBaseTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    logical                        ::  results, ignore
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf
    
    print *, "# TEST 1: Test true."
    ! Base 2
    if ( isBase("01", 2, results) .neqv. .FALSE. ) then
        print *, "Test 1 fail at ", "01"
        return
    else 
        if ( results .neqv. .TRUE. ) then
            print *, "Test 1 fail at ", "01"
            return
        end if
    end if
    ! Base 10
    ignore = isBase("0123456789", 10, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0123456789"
        return
    end if
    ! Base 11
    ignore = isBase("0123456789aA", 11, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0123456789aA"
        return
    end if
    ! Base 36
    if ( isBase("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", 36, results) &
          .neqv. .FALSE. ) then
        print *, "Test 1 fail at ", "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        return
    else 
        if ( results .neqv. .TRUE. ) then
            print *, "Test 1 fail at ", "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
            return
        end if
    end if
    ignore = isBase("  0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ   ", 36, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "  0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ   "
        return
    end if
    ! Signs
    ignore = isBase("+0", 2, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "+0" 
        return
    end if
    ignore = isBase("-0", 2, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf

    print *, "# TEST 2: Test false."
    ! Right before and right after digit/character as digit in ASCII code, also \0.
    ! Base 2
    ignore = isBase("/", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isBase("2", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "2"
        return
    end if
    ! Base 10
    ignore = isBase("/", 10, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "2"
        return
    end if
    ignore = isBase(":", 10, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ! Base 11
    ignore = isBase("/", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isBase(":", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ignore = isBase("@", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "@"
        return
    end if
    ignore = isBase("B", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "B"
        return
    end if
    ignore = isBase("`", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "`"
        return
    end if
    ignore = isBase("b", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "b"
        return
    end if
    ! Base 36
    ignore = isBase("/", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isBase(":", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ignore = isBase("@", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "@"
        return
    end if
    ignore = isBase("[", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "["
        return
    end if
    ignore = isBase("`", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "`"
        return
    end if
    ignore = isBase("{", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "{"
        return
    end if
    ignore = isBase(" z  z ", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " z  z "
        return
    end if
    ! Empty String, Empty Signs, & \0.
    ignore = isBase("", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    ignore = isBase("    ", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty string"
        return
    end if
    ignore = isBase("+", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+"
        return
    end if
    ignore = isBase("-", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-"
        return
    end if
    ignore = isBase(char(0), 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf

    print *, "# TEST 3: Errors check."
    if ( isBase("", 1, results) .neqv. .TRUE. ) then
        print *, "Test 3 fail at base 1."
        return 
    end if
    if ( isBase("", 37, results) .neqv. .TRUE. ) then
        print *, "Test 3 fail at base 37."
        return 
    end if
    print *, "# TEST 3 Passed"
        
    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine isUnsignedBaseTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "isUnsignedBaseTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    logical                        ::  results, ignore
    error = .TRUE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf

    print *, "# TEST 1: Test true."
    ! Base 2
    if ( isUnsignedBase("01", 2, results) .neqv. .FALSE. ) then
        print *, "Test 1 fail at ", "01"
        return
    else 
        if ( results .neqv. .TRUE. ) then
            print *, "Test 1 fail at ", "01"
            return
        end if
    end if
    ! Base 10
    ignore = isUnsignedBase("0123456789", 10, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0123456789"
        return
    end if
    ! Base 11
    ignore = isUnsignedBase("0123456789aA", 11, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "0123456789aA"
        return
    end if
    ! Base 36
    if ( isUnsignedBase("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", 36, results) &
          .neqv. .FALSE. ) then
        print *, "Test 1 fail at ", "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        return
    else 
        if ( results .neqv. .TRUE. ) then
            print *, "Test 1 fail at ", "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
            return
        end if
    end if
    ignore = isUnsignedBase("  0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ   ", 36, results)
    if ( results .neqv. .TRUE. ) then
        print *, "Test 1 fail at ", "  0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ   "
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf

    print *, "# TEST 2: Test false."
    ! Right before and right after digit/character as digit in ASCII code, also \0.
    ! Base 2
    ignore = isUnsignedBase("/", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isUnsignedBase("2", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "2"
        return
    end if
    ! Base 10
    ignore = isUnsignedBase("/", 10, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "2"
        return
    end if
    ignore = isUnsignedBase(":", 10, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ! Base 11
    ignore = isUnsignedBase("/", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isUnsignedBase(":", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ignore = isUnsignedBase("@", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "@"
        return
    end if
    ignore = isUnsignedBase("B", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "B"
        return
    end if
    ignore = isUnsignedBase("`", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "`"
        return
    end if
    ignore = isUnsignedBase("b", 11, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "b"
        return
    end if
    ! Base 36
    ignore = isUnsignedBase("/", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "/"
        return
    end if
    ignore = isUnsignedBase(":", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", ":"
        return
    end if
    ignore = isUnsignedBase("@", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "@"
        return
    end if
    ignore = isUnsignedBase("[", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "["
        return
    end if
    ignore = isUnsignedBase("`", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "`"
        return
    end if
    ignore = isUnsignedBase("{", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "{"
        return
    end if
    ignore = isUnsignedBase(" z  z ", 36, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", " z  z "
        return
    end if
    ! Empty String, Signs, & \0.
    ignore = isUnsignedBase("", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "0 length"
        return
    end if
    ignore = isUnsignedBase("    ", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "empty string"
        return
    end if
    ignore = isUnsignedBase("+0", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "+0"
        return
    end if
    ignore = isUnsignedBase("-0", 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "-0"
        return
    end if
    ignore = isUnsignedBase(char(0), 2, results)
    if ( results .neqv. .FALSE. ) then
        print *, "Test 2 fail at ", "null char"
        return
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf

    print *, "# TEST 3: Errors check."
    if ( isUnsignedBase("", 1, results) .neqv. .TRUE. ) then
        print *, "Test 3 fail at base 1."
        return 
    end if
    if ( isUnsignedBase("", 37, results) .neqv. .TRUE. ) then
        print *, "Test 3 fail at base 37."
        return 
    end if
    print *, "# TEST 3 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine assumeIsOddTest(error)
    use fniNumberStringUtil
    logical          , intent(out)   ::  error
    character(len=*) , parameter     ::  testName = "assumeIsOddTest"
    character(len=*) , parameter     ::  lf = char(10) // char(13)
    character(len=20), dimension(8)  ::  tCase
    logical          , dimension(8)  ::  lExp
    logical                          ::  r
    integer(k_int32)                 ::  i
    error = .TRUE.
    
    ! True
    tCase(1) = "21"
    lExp(1)  = .TRUE.
    tCase(2) = "1"
    lExp(2)  = .TRUE.
    tCase(3) = "   5555   "
    lExp(3)  = .TRUE.
    tCase(4) = "  2222801 "
    lExp(4)  = .TRUE.
    ! False
    tCase(5) = "12"
    lExp(5)  = .FALSE.
    tCase(6) = "0"
    lExp(6)  = .FALSE.
    tCase(7) = "   8888888   "
    lExp(7)  = .FALSE.
    tCase(8) = "   5555554   "
    lExp(8)  = .FALSE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf

    print *, "# TEST 1: Case to case."
    i = 1
    do while( i < 9 )
        if ( assumeIsOdd(tCase(i), r) .neqv. .FALSE. ) then
            print *, "Test 1, error fail at case '", i, "'."
            return
        end if
        if ( r .neqv. lExp(i) ) then
            print *, "Test 1, result fail at case '", i, "'."
            return
        end if
        i = i + 1
    end do 
    print *, "TEST 1 Passed"
    print *, lf, lf , lf
        
    print *, "# TEST 2: Test error."
    if ( assumeIsOdd("", r) .neqv. .TRUE. ) then 
        print *, "Test 2 fail at ", "0 length string"
        return
    end if
    if ( assumeIsOdd("   ", r) .neqv. .TRUE. ) then 
        print *, "Test 2 fail at ", "empty string"
        return
    end if
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine assumeIsEvenTest(error)
    use fniNumberStringUtil
    logical          , intent(out)   ::  error
    character(len=*) , parameter     ::  testName = "assumeIsEvenTest"
    character(len=*) , parameter     ::  lf = char(10) // char(13)
    character(len=20), dimension(8)  ::  tCase
    logical          , dimension(8)  ::  lExp
    logical                          ::  r
    integer(k_int32)                 ::  i
    error = .TRUE.

    ! True
    tCase(5) = "12"
    lExp(5)  = .TRUE.
    tCase(6) = "0"
    lExp(6)  = .TRUE.
    tCase(7) = "   8888888   "
    lExp(7)  = .TRUE.
    tCase(8) = "   5555554   "
    lExp(8)  = .TRUE.
    ! False
    tCase(1) = "21"
    lExp(1)  = .FALSE.
    tCase(2) = "1"
    lExp(2)  = .FALSE.
    tCase(3) = "   5555   "
    lExp(3)  = .FALSE.
    tCase(4) = "  2222801 "
    lExp(4)  = .FALSE.

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf
    
    print *, "# TEST 1: Test true."
    i = 1
    do while( i < 9 )
        if ( assumeIsEven(tCase(i), r) .neqv. .FALSE. ) then
            print *, "Test 1, error fail at case '", i, "'."
            return
        end if
        if ( r .neqv. lExp(i) ) then
            print *, "Test 1, result fail at case '", i, "'."
            return
        end if
        i = i + 1
    end do 
    print *, "TEST 1 Passed"
    print *, lf, lf , lf

    ! Empty strings will yield error, of which is true return.
    print *, "# TEST 2: Test error."
    if ( assumeIsEven("", r) .neqv. .TRUE. ) then 
        print *, "Test 2 fail at ", "0 length string"
        return
    end if
    if ( assumeIsEven("   ", r) .neqv. .TRUE. ) then 
        print *, "Test 2 fail at ", "empty string"
        return
    end if
    print *, "# TEST 2 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine assumeCompareTest(error)
    use fniNumberStringUtil
    implicit none
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "assumeCompareTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf , lf 
    
    print *, "# TEST 1: Test Smaller Than."
    ! Empty Compare.
    if ( assumeCompare("", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to +"
        return
    end if
    if ( assumeCompare("", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to 0"
        return
    end if
    if ( assumeCompare("", "21") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to 21"
        return
    end if
    if ( assumeCompare("", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to -1"
        return
    end if
    if ( assumeCompare("    ", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to +"
        return
    end if
    if ( assumeCompare("     ", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to +"
        return
    end if
    if ( assumeCompare("       ", "21") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to 21"
        return
    end if
    if ( assumeCompare("         ", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to -1"
        return
    end if
    ! Neg To.
    if ( assumeCompare("-1", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "-1", "0"
        return
    end if
    if ( assumeCompare("-1", " -0 ") /= -1 ) then 
        print *, "Test 1 fail at ", "-1", " -0 "
        return
    end if
    if ( assumeCompare("  -1   ", "+0") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "+0"
        return
    end if
    if ( assumeCompare("  -1   ", "-") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "-"
        return
    end if
    if ( assumeCompare("  -1   ", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "+"
        return
    end if
    if ( assumeCompare("-9", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "-9", "-1"
        return
    end if
    if ( assumeCompare("-1585", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "-1585", "0"
        return
    end if
    if ( assumeCompare("-1585", "1585") /= -1 ) then 
        print *, "Test 1 fail at ", "-1585", "1585"
        return
    end if
    if ( assumeCompare("-58585", "-58584") /= -1 ) then 
        print *, "Test 1 fail at ", "-58585", "-58584"
        return
    end if 
    if ( assumeCompare("-58585", "-35") /= -1 ) then 
        print *, "Test 1 fail at ", "-58585", "-58584"
        return
    end if 
    ! Pos To.
    if ( assumeCompare("1", "9") /= -1 ) then 
        print *, "Test 1 fail at ", "1", "9"
        return
    end if
    if ( assumeCompare("250", "2499") /= -1 ) then 
        print *, "Test 1 fail at ", "250", "2499"
        return
    end if
    if ( assumeCompare("250", "251") /= -1 ) then 
        print *, "Test 1 fail at ", "250", "251"
        return
    end if
    if ( assumeCompare("2785", "+2787") /= -1 ) then 
        print *, "Test 1 fail at ", "2785", "+2787"
        return
    end if
    if ( assumeCompare("+27", "+2574") /= -1 ) then 
        print *, "Test 1 fail at ", "+27", "+2574"
        return
    end if
    if ( assumeCompare("+851", "852") /= -1 ) then 
        print *, "Test 1 fail at ", "+851", "852"
        return
    end if
    if ( assumeCompare("358585", "358587") /= -1 ) then 
        print *, "Test 1 fail at ", "358585", "358587"
        return
    end if
    ! Zeroes 
    if ( assumeCompare("+", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "+", "1"
        return
    end if
    if ( assumeCompare("-", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "-", "1"
        return
    end if
    if ( assumeCompare("-0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "-0", "1"
        return
    end if
    if ( assumeCompare("+0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "+0", "1"
        return
    end if
    if ( assumeCompare("0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "0", "1"
        return
    end if
    if ( assumeCompare("0", "258") /= -1 ) then 
        print *, "Test 1 fail at ", "0", "258"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 2: Test Equal."
    ! Empty & Just Sign.
    if ( assumeCompare("", "") /= 0 ) then 
        print *, "Test 2 fail at ", "0 length to 0 length"
        return
    end if
    if ( assumeCompare("      ", "     ") /= 0 ) then 
        print *, "Test 2 fail at ", "empty to empty"
        return
    end if
    if ( assumeCompare("-", "+") /= 0 ) then 
        print *, "Test 2 fail at ", "-", "+"
        return
    end if
    if ( assumeCompare("+", "+") /= 0 ) then 
        print *, "Test 2 fail at ", "+", "+"
        return
    end if
    if ( assumeCompare("-", "-") /= 0 ) then 
        print *, "Test 2 fail at ", "-", "-"
        return
    end if
    ! Zeroes.
    if ( assumeCompare("-0", "-0") /= 0 ) then 
        print *, "Test 2 fail at ", "-0", "-0"
        return
    end if
    if ( assumeCompare("-0", "+0") /= 0 ) then 
        print *, "Test 2 fail at ", "-0", "+0"
        return
    end if
    if ( assumeCompare("0", "+0") /= 0 ) then 
        print *, "Test 2 fail at ", "0", "+0"
        return
    end if
    if ( assumeCompare(" 0  ", "0   ") /= 0 ) then 
        print *, "Test 2 fail at ", " 0  ", "0   "
        return
    end if
    ! Leading Zeroes.
    if ( assumeCompare("-000250", "   -0250   ") /= 0 ) then 
        print *, "Test 2 fail at ", " 00000  ", "00000000   "
        return
    end if
    if ( assumeCompare("-0000  ", "   +00000") /= 0 ) then 
        print *, "Test 2 fail at ", "-0000  ", "   +00000"
        return
    end if
    if ( assumeCompare("0250", "+000250") /= 0 ) then 
        print *, "Test 2 fail at ", "0250", "+000250"
        return
    end if
    if ( assumeCompare("250", "+0000250") /= 0 ) then 
        print *, "Test 2 fail at ", "250", "+0000250"
        return
    end if
    if ( assumeCompare("  -250  ", "  -0000250  ") /= 0 ) then 
        print *, "Test 2 fail at ", "  -250  ", "  -0000250  "
        return
    end if
    ! Other.
    if ( assumeCompare("+250", "250") /= 0 ) then 
        print *, "Test 2 fail at ", "+250", "250"
        return
    end if
    if ( assumeCompare("  +85007  ", "  +85007  ") /= 0 ) then 
        print *, "Test 2 fail at ", "  +85007  ", "  +85007  "
        return
    end if
    if ( assumeCompare(" -9875778787  ", "  -9875778787  ") /= 0 ) then 
        print *, "Test 2 fail at ", " -9875778787  ", "  -9875778787  "
        return
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf

    print *, "# TEST 3: Test Larger than."
    ! Empty Compare.
    if ( assumeCompare("+", "") /= 1 ) then 
        print *, "Test 3 fail at ", "+ to 0 length"
        return
    end if
    if ( assumeCompare("0", "    ") /= 1 ) then 
        print *, "Test 3 fail at ", "0 to empty"
        return
    end if
    if ( assumeCompare("-1", "") /= 1 ) then 
        print *, "Test 3 fail at ", "-1 to 0 length"
        return
    end if
    if ( assumeCompare("-1", "") /= 1 ) then 
        print *, "Test 3 fail at ", "-1 to empty"
        return
    end if
    ! Pos To.
    if ( assumeCompare("9", "1") /= 1 ) then 
        print *, "Test 3 fail at ", "9", "1"
        return
    end if
    if ( assumeCompare("1585", "-1585") /= 1 ) then 
        print *, "Test 3 fail at ", "1585", "-1585"
        return
    end if
    if ( assumeCompare("58585", "58584") /= 1 ) then 
        print *, "Test 3 fail at ", "58585", "58584"
        return
    end if
    if ( assumeCompare("58585", "35") /= 1 ) then 
        print *, "Test 3 fail at ", "58585", "58584"
        return
    end if
    if ( assumeCompare("+4789     ", "    4788") /= 1 ) then 
        print *, "Test 3 fail at ", "+4789     ", "    4788"
        return
    end if
    ! Neg To.
    if ( assumeCompare("-1", "-9") /= 1 ) then 
        print *, "Test 3 fail at ", "-1", "-9"
        return
    end if
    if ( assumeCompare("-250", "-2499") /= 1 ) then 
        print *, "Test 3 fail at ", "-250", "-2499"
        return
    end if
    if ( assumeCompare("-250", "-251") /= 1 ) then 
        print *, "Test 3 fail at ", "-250", "-251"
        return
    end if
    if ( assumeCompare("-2785", "-2787") /= 1 ) then 
        print *, "Test 3 fail at ", "-2785", "-2787"
        return
    end if
    if ( assumeCompare("-27", "-2574") /= 1 ) then 
        print *, "Test 3 fail at ", "-27", "-2574"
        return
    end if
    if ( assumeCompare("-851", "-852") /= 1 ) then 
        print *, "Test 3 fail at ", "-851", "-852"
        return
    end if
    if ( assumeCompare("-358585     ", "       -358587") /= 1 ) then 
        print *, "Test 3 fail at ", "-358585     ", "       -358587"
        return
    end if
    ! Zeroes
    if ( assumeCompare("1", "+") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+"
        return
    end if
    if ( assumeCompare("1", "  -  ") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "  -  "
        return
    end if
    if ( assumeCompare("1", "-0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "-0"
        return
    end if
    if ( assumeCompare("1", "+0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    if ( assumeCompare("1", "0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    if ( assumeCompare("  258  ", " 0 ") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    print *, "# TEST 3 Passed"
    
    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine assumeCompareAllBaseTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "assumeCompareAllBaseTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf

    print *, "# TEST 1: Test Smaller Than."
    ! Empty Compare.
    if ( assumeCompareAllBase("", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to +"
        return
    end if
    if ( assumeCompareAllBase("", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to 0"
        return
    end if
    if ( assumeCompareAllBase("", "21") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to 21"
        return
    end if
    if ( assumeCompareAllBase("", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "0 length to -1"
        return
    end if
    if ( assumeCompareAllBase("    ", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to +"
        return
    end if
    if ( assumeCompareAllBase("     ", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to +"
        return
    end if
    if ( assumeCompareAllBase("       ", "21") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to 21"
        return
    end if
    if ( assumeCompareAllBase("         ", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "empty to -1"
        return
    end if
    ! Neg To.
    if ( assumeCompareAllBase("-1", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "-1", "0"
        return
    end if
    if ( assumeCompareAllBase("-1", " -0 ") /= -1 ) then 
        print *, "Test 1 fail at ", "-1", " -0 "
        return
    end if
    if ( assumeCompareAllBase("  -1   ", "+0") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "+0"
        return
    end if
    if ( assumeCompareAllBase("  -1   ", "-") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "-"
        return
    end if
    if ( assumeCompareAllBase("  -1   ", "+") /= -1 ) then 
        print *, "Test 1 fail at ", "  -1   ", "+"
        return
    end if
    if ( assumeCompareAllBase("-9", "-1") /= -1 ) then 
        print *, "Test 1 fail at ", "-9", "-1"
        return
    end if
    if ( assumeCompareAllBase("-1585", "0") /= -1 ) then 
        print *, "Test 1 fail at ", "-1585", "0"
        return
    end if
    if ( assumeCompareAllBase("-1585", "1585") /= -1 ) then 
        print *, "Test 1 fail at ", "-1585", "1585"
        return
    end if
    if ( assumeCompareAllBase("-58585", "-58584") /= -1 ) then 
        print *, "Test 1 fail at ", "-58585", "-58584"
        return
    end if 
    if ( assumeCompareAllBase("-58585", "-35") /= -1 ) then 
        print *, "Test 1 fail at ", "-58585", "-58584"
        return
    end if 
    ! Pos To.
    if ( assumeCompareAllBase("1", "9") /= -1 ) then 
        print *, "Test 1 fail at ", "1", "9"
        return
    end if
    if ( assumeCompareAllBase("250", "2499") /= -1 ) then 
        print *, "Test 1 fail at ", "250", "2499"
        return
    end if
    if ( assumeCompareAllBase("250", "251") /= -1 ) then 
        print *, "Test 1 fail at ", "250", "251"
        return
    end if
    if ( assumeCompareAllBase("2785", "+2787") /= -1 ) then 
        print *, "Test 1 fail at ", "2785", "+2787"
        return
    end if
    if ( assumeCompareAllBase("+27", "+2574") /= -1 ) then 
        print *, "Test 1 fail at ", "+27", "+2574"
        return
    end if
    if ( assumeCompareAllBase("+851", "852") /= -1 ) then 
        print *, "Test 1 fail at ", "+851", "852"
        return
    end if
    if ( assumeCompareAllBase("358585", "358587") /= -1 ) then 
        print *, "Test 1 fail at ", "358585", "358587"
        return
    end if
    ! Zeroes 
    if ( assumeCompareAllBase("+", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "+", "1"
        return
    end if
    if ( assumeCompareAllBase("-", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "-", "1"
        return
    end if
    if ( assumeCompareAllBase("-0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "-0", "1"
        return
    end if
    if ( assumeCompareAllBase("+0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "+0", "1"
        return
    end if
    if ( assumeCompareAllBase("0", "1") /= -1 ) then 
        print *, "Test 1 fail at ", "0", "1"
        return
    end if
    if ( assumeCompareAllBase("0", "258") /= -1 ) then 
        print *, "Test 1 fail at ", "0", "258"
        return
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 2: Test Equal."
    ! Empty & Just Sign.
    if ( assumeCompareAllBase("", "") /= 0 ) then 
        print *, "Test 2 fail at ", "0 length to 0 length"
        return
    end if
    if ( assumeCompareAllBase("      ", "     ") /= 0 ) then 
        print *, "Test 2 fail at ", "empty to empty"
        return
    end if
    if ( assumeCompareAllBase("-", "+") /= 0 ) then 
        print *, "Test 2 fail at ", "-", "+"
        return
    end if
    if ( assumeCompareAllBase("+", "+") /= 0 ) then 
        print *, "Test 2 fail at ", "+", "+"
        return
    end if
    if ( assumeCompareAllBase("-", "-") /= 0 ) then 
        print *, "Test 2 fail at ", "-", "-"
        return
    end if
    ! Zeroes.
    if ( assumeCompareAllBase("-0", "-0") /= 0 ) then 
        print *, "Test 2 fail at ", "-0", "-0"
        return
    end if
    if ( assumeCompareAllBase("-0", "+0") /= 0 ) then 
        print *, "Test 2 fail at ", "-0", "+0"
        return
    end if
    if ( assumeCompareAllBase("0", "+0") /= 0 ) then 
        print *, "Test 2 fail at ", "0", "+0"
        return
    end if
    if ( assumeCompareAllBase(" 0  ", "0   ") /= 0 ) then 
        print *, "Test 2 fail at ", " 0  ", "0   "
        return
    end if
    ! Leading Zeroes.
    if ( assumeCompareAllBase("-000250", "   -0250   ") /= 0 ) then 
        print *, "Test 2 fail at ", " 00000  ", "00000000   "
        return
    end if
    if ( assumeCompareAllBase("-0000  ", "   +00000") /= 0 ) then 
        print *, "Test 2 fail at ", "-0000  ", "   +00000"
        return
    end if
    if ( assumeCompareAllBase("0250", "+000250") /= 0 ) then 
        print *, "Test 2 fail at ", "0250", "+000250"
        return
    end if
    if ( assumeCompareAllBase("250", "+0000250") /= 0 ) then 
        print *, "Test 2 fail at ", "250", "+0000250"
        return
    end if
    if ( assumeCompareAllBase("  -250  ", "  -0000250  ") /= 0 ) then 
        print *, "Test 2 fail at ", "  -250  ", "  -0000250  "
        return
    end if
    ! Other.
    if ( assumeCompareAllBase("+250", "250") /= 0 ) then 
        print *, "Test 2 fail at ", "+250", "250"
        return
    end if
    if ( assumeCompareAllBase("  +85007  ", "  +85007  ") /= 0 ) then 
        print *, "Test 2 fail at ", "  +85007  ", "  +85007  "
        return
    end if
    if ( assumeCompareAllBase(" -9875778787  ", "  -9875778787  ") /= 0 ) then 
        print *, "Test 2 fail at ", " -9875778787  ", "  -9875778787  "
        return
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf

    print *, "# TEST 3: Test Larger than."
    ! Empty Compare.
    if ( assumeCompareAllBase("+", "") /= 1 ) then 
        print *, "Test 3 fail at ", "+ to 0 length"
        return
    end if
    if ( assumeCompareAllBase("0", "    ") /= 1 ) then 
        print *, "Test 3 fail at ", "0 to empty"
        return
    end if
    if ( assumeCompareAllBase("-1", "") /= 1 ) then 
        print *, "Test 3 fail at ", "-1 to 0 length"
        return
    end if
    if ( assumeCompareAllBase("-1", "") /= 1 ) then 
        print *, "Test 3 fail at ", "-1 to empty"
        return
    end if
    ! Pos To.
    if ( assumeCompareAllBase("9", "1") /= 1 ) then 
        print *, "Test 3 fail at ", "9", "1"
        return
    end if
    if ( assumeCompareAllBase("1585", "-1585") /= 1 ) then 
        print *, "Test 3 fail at ", "1585", "-1585"
        return
    end if
    if ( assumeCompareAllBase("58585", "58584") /= 1 ) then 
        print *, "Test 3 fail at ", "58585", "58584"
        return
    end if
    if ( assumeCompareAllBase("58585", "35") /= 1 ) then 
        print *, "Test 3 fail at ", "58585", "58584"
        return
    end if
    if ( assumeCompareAllBase("+4789     ", "    4788") /= 1 ) then 
        print *, "Test 3 fail at ", "+4789     ", "    4788"
        return
    end if
    ! Neg To.
    if ( assumeCompareAllBase("-1", "-9") /= 1 ) then 
        print *, "Test 3 fail at ", "-1", "-9"
        return
    end if
    if ( assumeCompareAllBase("-250", "-2499") /= 1 ) then 
        print *, "Test 3 fail at ", "-250", "-2499"
        return
    end if
    if ( assumeCompareAllBase("-250", "-251") /= 1 ) then 
        print *, "Test 3 fail at ", "-250", "-251"
        return
    end if
    if ( assumeCompareAllBase("-2785", "-2787") /= 1 ) then 
        print *, "Test 3 fail at ", "-2785", "-2787"
        return
    end if
    if ( assumeCompareAllBase("-27", "-2574") /= 1 ) then 
        print *, "Test 3 fail at ", "-27", "-2574"
        return
    end if
    if ( assumeCompareAllBase("-851", "-852") /= 1 ) then 
        print *, "Test 3 fail at ", "-851", "-852"
        return
    end if
    if ( assumeCompareAllBase("-358585     ", "       -358587") /= 1 ) then 
        print *, "Test 3 fail at ", "-358585     ", "       -358587"
        return
    end if
    ! Zeroes
    if ( assumeCompareAllBase("1", "+") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+"
        return
    end if
    if ( assumeCompareAllBase("1", "  -  ") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "  -  "
        return
    end if
    if ( assumeCompareAllBase("1", "-0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "-0"
        return
    end if
    if ( assumeCompareAllBase("1", "+0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    if ( assumeCompareAllBase("1", "0") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    if ( assumeCompareAllBase("  258  ", " 0 ") /= 1 ) then 
        print *, "Test 3 fail at ", "1", "+0"
        return
    end if
    print *, "# TEST 3 Passed"
    print *, lf, lf, lf
    
    print *, "# TEST 4: Other bases."
    ! Base 11
    if ( assumeCompareAllBase("     a0a", "a10   ") /= -1 ) then 
        print *, "Test 4 fail at ", "     a0a", "a10   "
        return
    end if
    if ( assumeCompareAllBase("-a10", "-a0a") /= -1 ) then 
        print *, "Test 4 fail at ", "-a10", "-a0a"
        return
    end if
    if ( assumeCompareAllBase("a", "A") /= 0 ) then 
        print *, "Test 4 fail at ", "-a10", "-a0a"
        return
    end if
    if ( assumeCompareAllBase("a0a", "a09") /= 1 ) then 
        print *, "Test 4 fail at ", "-a10", "-a0a"
        return
    end if
    if ( assumeCompareAllBase("   -aa9", "-aaa  ") /= 1 ) then 
        print *, "Test 4 fail at ", "   -aa9", "-aaa  "
        return
    end if        
    ! Base 36.
    if ( assumeCompareAllBase("zj", "zk") /= -1 ) then 
        print *, "Test 4 fail at ", "zj", "zk"
        return
    end if
    if ( assumeCompareAllBase("-zb", "-za") /= -1 ) then 
        print *, "Test 4 fail at ", "-zb", "-za"
        return
    end if
    if ( assumeCompareAllBase("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ") /= 0 ) then 
        print *, "Test 4 fail at ", "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        return
    end if
    if ( assumeCompareAllBase("   zva", "zv0  ") /= 1 ) then 
        print *, "Test 4 fail at ", "   zva", "zv0  "
        return
    end if
    if ( assumeCompareAllBase("  -zzy  ", "   -zzz  ") /= 1 ) then 
        print *, "Test 4 fail at ", "  -zzy  ", "   -zzz  "
        return
    end if
    print *, "# TEST 4 Passed"
    
    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine fnStringCompareTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "fnStringCompareTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    integer(k_int32)               ::  results
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf

    print *, "# TEST 1: Test Smaller Than."
    ! Neg To.
    if ( fnStringCompare("-1", "0", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1", "0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1", "0"
            return
        end if
    end if
    if ( fnStringCompare("     -1", "-0    ", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "     -1", "-0    "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "     -1", "-0    "
            return
        end if
    end if
    if ( fnStringCompare("-1", "+0", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1", "+0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1", "+0"
            return
        end if
    end if
    if ( fnStringCompare("-9", "-1", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-9", "-1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-9", "-1"
            return
        end if
    end if
    if ( fnStringCompare("-1585", "0", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1585", "0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1585", "0"
            return
        end if
    end if
    if ( fnStringCompare("  -1585  ", "   1585   ", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "  -1585  ", "   1585   "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "  -1585  ", "   1585   "
            return
        end if
    end if
    if ( fnStringCompare("-58585", "-58584", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-58585", "-58584"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-58585", "-58584"
            return
        end if
    end if
    if ( fnStringCompare("-58585", "-35", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-58585", "-35"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-58585", "-35"
            return
        end if
    end if
    ! Pos To.
    if ( fnStringCompare("1", "9", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "1", "9"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "1", "9"
            return
        end if
    end if
    if ( fnStringCompare("250", "2499", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "250", "2499"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "250", "2499"
            return
        end if
    end if
    if ( fnStringCompare("250", "251", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "250", "251"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "250", "251"
            return
        end if
    end if
    if ( fnStringCompare("2785", "+2787", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "2785", "+2787"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "2785", "+2787"
            return
        end if
    end if
    if ( fnStringCompare("+27", "+2574", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "+27", "+2574"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "+27", "+2574"
            return
        end if
    end if
    if ( fnStringCompare("       +851      ", "   852     ", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "       +851      ", "   852     "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "       +851      ", "   852     "
            return
        end if
    end if
    if ( fnStringCompare("              358585", "358587       ", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "              358585", "358587       "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "              358585", "358587       "
            return
        end if
    end if        
    ! Zeroes
    if ( fnStringCompare("-0", "1", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-0", "1"
            return
        end if
    end if
    if ( fnStringCompare("+0", "1", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "+0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "+0", "1"
            return
        end if
    end if
    if ( fnStringCompare("0", "1", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "0", "1"
            return
        end if
    end if
    if ( fnStringCompare("0", "258", results) /= 0 ) then
        print *, "Test 1 fail logical at ", "0", "258"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "0", "258"
            return
        end if
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 2: Test Equal."
    ! Zeroes.
    if ( fnStringCompare("-0000", "-0", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-0000", "-0"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-0000", "-0"
            return
        end if
    end if
    if ( fnStringCompare("-0", "+000000", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-0", "+000000"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-0", "+000000"
            return
        end if
    end if
    if ( fnStringCompare("     0     ", "    0     ", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "     0     ", "    0     "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "     0     ", "    0     "
            return
        end if
    end if
    if ( fnStringCompare("   +0  ", " -0   ", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "   +0  ", " -0   "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "   +0  ", " -0   "
            return
        end if
    end if
    if ( fnStringCompare("+0", "0", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+0", "0"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+0", "0"
            return
        end if
    end if
    ! Leading Zeroes.
    if ( fnStringCompare("-000250", "-0250", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-000250", "-0250"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-000250", "-0250"
            return
        end if
    end if
    ! 1s.
    if ( fnStringCompare("+1", "1", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+1", "1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+1", "1"
            return
        end if
    end if
    if ( fnStringCompare("+1", "+1", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+1", "+1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+1", "+1"
            return
        end if
    end if
    if ( fnStringCompare("-1", "-1", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-1", "-1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-1", "-1"
            return
        end if
    end if
    ! Other.
    if ( fnStringCompare("+250", "250", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+250", "250"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+250", "250"
            return
        end if
    end if
    if ( fnStringCompare("+85007", "+85007", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+85007", "+85007"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+85007", "+85007"
            return
        end if
    end if
    if ( fnStringCompare("-9875778787", "-9875778787", results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-9875778787", "-9875778787"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-9875778787", "-9875778787"
            return
        end if
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 3: Test Larger than."
    ! Pos To.
    if ( fnStringCompare("1", "0", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1", "0"
            return
        end if
    end if
    if ( fnStringCompare("9", "1", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "9", "1"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "9", "1"
            return
        end if
    end if
    if ( fnStringCompare("1585", "0", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1585", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1585", "0"
            return
        end if
    end if
    if ( fnStringCompare("1585", "-1585", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1585", "-1585"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1585", "-1585"
            return
        end if
    end if
    if ( fnStringCompare("58585", "58584", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "58585", "58584"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "58585", "58584"
            return
        end if
    end if
    if ( fnStringCompare("58585", "35", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "58585", "35"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "58585", "35"
            return
        end if
    end if
    if ( fnStringCompare("4789", "+4788", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "4789", "+4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "4789", "+4788"
            return
        end if
    end if
    if ( fnStringCompare("+4789", "4788", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "+4789", "4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "+4789", "4788"
            return
        end if
    end if
    if ( fnStringCompare("+4789", "+4788", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "+4789", "+4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "+4789", "+4788"
            return
        end if
    end if
    ! Neg To
    if ( fnStringCompare("-1", "-9", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-1", "-9"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-1", "-9"
            return
        end if
    end if
    if ( fnStringCompare("-250", "-2499", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-250", "-2499"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-250", "-2499"
            return
        end if
    end if
    if ( fnStringCompare("   -250           ", " -251    ", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "   -250           ", " -251    "
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "   -250           ", " -251    "
            return
        end if
    end if
    if ( fnStringCompare("-2785     ", "  -2787    ", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-2785     ", "  -2787    "
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-2785     ", "  -2787    "
            return
        end if
    end if
    if ( fnStringCompare("-27", "-2574", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-27", "-2574"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-27", "-2574"
            return
        end if
    end if
    if ( fnStringCompare("-851", "-852", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-851", "-852"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-851", "-852"
            return
        end if
    end if
    if ( fnStringCompare("-358585", "-358587", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-358585", "-358587"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-358585", "-358587"
            return
        end if
    end if
    ! Zeroes
    if ( fnStringCompare("001", "-0", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "001", "-0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "001", "-0"
            return
        end if
    end if
    if ( fnStringCompare("1", "+0", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1", "+0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1", "+0"
            return
        end if
    end if
    if ( fnStringCompare("   001  ", "   0  ", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "001", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "001", "0"
            return
        end if
    end if
    if ( fnStringCompare("258", "0", results) /= 0 ) then
        print *, "Test 3 fail logical at ", "258", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "258", "0"
            return
        end if
    end if
    print *, "# TEST 3 Passed" 
    print *, lf, lf, lf

    print *, "# TEST 4: Errors."
    ! Single signs.
    if ( fnStringCompare("+", "123", results) /= 3 ) then
        print *, "Test 4 fail at ", "+", "123"
        return
    end if
    if ( fnStringCompare("-", "123", results) /= 3 ) then
        print *, "Test 4 fail at ", "-", "123"
        return
    end if
    if ( fnStringCompare("123", "-", results) /= 4 ) then
        print *, "Test 4 fail at ", "123", "-"
        return
    end if
    if ( fnStringCompare("123", "+", results) /= 4 ) then
        print *, "Test 4 fail at ", "123", "+"
        return
    end if
    ! length 1 incorrect digit.
    if ( fnStringCompare(char(0), "1234", results) /= 3 ) then
        print *, "Test 4 fail at ", "null char to 1234"
        return
    end if
    if ( fnStringCompare("/", "1234", results) /= 3 ) then
        print *, "Test 4 fail at ", "/", "1234"
        return
    end if
    if ( fnStringCompare(":", "1234", results) /= 3 ) then
        print *, "Test 4 fail at ", ":", "1234"
        return
    end if
    if ( fnStringCompare("a", "1234", results) /= 3 ) then
        print *, "Test 4 fail at ", "a", "1234"
        return
    end if
    if ( fnStringCompare("1234", char(0), results) /= 4 ) then
        print *, "Test 4 fail at ", "1234 to null char"
        return
    end if
    if ( fnStringCompare("1234", "/", results) /= 4 ) then
        print *, "Test 4 fail at ", "1234", "/"
        return
    end if
    if ( fnStringCompare("1234", ":", results) /= 4 ) then
        print *, "Test 4 fail at ", "1234", ":"
        return
    end if
    if ( fnStringCompare("1234", "a", results) /= 4 ) then
        print *, "Test 4 fail at ", "1234", "a"
        return
    end if
    ! equal length incorrect digit.
    if ( fnStringCompare("  12345678\0", "123456789  ", results) /= 3 ) then
        print *, "Test 4 fail at ", "  12345678\0", "123456789  "
        return
    end if
    if ( fnStringCompare("12345678/  ", "  123456789", results) /= 3 ) then
        print *, "Test 4 fail at ", "12345678/  ", "  123456789"
        return
    end if
    if ( fnStringCompare("   12345678: ", " 123456789   ", results) /= 3 ) then
        print *, "Test 4 fail at ", "   12345678: ", " 123456789   "
        return
    end if
    if ( fnStringCompare(" 12345678a   ", "   123456789 ", results) /= 3 ) then
        print *, "Test 4 fail at ", " 12345678a   ", "   123456789 "
        return
    end if
    if ( fnStringCompare("123456789", "12345678"//char(0), results) /= 4 ) then
        print *, "Test 4 fail at ", "123456789", "12345678"//char(0)
        return
    end if
    if ( fnStringCompare("123456789", "12345678/", results) /= 4 ) then
        print *, "Test 4 fail at ", "123456789", "12345678/"
        return
    end if
    if ( fnStringCompare("123456789", "12345678:", results) /= 4 ) then
        print *, "Test 4 fail at ", "123456789", "12345678:"
        return
    end if
    if ( fnStringCompare("123456789", "12345678a", results) /= 4 ) then
        print *, "Test 4 fail at ", "123456789", "12345678a"
        return
    end if
    ! different length incorrect digit.
    if ( fnStringCompare("  12345678\0   ", "   12345678901  ", results) /= 3 ) then
        print *, "Test 4 fail at ", "  12345678\0   ", "   12345678901  "
        return
    end if
    if ( fnStringCompare("   12345678/  ", "   12345678901  ", results) /= 3 ) then
        print *, "Test 4 fail at ", "   12345678/  ", "   12345678901  "
        return
    end if
    if ( fnStringCompare("12345678:", "12345", results) /= 3 ) then
        print *, "Test 4 fail at ", "12345678:", "12345"
        return
    end if
    if ( fnStringCompare("12345678a", "12345", results) /= 3 ) then
        print *, "Test 4 fail at ", "12345678a", "12345"
        return
    end if
    if ( fnStringCompare("12345678901", "12345678\0", results) /= 4 ) then
        print *, "Test 4 fail at ", "12345678901", "12345678\0"
        return
    end if
    if ( fnStringCompare("12345678901", "12345678/", results) /= 4 ) then
        print *, "Test 4 fail at ", "12345678901", "12345678/"
        return
    end if
    if ( fnStringCompare("12345", "12345678:", results) /= 4 ) then
        print *, "Test 4 fail at ", "12345", "12345678:"
        return
    end if
    if ( fnStringCompare("12345", "12345678a", results) /= 4 ) then
        print *, "Test 4 fail at ", "12345", "12345678a"
        return
    end if
    ! Empty string
    if ( fnStringCompare("", "123", results) /= 1 ) then
        print *, "Test 4 fail at ", "0 length to 123"
        return
    end if
    if ( fnStringCompare("    ", "123", results) /= 1 ) then
        print *, "Test 4 fail at ", "empty to 123"
        return
    end if
    if ( fnStringCompare("123", "", results) /= 2 ) then
        print *, "Test 4 fail at ", "123 to 0 length"
        return
    end if
    if ( fnStringCompare("123", "    ", results) /= 2 ) then
        print *, "Test 4 fail at ", "123 to empty"
        return
    end if
    print *, "# TEST 4 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine fnStringCompareAsBaseTest(error)
    use fniNumberStringUtil
    logical         , intent(out)  ::  error
    character(len=*), parameter    ::  testName = "fnStringCompareAsBaseTest"
    character(len=*), parameter    ::  lf = char(10) // char(13)
    integer(k_int32)               ::  results
    error = .TRUE.
            
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf, lf

    print *, "# TEST 1: Test Smaller Than."
    ! Neg To.
    if ( fnStringCompareAsBase("-1", "0", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1", "0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1", "0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("     -1", "-0    ", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "     -1", "-0    "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "     -1", "-0    "
            return
        end if
    end if
    if ( fnStringCompareAsBase("-1", "+0", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1", "+0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1", "+0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-9", "-1", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-9", "-1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-9", "-1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-1585", "0", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-1585", "0"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-1585", "0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("  -1585  ", "   1585   ", 10 , results) /= 0 ) then
        print *, "Test 1 fail logical at ", "  -1585  ", "   1585   "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "  -1585  ", "   1585   "
            return
        end if
    end if
    if ( fnStringCompareAsBase("-58585", "-58584", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-58585", "-58584"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-58585", "-58584"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-58585", "-35", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-58585", "-35"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-58585", "-35"
            return
        end if
    end if
    ! Pos To.
    if ( fnStringCompareAsBase("1", "9", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "1", "9"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "1", "9"
            return
        end if
    end if
    if ( fnStringCompareAsBase("250", "2499", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "250", "2499"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "250", "2499"
            return
        end if
    end if
    if ( fnStringCompareAsBase("250", "251", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "250", "251"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "250", "251"
            return
        end if
    end if
    if ( fnStringCompareAsBase("2785", "+2787", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "2785", "+2787"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "2785", "+2787"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+27", "+2574", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "+27", "+2574"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "+27", "+2574"
            return
        end if
    end if
    if ( fnStringCompareAsBase("       +851      ", "   852     ", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "       +851      ", "   852     "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "       +851      ", "   852     "
            return
        end if
    end if
    if ( fnStringCompareAsBase("              358585", "358587       ", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "              358585", "358587       "
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "              358585", "358587       "
            return
        end if
    end if        
    ! Zeroes
    if ( fnStringCompareAsBase("-0", "1", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "-0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "-0", "1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+0", "1", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "+0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "+0", "1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("0", "1", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "0", "1"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "0", "1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("0", "258", 10, results) /= 0 ) then
        print *, "Test 1 fail logical at ", "0", "258"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 1 fail result at", "0", "258"
            return
        end if
    end if
    print *, "# TEST 1 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 2: Test Equal."
    ! Zeroes.
    if ( fnStringCompareAsBase("-0000", "-0", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-0000", "-0"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-0000", "-0" 
            return
        end if
    end if
    if ( fnStringCompareAsBase("-0", "+000000", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-0", "+000000"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-0", "+000000"
            return
        end if
    end if
    if ( fnStringCompareAsBase("     0     ", "    0     ", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "     0     ", "    0     "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "     0     ", "    0     "
            return
        end if
    end if
    if ( fnStringCompareAsBase("   +0  ", " -0   ", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "   +0  ", " -0   "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "   +0  ", " -0   "
            return
        end if
    end if
    if ( fnStringCompareAsBase("+0", "0", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+0", "0"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+0", "0"
            return
        end if
    end if
    ! Leading Zeroes.
    if ( fnStringCompareAsBase("-000250", "-0250", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-000250", "-0250"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-000250", "-0250"
            return
        end if
    end if
    ! 1s.
    if ( fnStringCompareAsBase("+1", "1", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+1", "1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+1", "1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+1", "+1", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+1", "+1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+1", "+1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-1", "-1", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-1", "-1"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-1", "-1"
            return
        end if
    end if
    ! Other.
    if ( fnStringCompareAsBase("+250", "250", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+250", "250"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+250", "250"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+85007", "+85007", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "+85007", "+85007"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "+85007", "+85007"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-9875778787", "-9875778787", 10, results) /= 0 ) then
        print *, "Test 2 fail logical at ", "-9875778787", "-9875778787"
        return
    else
        if ( results /= 0 ) then
            print *, "Test 2 fail result at", "-9875778787", "-9875778787"
            return
        end if
    end if
    print *, "# TEST 2 Passed"
    print *, lf, lf, lf
        
    print *, "# TEST 3: Test Larger than."
    ! Pos To.
    if ( fnStringCompareAsBase("1", "0", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1", "0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("9", "1", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "9", "1"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "9", "1"
            return
        end if
    end if
    if ( fnStringCompareAsBase("1585", "0", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1585", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1585", "0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("1585", "-1585", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1585", "-1585"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "1585", "-1585"
            return
        end if
    end if
    if ( fnStringCompareAsBase("58585", "58584", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "58585", "58584"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "58585", "58584"
            return
        end if
    end if
    if ( fnStringCompareAsBase("58585", "35", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "58585", "35"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "58585", "35"
            return
        end if
    end if
    if ( fnStringCompareAsBase("4789", "+4788", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "4789", "+4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "4789", "+4788"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+4789", "4788", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "+4789", "4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "+4789", "4788"
            return
        end if
    end if
    if ( fnStringCompareAsBase("+4789", "+4788", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "+4789", "+4788"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "+4789", "+4788"
            return
        end if
    end if
    ! Neg To
    if ( fnStringCompareAsBase("-1", "-9", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-1", "-9"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-1", "-9"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-250", "-2499", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-250", "-2499"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-250", "-2499"
            return
        end if
    end if
    if ( fnStringCompareAsBase("   -250           ", " -251    ", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "   -250           ", " -251    "
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "   -250           ", " -251    "
            return
        end if
    end if
    if ( fnStringCompareAsBase("-2785     ", "  -2787    ", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-2785     ", "  -2787    "
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-2785     ", "  -2787    "
            return
        end if
    end if
    if ( fnStringCompareAsBase("-27", "-2574", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-27", "-2574"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-27", "-2574"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-851", "-852", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-851", "-852"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-851", "-852"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-358585", "-358587", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "-358585", "-358587"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at", "-358585", "-358587"
            return
        end if
    end if
    ! Zeroes
    if ( fnStringCompareAsBase("001", "-0", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "001", "-0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at ", "001", "-0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("1", "+0", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "1", "+0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at ", "1", "+0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("   001  ", "   0  ", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "001", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at ", "001", "0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("258", "0", 10, results) /= 0 ) then
        print *, "Test 3 fail logical at ", "258", "0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 3 fail result at ", "258", "0"
            return
        end if
    end if
    print *, "# TEST 3 Passed" 
    print *, lf, lf, lf

    print *, "# TEST 4: Other bases."
    ! Base11
    if ( fnStringCompareAsBase("a0a", "a10", 11, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "a0a", "a10"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 4 fail result at ", "a0a", "a10"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-a10", "-a0a", 11, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "-a10", "-a0a"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 4 fail result at ", "-a10", "-a0a"
            return
        end if
    end if
    if ( fnStringCompareAsBase(" 123456789a", "123456789A  ", 11, results) /= 0 ) then
        print *, "Test 4 fail logical at ", " 123456789a", "123456789A  "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 4 fail result at ", " 123456789a", "123456789A  "
            return
        end if
    end if
    if ( fnStringCompareAsBase("a0a", "a09", 11, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "a0a", "a09"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 4 fail result at ", "a0a", "a09"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-aa9", "-aaa", 11, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "-aa9", "-aaa"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 4 fail result at ", "-aa9", "-aaa"
            return
        end if
    end if
    ! Base 36.
    if ( fnStringCompareAsBase("z9", "za", 36, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "z9", "za"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 4 fail result at ", "z9", "za"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-zb", "-za", 36, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "-zb", "-za"
        return
    else
        if ( results /= -1 ) then
            print *, "Test 4 fail result at ", "-zb", "-za"
            return
        end if
    end if
    if ( fnStringCompareAsBase("  abcdefghijklmnopqrstuvwxyz  ", "  ABCDEFGHIJKLMNOPQRSTUVWXYZ  ", 36, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "  abcdefghijklmnopqrstuvwxyz  ", "  ABCDEFGHIJKLMNOPQRSTUVWXYZ  "
        return
    else
        if ( results /= 0 ) then
            print *, "Test 4 fail result at ", "  abcdefghijklmnopqrstuvwxyz  ", "  ABCDEFGHIJKLMNOPQRSTUVWXYZ  "
            return
        end if
    end if
    if ( fnStringCompareAsBase("zva", "zv0", 36, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "zva", "zv0"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 4 fail result at ", "zva", "zv0"
            return
        end if
    end if
    if ( fnStringCompareAsBase("-zzy", "-zzz", 36, results) /= 0 ) then
        print *, "Test 4 fail logical at ", "-zzy", "-zzz"
        return
    else
        if ( results /= 1 ) then
            print *, "Test 4 fail result at ", "-zzy", "-zzz"
            return
        end if
    end if
    print *, "# TEST 4 Passed"
    print *, lf, lf, lf

    print *, "# TEST 5: Errors."
    ! Single signs.
    if ( fnStringCompareAsBase("+", "123", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "+", "123"
        return
    end if
    if ( fnStringCompareAsBase("-", "123", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "-", "123"
        return
    end if
    if ( fnStringCompareAsBase("123", "-", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123", "-"
        return
    end if
    if ( fnStringCompareAsBase("123", "+", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123", "+"
        return
    end if
    ! length 1 incorrect digit.
    if ( fnStringCompareAsBase(char(0), "1234", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "null char to 1234"
        return
    end if
    if ( fnStringCompareAsBase("/", "1234", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "/", "1234"
        return
    end if
    if ( fnStringCompareAsBase(":", "1234", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", ":", "1234"
        return
    end if
    if ( fnStringCompareAsBase("a", "1234", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "a", "1234"
        return
    end if
    if ( fnStringCompareAsBase("1234", char(0), 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "1234 to null char"
        return
    end if
    if ( fnStringCompareAsBase("1234", "/", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "1234", "/"
        return
    end if
    if ( fnStringCompareAsBase("1234", ":", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "1234", ":"
        return
    end if
    if ( fnStringCompareAsBase("1234", "a", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "1234", "a"
        return
    end if
    ! equal length incorrect digit.
    if ( fnStringCompareAsBase("  12345678\0", "123456789  ", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "  12345678\0", "123456789  "
        return
    end if
    if ( fnStringCompareAsBase("12345678/  ", "  123456789", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "12345678/  ", "  123456789"
        return
    end if
    if ( fnStringCompareAsBase("   12345678: ", " 123456789   ", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "   12345678: ", " 123456789   "
        return
    end if
    if ( fnStringCompareAsBase(" 12345678a   ", "   123456789 ", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", " 12345678a   ", "   123456789 "
        return
    end if
    if ( fnStringCompareAsBase("123456789", "12345678"//char(0), 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123456789", "12345678"//char(0)
        return
    end if
    if ( fnStringCompareAsBase("123456789", "12345678/", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123456789", "12345678/"
        return
    end if
    if ( fnStringCompareAsBase("123456789", "12345678:", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123456789", "12345678:"
        return
    end if
    if ( fnStringCompareAsBase("123456789", "12345678a", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "123456789", "12345678a"
        return
    end if
    ! different length incorrect digit.
    if ( fnStringCompareAsBase("  12345678\0   ", "   12345678901  ", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "  12345678\0   ", "   12345678901  "
        return
    end if
    if ( fnStringCompareAsBase("   12345678/  ", "   12345678901  ", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "   12345678/  ", "   12345678901  "
        return
    end if
    if ( fnStringCompareAsBase("12345678:", "12345", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "12345678:", "12345"
        return
    end if
    if ( fnStringCompareAsBase("12345678a", "12345", 10, results) /= 3 ) then
        print *, "Test 5 fail at ", "12345678a", "12345"
        return
    end if
    if ( fnStringCompareAsBase("12345678901", "12345678\0", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "12345678901", "12345678\0"
        return
    end if
    if ( fnStringCompareAsBase("12345678901", "12345678/", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "12345678901", "12345678/"
        return
    end if
    if ( fnStringCompareAsBase("12345", "12345678:", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "12345", "12345678:"
        return
    end if
    if ( fnStringCompareAsBase("12345", "12345678a", 10, results) /= 4 ) then
        print *, "Test 5 fail at ", "12345", "12345678a"
        return
    end if
    ! Base 11 incorrect digit.
    if ( fnStringCompareAsBase("/", "1", 11, results) /= 3 ) then
        print *, "Test 5 fail at ", "/", "1"
        return
    end if
    if ( fnStringCompareAsBase("b", "1", 11, results) /= 3 ) then
        print *, "Test 5 fail at ", "b", "1"
        return
    end if
    if ( fnStringCompareAsBase("B", "1", 11, results) /= 3 ) then
        print *, "Test 5 fail at ", "B", "1"
        return
    end if
    if ( fnStringCompareAsBase("1", "/", 11, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "/"
        return
    end if
    if ( fnStringCompareAsBase("1", "b", 11, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "b"
        return
    end if    
    if ( fnStringCompareAsBase("1", "B", 11, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "B"
        return
    end if
    ! Base 36 incorrect digit.
    if ( fnStringCompareAsBase("/", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", "/", "1"
        return
    end if
    if ( fnStringCompareAsBase(":", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", ":", "1"
        return
    end if
    if ( fnStringCompareAsBase("@", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", "@", "1"
        return
    end if
    if ( fnStringCompareAsBase("[", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", "[", "1"
        return
    end if
    if ( fnStringCompareAsBase("`", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", "`", "1"
        return
    end if
    if ( fnStringCompareAsBase("{", "1", 36, results) /= 3 ) then
        print *, "Test 5 fail at ", "{", "1"
        return
    end if
    if ( fnStringCompareAsBase("1", "/", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "/"
        return
    end if
    if ( fnStringCompareAsBase("1", ":", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", ":"
        return
    end if
    if ( fnStringCompareAsBase("1", "@", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "@"
        return
    end if
    if ( fnStringCompareAsBase("1", "[", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "["
        return
    end if
    if ( fnStringCompareAsBase("1", "`", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "`"
        return
    end if
    if ( fnStringCompareAsBase("1", "{", 36, results) /= 4 ) then
        print *, "Test 5 fail at ", "1", "{"
        return
    end if
    ! Unsupported Base
    if ( fnStringCompareAsBase("000", "000", 1, results) /= 5 ) then
        print *, "Test 5 fail at ", "000", "000"
        return
    end if
    if ( fnStringCompareAsBase("001", "001", 37, results) /= 5 ) then
        print *, "Test 5 fail at ", "001", "001"
        return
    end if    
    ! Empty string
    if ( fnStringCompareAsBase("", "123", 10, results) /= 1 ) then
        print *, "Test 5 fail at ", "0 length to 123"
        return
    end if
    if ( fnStringCompareAsBase("    ", "123", 10, results) /= 1 ) then
        print *, "Test 5 fail at ", "empty to 123"
        return
    end if
    if ( fnStringCompareAsBase("123", "", 10, results) /= 2 ) then
        print *, "Test 5 fail at ", "123 to 0 length"
        return
    end if
    if ( fnStringCompareAsBase("123", "    ", 10, results) /= 2 ) then
        print *, "Test 5 fail at ", "123 to empty"
        return
    end if
    print *, "# TEST 5 Passed"

    error = .FALSE.
    print *, lf, lf
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

