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

! This test file test fnHexUtil plus the int32ToHexAsU and the
! int32ToHex function
integer(k_int32) function getRandom32 (minimum, maximum)
    use fnConsts
    implicit none
    integer(k_int32), intent(in)  ::  minimum
    integer(k_int32), intent(in)  ::  maximum
    real                          ::  rn
    
    CALL RANDOM_NUMBER(rn)   
    rn = rn * (maximum + 1 - minimum)
    getRandom32 =  int(rn) + minimum
end function

program fnHexUtilTest
    implicit none
    logical  ::  error
    
    call hexToInt32Test(error)
    if ( error ) stop 1
    call hexToInt32TrueErrorTest(error)
    if ( error ) stop 1
    call hexCompareAsInt32Test(error)
    if ( error ) stop 1
    call hexInt32OrSmallerTest(error)
    if ( error ) stop 1
end program

subroutine hexToInt32Test(error)
    use fnHexUtil
    use fnInt32Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "hexToInt32Test"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 15, maxlen = 8, maxlastdigit = 15
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=8)                       ::  hString
    character(len=9)                       ::  hString2
    integer(k_int32)                       ::  getRandom32, n1, n2, i, i2, start, nlen, llen
    logical                                ::  errorLogical
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                             " &
                       // "                                             " &
                       // "          "

        start = getRandom32(1, slen - maxlen)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

        if ( nlen == maxlen ) then 
            nStringArray(i)(start:start) = fndigits(getRandom32(0, maxlastdigit))
            start = start + 1
            nlen = nlen - 1
        end if

        do while ( nlen > 0 )
            nStringArray(i)(start:start) = fndigits(getRandom32(0, maxdigit))
            start = start + 1
            nlen = nlen - 1 
        end do

        i = i + 1
    end do
    print *, "*Print 2 random data out of '", rCase , "' to see if they alright."
    print *, "'",nStringArray(1),"'"
    print *, "'",nStringArray(rCase),"'"
    print *, "*The largest random data has a length of '", llen ,"'."
    print *, lf, lf, lf

    print *, "# Test 1: Trimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )

        nString = ADJUSTL(nStringArray(i))
        call hexToInt32(TRIM(nString), n1, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            hString2 = int32ToHex(n1)
            if ( hString2(9:9) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString2
                return
            end if     
            nString = ADJUSTL(hString2)
        else
            hString = int32ToHexAsU(n1)
            if ( hString(8:8) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString
                return
            end if
            nString = ADJUSTL(hString)
        end if
        
        call hexToInt32(TRIM(nString), n2, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if

        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        i = i + 1
    end do 
    ! 2147483647
    call hexToInt32("7fffffff", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(2147483647)."
        print *, n1
        return 
    end if
    if ( n1 /= 2147483647 ) then
        print *, "Test Failed. Parsing not match value(2147483647)."
        print *, n1 
        return 
    end if
    ! -1
    call hexToInt32("ffffffff", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0 
    call hexToInt32("0", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0 ) then
        print *, "Test Failed. Parsing not match value(0)."
        print *, n1 
        return 
    end if
    ! Character equality 
    call hexToInt32("abcdef", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Logical test Failed at character equality (lower case)."
        print *, n1
        return 
    end if
    call hexToInt32("ABCDEF", n2, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Logical test Failed at character equality (upper case)."
        print *, n1
        return 
    end if
    if ( n1 /= n2 ) then
        print *, "Test Failed. Parsing not match value at character equality."
        print *, n1 
        return 
    end if
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        call hexToInt32(nString, n1, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i), lf, nString, lf, n1
            error = .TRUE.
            return 
        end if
        
        nString = int32ToHexAsU(n1)
        call hexToInt32(nString, n2, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i), lf, nString, lf, n2
            error = .TRUE.
            return 
        end if
        
        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, nStringArray(i)
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if
        i = i + 1
    end do 
    print *, "# Test 2: Passed."
    print *, lf, lf, lf
        
    print *, "# TEST 3: Parsing errors."
    ! 1 too many & 1 digit
    call hexToInt32("100000000", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    !Incorrect digit.
    call hexToInt32(char(0), n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call hexToInt32("/", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at /"
        return
    end if
    call hexToInt32(":", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at :"
        return
    end if
    call hexToInt32("@", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at @"
        return
    end if
    call hexToInt32("G", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at G"
        return
    end if
    call hexToInt32("`", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at `"
        return
    end if
    call hexToInt32("g", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at `"
        return
    end if    
    ! Signs where unsigned.
    call hexToInt32("-0", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at -0"
        return
    end if
    call hexToInt32("+0", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    call hexToInt32("      ", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call hexToInt32("", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf
    
    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call hexToInt32("months-f1a7b", n1, errorLogical, 8, 13)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-f1a7b", n1
        return        
    else 
        if ( n1 /= 989819 ) then 
            print *, "Test 4 failed parse at ", "months-f1a7b", n1
            return 
        end if
    end if
    call hexToInt32("data-range:0xf1a249b-ext", n1, errorLogical, 14, 20)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "data-range:0xf1a249b-ext", n1
        return        
    else 
        if ( n1 /= 253371547 ) then 
            print *, "Test 4 failed parse at ", "data-range:0xf1a249b-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    call hexToInt32("00000", n1, errorLogical, 2, 1)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "00000"
        return 
    end if
    call hexToInt32("0000000", n1, errorLogical, 8, 20)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000"
        return 
    end if
    call hexToInt32("0000000000000000", n2, errorLogical, -15, 0)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine hexToInt32TrueErrorTest(error)
    use fnHexUtil
    use fnInt32Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "hexToInt32TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 15, maxlen = 8, maxlastdigit = 15
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=8)                       ::  hString
    character(len=9)                       ::  hString2
    integer(k_int32)                       ::  getRandom32, n1, n2, i, &
                                               i2, start, nlen, llen, errorInt
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                             " &
                       // "                                             " &
                       // "          "


        start = getRandom32(1, slen - maxlen)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

        if ( nlen == maxlen ) then 
            nStringArray(i)(start:start) = fndigits(getRandom32(0, maxlastdigit))
            start = start + 1
            nlen = nlen - 1
        end if

        do while ( nlen > 0 ) 
            nStringArray(i)(start:start) = fndigits(getRandom32(0, maxdigit))
            start = start + 1
            nlen = nlen - 1 
        end do  

        i = i + 1
    end do
    print *, "*Print 2 random data out of '", rCase , "' to see if they alright."
    print *, "'",nStringArray(1),"'"
    print *, "'",nStringArray(rCase),"'"
    print *, "*The largest random data has a length of '", llen ,"'."
    print *, lf, lf, lf

    print *, "# Test 1: Trimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = ADJUSTL(nStringArray(i))
        
        call hexToInt32TrueError(TRIM(nString), n1, errorInt)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            hString2 = int32ToHex(n1)
            if ( hString2(9:9) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString2
                return
            end if
            nString = ADJUSTL(hString2)
        else
            hString = int32ToHexAsU(n1)
            if ( hString(8:8) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString
                return
            end if
            nString = ADJUSTL(hString)
        end if
        
        call hexToInt32TrueError(TRIM(nString), n2, errorInt)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        i = i + 1
    end do 
    ! 2147483647
    call hexToInt32TrueError("7fffffff", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(2147483647)."
        print *, n1
        return 
    end if
    if ( n1 /= 2147483647 ) then
        print *, "Test Failed. Parsing not match value(2147483647)."
        print *, n1 
        return 
    end if
    ! -1
    call hexToInt32TrueError("ffffffff", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0
    call hexToInt32TrueError("0", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0 ) then
        print *, "Test Failed. Parsing not match value(0)."
        print *, n1 
        return 
    end if
    ! Character equality
    call hexToInt32TrueError("abcdef", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Logical test Failed at character equality (lower case)."
        print *, n1
        return 
    end if
    call hexToInt32TrueError("ABCDEF", n2, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Logical test Failed at character equality (upper case)."
        print *, n1
        return 
    end if
    if ( n1 /= n2 ) then
        print *, "Test Failed. Parsing not match value at character equality."
        print *, n1 
        return 
    end if
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        call hexToInt32TrueError(nString, n1, errorInt)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if

        nString = int32ToHexAsU(n1)
        call hexToInt32TrueError(nString, n2, errorInt)
        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if
        
        i = i + 1
    end do 
    print *, "# Test 2: Passed."
    print *, lf, lf, lf

    print *, "# TEST 3: Parsing errors, test error return value."
    ! 1 too many & 1 digit too many
    call hexToInt32TrueError("100000000", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    !Incorrect digit
    call hexToInt32TrueError(char(0), n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call hexToInt32TrueError("/", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if
    call hexToInt32TrueError(":", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at :"
        return
    end if
    call hexToInt32TrueError("@", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at @"
        return
    end if
    call hexToInt32TrueError("G", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at G"
        return
    end if
    call hexToInt32TrueError("100000000`", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 100000000`"
        return
    end if
    call hexToInt32TrueError("100000000g", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 100000000g"
        return
    end if
    ! Signs where unsigned
    call hexToInt32TrueError("-0", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -0"
        return
    end if
    call hexToInt32TrueError("+0", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    call hexToInt32TrueError("      ", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call hexToInt32TrueError("", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call hexToInt32TrueError("months-f1a7b", n1, errorInt, 8, 13)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "months-f1a7b", n2
        return        
    else 
        if ( n1 /= 989819 ) then 
            print *, "Test 4 failed parse at ", "months-f1a7b", n1
            return 
        end if
    end if
    call hexToInt32TrueError("data-range:0xf1a249b-ext", n1, errorInt, 14, 20)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "data-range:0xf1a249b-ext", n1
        return        
    else 
        if ( n1 /= 253371547 ) then 
            print *, "Test 4 failed parse at ", "data-range:0xf1a249b-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    call hexToInt32TrueError("00000000", n1, errorInt, 2, 1)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "00000000"
        return 
    end if
    call hexToInt32TrueError("0000", n1, errorInt, 8, 20)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "0000"
        return 
    end if
    call hexToInt32TrueError("12345678", n2, errorInt, -15, 0)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "000000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine hexCompareAsInt32Test(error)
    use fnHexUtil
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "hexCompareAsInt32Test"
    integer(k_int32) , dimension(15)  ::  rExp
    character(len=32), dimension(15)  ::  lvalue
    character(len=32), dimension(15)  ::  rvalue
    logical                           ::  errorLogical
    integer(k_int32)                  ::  n1, i
    error = .TRUE.

    ! Prep Cases
    ! 1
    lvalue(1) = "f1"
    rvalue(1) = "0"
    rExp(1)   = 1

    lvalue(2) = "1f50"
    rvalue(2) = "1f"
    rExp(2)   = 1

    lvalue(3) = "a7f5b"
    rvalue(3) = "a7f5a"
    rExp(3)   = 1

    lvalue(4) = "abcd4"
    rvalue(4) = "A"
    rExp(4)   = 1

    ! Note that hex are being parsed as they are bit representation
    ! to the signed int type. Thus,               80000001
    ! is actually -2147483647 and is smaller than 7fffffff
    ! and is 2147483647
    lvalue(5) = "7fffffff"
    rvalue(5) = "80000001"
    rExp(5)   = 1

    ! 0
    lvalue(6) = "c7c9"
    rvalue(6) = "C7c9"
    rExp(6)   = 0

    lvalue(7) = "ffffffff"
    rvalue(7) = "ffffffff"
    rExp(7)   = 0

    lvalue(8) = "afc0052"
    rvalue(8) = "afc0052"
    rExp(8)   = 0

    lvalue(9) = "0"
    rvalue(9) = "00000"
    rExp(9)   = 0

    lvalue(10) = "000000"
    rvalue(10) = "000"
    rExp(10)   = 0

    ! -1
    lvalue(11) = "0"
    rvalue(11) = "f1"
    rExp(11)   = -1

    lvalue(12) = "f15"
    rvalue(12) = "1254fc"
    rExp(12)   = -1

    lvalue(13) = "f079a"
    rvalue(13) = "f1578"
    rExp(13)   = -1

    lvalue(14) = "fffeffff"
    rvalue(14) = "ffffffff"
    rExp(14)   = -1

    lvalue(15) = "f9cdeff"
    rvalue(15) = "fabcdef"
    rExp(15)   = -1
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Right format comparision."
    i = 1
    do while ( i <= 15 )
        call hexCompareAsInt32(lvalue(i), rvalue(i), n1, errorLogical)
        if ( errorLogical .neqv. .FALSE. ) then
            print *, "---Test 2 fail logical at case '", i ,"'. Got error."
            return 
        end if
        if ( n1 /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '", n1, "'"
            return
        end if
        i = i + 1
    end do 
    print *, "# TEST 1 Passed."
    print *, lf, lf

    print *, "# TEST 2: Errors(Errors suppose to be true)."
    ! Empty string
    call hexCompareAsInt32("10", "", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "10 to empty"
        return 
    end if
    call hexCompareAsInt32("", "10", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "empty to 10"
        return 
    end if
    ! Incorrect format
    call hexCompareAsInt32("11g", "110", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "11g", "110"
        return 
    end if
    call hexCompareAsInt32("110", "11g", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "110", "11g"
        return 
    end if
    ! Beyond capacity
    call hexCompareAsInt32("11111", "100000000", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "11111", "100000000"
        return 
    end if
    call hexCompareAsInt32("100000000", "11111", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "100000000", "11111"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine hexInt32OrSmallerTest(error)
    use fnHexUtil
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "hexInt32OrSmallerTest"
    integer(k_int32) , dimension(23)  ::  rExp
    character(len=40), dimension(23)  ::  lvalue
    character(len=40), dimension(23)  ::  rvalue
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    ! Invalid to
    lvalue(1) = "112/"
    rvalue(1) = "-0"
    rExp(1)   = 0

    lvalue(2) = "123:"
    rvalue(2) = "100000000"
    rExp(2)   = -1

    lvalue(3) = "01g"
    rvalue(3) = ""
    rExp(3)   = -1

    lvalue(4) = "01G"
    rvalue(4) = "     "
    rExp(4)   = -1

    lvalue(5) = " 1 1 "
    rvalue(5) = "7"
    rExp(5)   = -1

    ! Beyond capacity to
    lvalue(6) = "100000000"
    rvalue(6) = "/1"
    rExp(6)   = 1

    lvalue(7) = "100000000"
    rvalue(7) = "1000000000"
    rExp(7)   = 0

    lvalue(8) = "100000000"
    rvalue(8) = "      "
    rExp(8)   = -1

    lvalue(9) = "100000000"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "100000000"
    rvalue(10) = "7"
    rExp(10)   = -1

    ! Empty to
    lvalue(11) = ""
    rvalue(11) = "11@"
    rExp(11)   = 1

    lvalue(12) = "           "
    rvalue(12) = "11`"
    rExp(12)   = 1

    lvalue(13) = "           "
    rvalue(13) = "100000000"
    rExp(13)   = 1

    lvalue(14) = "      "
    rvalue(14) = ""
    rExp(14)   = 0

    lvalue(15) = "      "
    rvalue(15) = "7"
    rExp(15)   = -1
    ! Valid to
    lvalue(16) = "b25a"
    rvalue(16) = "1 11"
    rExp(16)   = 1

    lvalue(17) = "b25a"
    rvalue(17) = "100000000"
    rExp(17)   = 1
    
    lvalue(18) = "b25a"
    rvalue(18) = ""
    rExp(18)   = 1

    lvalue(19) = "b25a"
    rvalue(19) = "    "
    rExp(19)   = 1

    lvalue(20) = "b25a"
    rvalue(20) = "00b25a"
    rExp(20)   = 0

    lvalue(21) = "00b25a"
    rvalue(21) = "b25a"
    rExp(21)   = 0

    lvalue(22) = "fc4258"
    rvalue(22) = "Fb898F"
    rExp(22)   = 1

    lvalue(23) = "abcdeF"
    rvalue(23) = "AfabcD"
    rExp(23)   = -1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Case to case."
    i = 1
    do while ( i <= 23 )
        if ( hexInt32OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , hexInt32OrSmaller(lvalue(i), rvalue(i)), "'"
            return
        end if
        i = i + 1
    end do 
    print *, "TEST 1 Passed."
    print *, lf, lf

    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
    error = .FALSE.
end subroutine

pure subroutine fnHexUtilTestPure()
    use fnHexUtil
    implicit none
    logical           ::  el
    integer(k_int32)  ::  ei, r

    call hexToInt32("0", r, el)
    call hexToInt32TrueError("0",  r, ei)
    call hexCompareAsInt32("0", "0", r, el)
    r = hexInt32OrSmaller("1","1")
end subroutine
