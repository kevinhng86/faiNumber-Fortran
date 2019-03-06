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

! This test file test fnHexUtil64 plus the int64ToHexAsU and the
! int64ToHex function
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

program fnHexUtil64Test
    implicit none
    logical  ::  error
    
    call hexToInt64Test(error)
    if ( error ) stop
    call hexToInt64TrueErrorTest(error)
    if ( error ) stop
    call hexCompareAsInt64Test(error)
    if ( error ) stop
    call hexInt64OrSmallerTest(error)
    if ( error ) stop
end program

subroutine hexToInt64Test(error)
    use fnHexUtil64
    use fnInt64Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "hexToInt64Test"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 15, maxlen = 16, maxlastdigit = 15
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=16)                      ::  hString
    character(len=17)                      ::  hString2
    integer(k_int32)                       ::  getRandom32, i, i2, start, nlen, llen
    integer(k_int64)                       ::  n1, n2
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
        errorLogical = hexToInt64(TRIM(nString), n1)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            hString2 = int64ToHex(n1)
            if ( hString2(17:17) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString2
                return
            end if     
            nString = ADJUSTL(hString2)
        else
            hString = int64ToHexAsU(n1)
            if ( hString(16:16) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString
                return
            end if
            nString = ADJUSTL(hString)
        end if
        
        errorLogical = hexToInt64(TRIM(nString), n2)
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
    ! 9223372036854775807
    errorLogical = hexToInt64("7fffffffffffffff", n1)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(9223372036854775807)."
        print *, n1
        return 
    end if
    if ( n1 /= 9223372036854775807_k_int64 ) then
        print *, "Test Failed. Parsing not match value(9223372036854775807)."
        print *, n1 
        return 
    end if
    ! -1
    errorLogical = hexToInt64("ffffffffffffffff", n1)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1_k_int64 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0 
    errorLogical = hexToInt64("0", n1)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0_k_int64 ) then
        print *, "Test Failed. Parsing not match value(0)."
        print *, n1 
        return 
    end if
    ! Character equality 
    errorLogical = hexToInt64("abcdef", n1)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Logical test Failed at character equality (lower case)."
        print *, n1
        return 
    end if
    errorLogical = hexToInt64("ABCDEF", n2)
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
        errorLogical = hexToInt64(nString, n1)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i), lf, nString, lf, n1
            error = .TRUE.
            return 
        end if
        
        nString = int64ToHexAsU(n1)
        errorLogical = hexToInt64(nString, n2)
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
    if ( hexToInt64("10000000000000000", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    !Incorrect digit.
    if ( hexToInt64(char(0), n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at null char"
        return
    end if   
    if ( hexToInt64("/", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at /"
        return
    end if   
    if ( hexToInt64(":", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at :"
        return
    end if
    if ( hexToInt64("@", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at @"
        return
    end if
    if ( hexToInt64("G", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at G"
        return
    end if 
    if ( hexToInt64("`", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at `"
        return
    end if
    if ( hexToInt64("g", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at `"
        return
    end if    
    ! Signs where unsigned.
    if ( hexToInt64("-0", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at -0"
        return
    end if
    if ( hexToInt64("+0", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    if ( hexToInt64("      ", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at empty"
        return
    end if
    if ( hexToInt64("", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf
    
    print *, "# Test 4: Positioning test."
    ! Sucess cases
    if ( hexToInt64("months-f1a7b", n1, 8, 13) .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-f1a7b", n1
        return        
    else 
        if ( n1 /= 989819_k_int64 ) then 
            print *, "Test 4 failed parse at ", "months-f1a7b", n1
            return 
        end if
    end if                                
    if ( hexToInt64("data-range:0xf1a249b-ext", n1, 14, 20) .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "data-range:0xf1a249b-ext", n1
        return        
    else 
        if ( n1 /= 253371547_k_int64 ) then 
            print *, "Test 4 failed parse at ", "data-range:0xf1a249b-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    if ( hexToInt64("00000", n1, 2, 1) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "00000"
        return 
    end if 
    if ( hexToInt64("0000000", n1, 8, 20) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000"
        return 
    end if 
    if ( hexToInt64("0000000000000000", n2, -15, 0) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine hexToInt64TrueErrorTest(error)
    use fnHexUtil64
    use fnInt64Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "hexToInt64TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 15, maxlen = 16, maxlastdigit = 15
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=16)                      ::  hString
    character(len=17)                      ::  hString2
    integer(k_int32)                       ::  getRandom32, i, &
                                               i2, start, nlen, llen, errorInt
    integer(k_int64)                       ::  n1, n2

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
        
        errorInt = hexToInt64TrueError(TRIM(nString), n1)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            hString2 = int64ToHex(n1)
            if ( hString2(17:17) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString2
                return
            end if
            nString = ADJUSTL(hString2)
        else
            hString = int64ToHexAsU(n1)
            if ( hString(16:16) == charspace ) then
                print *, "Something is wrong with the hex function"
                print *, n1, lf, hString
                return
            end if
            nString = ADJUSTL(hString)
        end if
        
        errorInt = hexToInt64TrueError(TRIM(nString), n2)
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
    ! 9223372036854775807
    errorInt = hexToInt64TrueError("7fffffffffffffff", n1)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(9223372036854775807)."
        print *, n1
        return 
    end if
    if ( n1 /= 9223372036854775807_k_int64 ) then
        print *, "Test Failed. Parsing not match value(9223372036854775807)."
        print *, n1 
        return 
    end if
    ! -1
    errorInt = hexToInt64TrueError("ffffffffffffffff", n1)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1_k_int64 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0
    errorInt = hexToInt64TrueError("0", n1)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0_k_int64 ) then
        print *, "Test Failed. Parsing not match value(0)."
        print *, n1 
        return 
    end if
    ! Character equality 
    errorInt = hexToInt64TrueError("abcdef", n1)
    if ( errorInt /= 0 ) then
        print *, "Logical test Failed at character equality (lower case)."
        print *, n1
        return 
    end if
    errorInt = hexToInt64TrueError("ABCDEF", n2)
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
        errorInt = hexToInt64TrueError(nString, n1)        
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if

        nString = int64ToHexAsU(n1)
        errorInt = hexToInt64TrueError(nString, n2)  
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
    if ( hexToInt64TrueError("10000000000000000", n1) /= 3 ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    !Incorrect digit
    if ( hexToInt64TrueError(char(0), n1) /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if   
    if ( hexToInt64TrueError("/", n1) /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if
    if ( hexToInt64TrueError(":", n1) /= 2 ) then
        print *, "Test 3 fail at :"
        return
    end if   
    if ( hexToInt64TrueError("@", n1) /= 2 ) then
        print *, "Test 3 fail at @"
        return
    end if
    if ( hexToInt64TrueError("G", n1) /= 2 ) then
        print *, "Test 3 fail at G"
        return
    end if
    if ( hexToInt64TrueError("10000000000000000`", n1) /= 2 ) then
        print *, "Test 3 fail at 10000000000000000`"
        return
    end if
    if ( hexToInt64TrueError("10000000000000000g", n1) /= 2 ) then
        print *, "Test 3 fail at 10000000000000000g"
        return
    end if
    ! Signs where unsigned
    if ( hexToInt64TrueError("-0", n1) /= 2 ) then
        print *, "Test 3 fail at -0"
        return
    end if
    if ( hexToInt64TrueError("+0", n1) /= 2 ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    if ( hexToInt64TrueError("      ", n1) /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    if ( hexToInt64TrueError("", n1) /= 1 ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    if ( hexToInt64TrueError("months-f1a7b", n1, 8, 13) /= 0 ) then
        print *, "Test 4 failed logical at ", "months-f1a7b", n2
        return        
    else 
        if ( n1 /= 989819 ) then 
            print *, "Test 4 failed parse at ", "months-f1a7b", n1
            return 
        end if
    end if
    if ( hexToInt64TrueError("data-range:0xf1a249b-ext", n1, 14, 20) /= 0 ) then
        print *, "Test 4 failed logical at ", "data-range:0xf1a249b-ext", n1
        return        
    else 
        if ( n1 /= 253371547 ) then 
            print *, "Test 4 failed parse at ", "data-range:0xf1a249b-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    if ( hexToInt64TrueError("00000000", n1, 2, 1) /= 5 ) then
        print *, "Test 4 failed at ", "00000000"
        return 
    end if 
    if ( hexToInt64TrueError("0000", n1, 8, 20) /= 5 ) then
        print *, "Test 4 failed at ", "0000"
        return 
    end if 
    if ( hexToInt64TrueError("12345678", n2, -15, 0) /= 5 ) then
        print *, "Test 4 failed at ", "000000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine hexCompareAsInt64Test(error)
    use fnHexUtil64
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "hexCompareAsInt64Test"
    integer(k_int32) , dimension(15)  ::  rExp
    character(len=40), dimension(15)  ::  lvalue
    character(len=40), dimension(15)  ::  rvalue
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
    ! to the signed int type. Thus, the first hex digit below is smaller
    ! than the second.
    !              
    ! 8000000000000001(-9223372036854775807)
    ! 7fffffffffffffff( 9223372036854775807)
    lvalue(5) = "7fffffffffffffff"
    rvalue(5) = "8000000000000001"
    rExp(5)   = 1

    ! 0
    lvalue(6) = "c7c9"
    rvalue(6) = "C7c9"
    rExp(6)   = 0

    lvalue(7) = "ffffffffffffffff"
    rvalue(7) = "ffffffffffffffff"
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

    lvalue(14) = "fffffffeffffffff"
    rvalue(14) = "ffffffffffffffff"
    rExp(14)   = -1

    lvalue(15) = "f9cdeff"
    rvalue(15) = "fabcdef"
    rExp(15)   = -1
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Right format comparision."
    i = 1
    do while ( i <= 15 )
        if ( hexCompareAsInt64(lvalue(i), rvalue(i), n1) .neqv. .FALSE. ) then
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
    if ( hexCompareAsInt64("10", "", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "10 to empty"
        return 
    end if
    if ( hexCompareAsInt64("", "10", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "empty to 10"
        return 
    end if
    ! Incorrect format
    if ( hexCompareAsInt64("11g", "110", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "11g", "110"
        return 
    end if
    if ( hexCompareAsInt64("110", "11g", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "110", "11g"
        return 
    end if
    ! Beyond capacity         
    if ( hexCompareAsInt64("11111", "10000000000000000", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "11111", "10000000000000000"
        return 
    end if
    if ( hexCompareAsInt64("10000000000000000", "11111", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "10000000000000000", "11111"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine hexInt64OrSmallerTest(error)
    use fnHexUtil64
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "hexInt64OrSmallerTest"
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
    rvalue(2) = "10000000000000000"
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
    lvalue(6) = "10000000000000000"
    rvalue(6) = "/1"
    rExp(6)   = 1

    lvalue(7) = "10000000000000000"
    rvalue(7) = "100000000000000000"
    rExp(7)   = 0

    lvalue(8) = "10000000000000000"
    rvalue(8) = "      "
    rExp(8)   = -1

    lvalue(9) = "10000000000000000"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "10000000000000000"
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
    rvalue(13) = "10000000000000000"
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
    rvalue(17) = "10000000000000000"
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
        if ( hexInt64OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , hexInt64OrSmaller(lvalue(i), rvalue(i)), "'"
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
