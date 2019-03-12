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

! This test file test fniBinaryUtil plus the int32ToBinaryAsU and the 
! int32ToBinary function
integer(k_int32) function getRandom32 (minimum, maximum)
    use fniConsts
    implicit none
    integer(k_int32), intent(in)  ::  minimum
    integer(k_int32), intent(in)  ::  maximum
    real                          ::  rn
    
    CALL RANDOM_NUMBER(rn)   
    rn = rn * (maximum + 1 - minimum)
    getRandom32 =  int(rn) + minimum
end function

program fniBinaryUtilTest
    implicit none
    logical  ::  error
    
    call binaryToInt32Test(error)
    if ( error ) stop 1
    call binaryToInt32TrueErrorTest(error)
    if ( error ) stop 1
    call binaryCompareAsInt32Test(error)
    if ( error ) stop 1
    call binaryInt32OrSmallerTest(error)
    if ( error ) stop 1
end program

subroutine binaryToInt32Test(error)
    use fniBinaryUtil
    use fniInt32Util
    implicit none
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "binaryToInt32Test"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 130
    integer(k_int32)   , parameter         ::  maxdigit = 1, maxlen = 32
    logical            , intent(out)       ::  error
    character(len=slen)                    ::  nString
    character(len=32)                      ::  bString
    character(len=33)                      ::  bString2
    integer(k_int32)                       ::  getRandom32, n1, n2, i, i2, start, nlen, llen
    character(len=slen), dimension(rCase)  ::  nStringArray
    logical                                ::  errorLogical
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                                           " &
                       // "                                                           " &
                       // "            " 
                       
        start = getRandom32(1, slen - maxlen)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

        do while ( nlen > 0 )
            nStringArray(i)(start:start) = char(getRandom32(0, maxdigit) + 48)
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
        errorLogical = binaryToInt32(TRIM(nString), n1)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            bString2 = int32ToBinary(n1)
            if ( bString2(33:33) == charspace ) then
                print *, "Something is wrong with the binary function"
                print *, n1, lf, bString2
                return
            end if     
            nString = ADJUSTL(bString2)
        else
            bString = int32ToBinaryAsU(n1) 
            if ( bString(32:32) == charspace ) then
                print *, "Something is wrong with the binary function"
                print *, n1, lf, bString
                return
            end if
            nString = ADJUSTL(bString)
        end if
        
        errorLogical = binaryToInt32(TRIM(nString), n2)
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
    errorLogical = binaryToInt32("1111111111111111111111111111111", n1)
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
    errorLogical = binaryToInt32("11111111111111111111111111111111", n1)
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
    errorLogical = binaryToInt32("0", n1)
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
    
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        errorLogical = binaryToInt32(nString, n1)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i), lf, nString, lf, n1
            error = .TRUE.
            return 
        end if
        
        nString = int32ToBinaryAsU(n1)
        errorLogical = binaryToInt32(nString, n2)
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
    ! 1 too many and 1 digit too many
    if ( binaryToInt32("100000000000000000000000000000000", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    !Incorrect digit.
    if ( binaryToInt32(char(0), n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at null char"
        return
    end if   
    if ( binaryToInt32("/", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at /"
        return
    end if   
    if ( binaryToInt32("2", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at 2"
        return
    end if   
    ! Signs where unsigned.
    if ( binaryToInt32("-0", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at -0"
        return
    end if
    if ( binaryToInt32("+0", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    if ( binaryToInt32("      ", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at empty"
        return
    end if
    if ( binaryToInt32("", n1) .neqv. .TRUE. ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf
    
    print *, "# Test 4: Positioning test."
    ! Sucess cases
    if ( binaryToInt32("months-10110", n1, 8, 13) .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-10110", n1
        return        
    else 
        if ( n1 /= 22 ) then 
            print *, "Test 4 failed parse at ", "months-10110", n1
            return 
        end if
    end if                                
    if ( binaryToInt32("data-range:1111010-ext", n1, 12, 18) .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "data-range:1111010-ext", n1
        return        
    else 
        if ( n1 /= 122 ) then 
            print *, "Test 4 failed parse at ", "data-range:1111010-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    if ( binaryToInt32("00000", n1, 2, 1) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "00000"
        return 
    end if 
    if ( binaryToInt32("0000000", n1, 8, 20) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000"
        return 
    end if 
    if ( binaryToInt32("0000000000000000", n2, -15, 0) .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine binaryToInt32TrueErrorTest(error)
    use fniBinaryUtil
    use fniInt32Util
    implicit none
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "binaryToInt32TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 130
    integer(k_int32)   , parameter         ::  maxdigit = 1, maxlen = 32
    logical            , intent(out)       ::  error
    character(len=slen)                    ::  nString
    character(len=32)                      ::  bString
    character(len=33)                      ::  bString2    
    integer(k_int32)                       ::  getRandom32, n1, n2, i, &
                                               i2, start, nlen, llen, errorInt
    character(len=slen), dimension(rCase)  ::  nStringArray
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                                           " &
                       // "                                                           " &
                       // "            " 
                       
        start = getRandom32(1, slen - maxlen)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

        do while ( nlen > 0 ) 
            nStringArray(i)(start:start) = char(getRandom32(0, maxdigit) + 48)
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
        
        errorInt = binaryToInt32TrueError(TRIM(nString), n1)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            bString2 = int32ToBinary(n1)
            if ( bString2(33:33) == charspace ) then
                print *, "Something is wrong with the binary function"
                print *, n1, lf, bString2
                return
            end if     
            nString = ADJUSTL(bString2)
        else
            bString = int32ToBinaryAsU(n1) 
            if ( bString(32:32) == charspace ) then
                print *, "Something is wrong with the binary function"
                print *, n1, lf, bString
                return
            end if
            nString = ADJUSTL(bString)
        end if
        
        errorInt = binaryToInt32TrueError(TRIM(nString), n2)
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
    errorInt = binaryToInt32TrueError("1111111111111111111111111111111", n1)
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
    errorInt = binaryToInt32TrueError("11111111111111111111111111111111", n1)
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
    errorInt = binaryToInt32TrueError("0", n1)
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
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        errorInt = binaryToInt32TrueError(nString, n1)        
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if

        nString = int32ToBinaryAsU(n1)
        errorInt = binaryToInt32TrueError(nString, n2)  
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
    ! 1 too many and 1 digit too many
    if ( binaryToInt32TrueError("100000000000000000000000000000000", n1) /= 3 ) then
        print *, "Test 3 fail at ", "100000000000000000000000000000000"
        return
    end if
    !Incorrect digit
    if ( binaryToInt32TrueError(char(0), n1) /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if   
    if ( binaryToInt32TrueError("/", n1) /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if   
    if ( binaryToInt32TrueError("1111111111111111111111111111111111112", n1) /= 2 ) then
        print *, "Test 3 fail at 1111111111111111111111111111111111112"
        return
    end if
    if ( binaryToInt32TrueError("100000000000000000000000000000000000:", n1) /= 2 ) then
        print *, "Test 3 fail at 100000000000000000000000000000000000:"
        return
    end if
    ! Signs where unsigned
    if ( binaryToInt32TrueError("-0", n1) /= 2 ) then
        print *, "Test 3 fail at -0"
        return
    end if
    if ( binaryToInt32TrueError("+0", n1) /= 2 ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    if ( binaryToInt32TrueError("      ", n1) /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    if ( binaryToInt32TrueError("", n1) /= 1 ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    if ( binaryToInt32TrueError("months-10110", n1, 8, 13) /= 0 ) then
        print *, "Test 4 failed logical at ", "months-10110", n2
        return        
    else 
        if ( n1 /= 22 ) then 
            print *, "Test 4 failed parse at ", "months-10110", n1
            return 
        end if
    end if
    if ( binaryToInt32TrueError("data-range:1111010-ext", n1, 12, 18) /= 0 ) then
        print *, "Test 4 failed logical at ", "data-range:1111010-ext", n1
        return        
    else 
        if ( n1 /= 122 ) then 
            print *, "Test 4 failed parse at ", "data-range:1111010-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    if ( binaryToInt32TrueError("00000000", n1, 2, 1) /= 5 ) then
        print *, "Test 4 failed at ", "00000000"
        return 
    end if 
    if ( binaryToInt32TrueError("0000", n1, 8, 20) /= 5 ) then
        print *, "Test 4 failed at ", "0000"
        return 
    end if 
    if ( binaryToInt32TrueError("12345678", n2, -15, 0) /= 5 ) then
        print *, "Test 4 failed at ", "000000000000000000"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine binaryCompareAsInt32Test(error)
    use fniBinaryUtil
    use fniInt32Util
    implicit none
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "binaryCompareAsInt32Test"
    logical          , intent(out)    ::  error
    integer(k_int32) , dimension(15)  ::  rExp
    character(len=32), dimension(15)  ::  lvalue
    character(len=32), dimension(15)  ::  rvalue
    integer(k_int32)                  ::  n1, i
    error = .TRUE.

    ! Prep Cases
    ! 1
    lvalue(1) = "10111"
    rvalue(1) = "0"
    rExp(1)   = 1

    lvalue(2) = "1111101000000"
    rvalue(2) = "101011110"
    rExp(2)   = 1

    lvalue(3) = "1111101000000"
    rvalue(3) = "101011110"
    rExp(3)   = 1

    lvalue(4) = "1111101000000"
    rvalue(4) = "1111100111111"
    rExp(4)   = 1

    ! Note that binary are being parsed as they are bit representation
    ! to the signed int type. Thus, 10000000000000000000000000000001
    ! is actually -2147483647 and is smaller than 1111111111111111111111111111111
    ! (1 digit less) and is 2147483647
    lvalue(5) = "1111111111111111111111111111111"
    rvalue(5) = "10000000000000000000000000000001"
    rExp(5)   = 1

    ! 0
    lvalue(6) = "10111"
    rvalue(6) = "10111"
    rExp(6)   = 0

    lvalue(7) = "11111111111111111111111111111111"
    rvalue(7) = "11111111111111111111111111111111"
    rExp(7)   = 0

    lvalue(8) = "10101111000000000100011010"
    rvalue(8) = "10101111000000000100011010"
    rExp(8)   = 0

    lvalue(9) = "0"
    rvalue(9) = "00000"
    rExp(9)   = 0

    lvalue(10) = "000000"
    rvalue(10) = "000"
    rExp(10)   = 0

    ! -1
    lvalue(11) = "0"
    rvalue(11) = "11100110011001"
    rExp(11)   = -1

    lvalue(12) = "10111111111111100000"
    rvalue(12) = "11001101100000111111"
    rExp(12)   = -1

    lvalue(13) = "1111100111111"
    rvalue(13) = "1111101000000"
    rExp(13)   = -1

    lvalue(14) = "11111111111001111111111111111111"
    rvalue(14) = "11111111111111111111111111111111"
    rExp(14)   = -1

    lvalue(15) = "110100011000101000111000100100"
    rvalue(15) = "1101000110001010001110001000110"
    rExp(15)   = -1
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Right format comparision."
    i = 1
    do while ( i <= 15 )
        if ( BinaryCompareAsInt32(lvalue(i), rvalue(i), n1) .neqv. .FALSE. ) then
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
    if ( BinaryCompareAsInt32("10", "", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "10 to empty"
        return 
    end if
    if ( BinaryCompareAsInt32("", "10", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "empty to 10"
        return 
    end if
    ! Incorrect format
    if ( BinaryCompareAsInt32("112", "110", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "112", "110"
        return 
    end if
    if ( BinaryCompareAsInt32("110", "11`", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "110", "11`"
        return 
    end if
    ! Beyond capacity         
    if ( binaryCompareAsInt32("11111", "100000000000000000000000000000000", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "11111", "100000000000000000000000000000000"
        return 
    end if
    if ( binaryCompareAsInt32("100000000000000000000000000000000", "11111", n1) .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "100000000000000000000000000000000", "11111"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine binaryInt32OrSmallerTest(error)
    use fniBinaryUtil
    use fniInt32Util
    implicit none
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "binaryInt32OrSmallerTest"
    logical          , intent(out)    ::  error
    integer(k_int32) , dimension(23)  ::  rExp
    character(len=40), dimension(23)  ::  lvalue
    character(len=40), dimension(23)  ::  rvalue
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    ! Invalid to
    lvalue(1) = "12"
    rvalue(1) = "-0"
    rExp(1)   = 0

    lvalue(2) = "111:"
    rvalue(2) = "100000000000000000000000000000000"
    rExp(2)   = -1

    lvalue(3) = "01/"
    rvalue(3) = ""
    rExp(3)   = -1

    lvalue(4) = "01a"
    rvalue(4) = "     "
    rExp(4)   = -1

    lvalue(5) = " 1 1 "
    rvalue(5) = "01100"
    rExp(5)   = -1

    ! Beyond capacity to
    lvalue(6) = "100000000000000000000000000000000"
    rvalue(6) = "/1"
    rExp(6)   = 1

    lvalue(7) = "100000000000000000000000000000000"
    rvalue(7) = "10000000000000000000000000000000000"
    rExp(7)   = 0

    lvalue(8) = "100000000000000000000000000000000"
    rvalue(8) = "      "
    rExp(8)   = -1

    lvalue(9) = "100000000000000000000000000000000"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "100000000000000000000000000000000"
    rvalue(10) = "110011"
    rExp(10)   = -1

    ! Empty to
    lvalue(11) = ""
    rvalue(11) = "112"
    rExp(11)   = 1

    lvalue(12) = "           "
    rvalue(12) = "112"
    rExp(12)   = 1

    lvalue(13) = "           "
    rvalue(13) = "1000000000000000000000000000000000000000"
    rExp(13)   = 1

    lvalue(14) = "      "
    rvalue(14) = ""
    rExp(14)   = 0

    lvalue(15) = "      "
    rvalue(15) = "11001"
    rExp(15)   = -1
    ! Valid to
    lvalue(16) = "010"
    rvalue(16) = "111a"
    rExp(16)   = 1

    lvalue(17) = "010"
    rvalue(17) = "1000000000000000000000000000000000000000"
    rExp(17)   = 1
    
    lvalue(18) = "010"
    rvalue(18) = ""
    rExp(18)   = 1

    lvalue(19) = "010"
    rvalue(19) = "    "
    rExp(19)   = 1

    lvalue(20) = "1010"
    rvalue(20) = "001010"
    rExp(20)   = 0

    lvalue(21) = "001010"
    rvalue(21) = "1010"
    rExp(21)   = 0

    lvalue(22) = "11111111111111111111111111111111"
    rvalue(22) = "10111101111101111111101111111111"
    rExp(22)   = 1

    lvalue(23) = "1111011111111111111111111111111"
    rvalue(23) = "1111101111101111111101111111111"
    rExp(23)   = -1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Case to case."
    i = 1
    do while ( i <= 23 )
        if ( binaryInt32OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , binaryInt32OrSmaller(lvalue(i), rvalue(i)), "'"
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
