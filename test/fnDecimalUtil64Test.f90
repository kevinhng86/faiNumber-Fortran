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

program fnDecimalUtilTest64
    implicit none
    logical  ::  error
    
    call decToInt64Test(error)
    if ( error ) stop
    call decToInt64TrueErrorTest(error)
    if ( error ) stop
    call decCompareTest64(error)
    if ( error ) stop
    call decInt64OrSmallerTest(error)
    if ( error ) stop
end program

subroutine decToInt64Test(error)
    use fnDecimalUtil64
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "decToInt64Test"
    integer(k_int32)   , parameter         ::  rCase = 5000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 9, maxlen = 19, maxlastdigit = 8
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    integer(k_int32)                       ::  getRandom32, i, i2, start, &
                                               nlen, llen
    integer(k_int64)                       ::  n1, n2
    logical                                ::  errorLogical
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                                   " &
                       // "                                                 "

        start = getRandom32(1, slen - maxlen - 1)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen
        
        if ( getRandom32(0,1) == 1 ) nStringArray(i)(start:start) = char(45)
        start = start + 1
        
        if ( nlen == maxlen ) then 
            nStringArray(i)(start:start) = char(getRandom32(0, maxlastdigit) + 48)
            start = start + 1
            nlen = nlen - 1
        end if

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
        
        call decToInt64(trim(nString), n1, errorLogical)
        read (nString, *) n2
        
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, trim(nString), lf, n1 , lf, n2
            return 
        end if
        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, trim(nString), lf, n1 , lf, n2
            return 
        end if
        
        i = i + 1
    end do 
    ! Max value
    call decToInt64("9223372036854775807", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail max value(9223372036854775807)."
        print *, n1
        return 
    end if
    if ( n1 /= 9223372036854775807_k_int64 ) then
        print *, "Test Failed. Parsing not match max value(9223372036854775807)."
        print *, n1 
        return 
    end if
    ! Min value
    call decToInt64("-9223372036854775808", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail min value(-9223372036854775808)."
        print *, n1
        return 
    end if
    if ( n1 /= (-9223372036854775807_k_int64 - 1) ) then
        print *, "Test Failed. Parsing not match max value(-9223372036854775808)."
        print *, n1
        return
    end if
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        
        call decToInt64(nString, n1, errorLogical)
        read (nString, *) n2
        
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if
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

    print *, "# Test 3: Parsing invalid format";
    ! 1 too many
    call decToInt64("-9223372036854775809", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then 
        print* , "Fail test 3 at ", "-9223372036854775809"
        return
    end if 
    call decToInt64("9223372036854775808", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "9223372036854775807"
        return
    end if
    ! 1 digit too many.
    call decToInt64("-92233720368547758080", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "-92233720368547758080"
        return
    end if
    call decToInt64("92233720368547758070", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "92233720368547758070"
        return
    end if 
    ! Incorrect digit.
    call decToInt64(char(0), n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", char(0)
        return
    end if
    call decToInt64("/", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "/"
        return
    end if
    call decToInt64(":", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", ":"
        return
    end if
    call decToInt64("12345a", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "12345a"
        return
    end if
    ! Signs.
    call decToInt64("-", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "-"
        return
    end if
    call decToInt64("+", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "+"
        return
    end if
    ! Empty string.
    call decToInt64(" ", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at empty strings (space)"
        return
    end if
    call decToInt64("", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at empty strings (0 length)"
        return
    end if
    print *, "# TEST 3 Passed"
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call decToInt64("months-21478", n2, errorLogical, 8, 13)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-21478", n2
        return        
    else 
        if ( n2 /= 21478 ) then 
            print *, "Test 4 failed parse at ", "months-21478", n2
            return 
        end if
    end if
    call decToInt64("data-range:-214789-ext", n2, errorLogical, 12, 18)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "data-range:-214789-ext", n2
        return        
    else 
        if ( n2 /= -214789 ) then 
            print *, "Test 4 failed parse at ", "data-range:-214789-ext", n2
            return 
        end if
    end if
    ! Fail Cases
    call decToInt64("21478", n2, errorLogical, 2, 1)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "21478"
        return 
    end if
    call decToInt64("1234567", n2, errorLogical, 8, 20)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "1234567"
        return 
    end if
    call decToInt64("12345678", n2, errorLogical, -15, 0) 
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "12345678"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine decToInt64TrueErrorTest(error)
    use fnDecimalUtil64
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "decToInt64TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 5000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 9, maxlen = 19, maxlastdigit = 8
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    integer(k_int32)                       ::  getRandom32, i, i2, start, &
                                               nlen, llen, errorInt
    integer(k_int64)                       ::  n1, n2
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;

        nStringArray(i) = "                                                   " &
                       // "                                                 "

        start = getRandom32(1, slen - maxlen - 1)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

        if ( getRandom32(0,1) == 1 ) nStringArray(i)(start:start) = char(45)
        start = start + 1
        
        if ( nlen == maxlen ) then 
            nStringArray(i)(start:start) = char(getRandom32(0, maxlastdigit) + 48)
            start = start + 1
            nlen = nlen - 1
        end if

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
        
        call decToInt64TrueError(trim(nString), n1, errorInt)
        read (nString, *) n2
        
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, trim(nString), lf, n1 , lf, n2
            return 
        end if
        if ( n1 /= n2 ) then
            print *, "Test Failed. Parsing not match."
            print *, trim(nString), lf, n1 , lf, n2
            return 
        end if
        
        i = i + 1
    end do 
    ! Max value
    call decToInt64TrueError("9223372036854775807", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail max value(9223372036854775807)."
        print *, n1
        return 
    end if
    if ( n1 /= 9223372036854775807_k_int64 ) then
        print *, "Test Failed. Parsing not match max value(9223372036854775807)."
        print *, n1 
        return 
    end if
    ! Min value
    call decToInt64TrueError("-9223372036854775808", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail min value(-9223372036854775808)."
        print *, n1
        return 
    end if
    if ( n1 /= (-9223372036854775807_k_int64 - 1) ) then
        print *, "Test Failed. Parsing not match max value(-9223372036854775808)."
        print *, n1 
        return 
    end if
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        
        call decToInt64TrueError(nString, n1, errorInt)
        read (nString, *) n2
        
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if
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
    ! Underflow
    call decToInt64TrueError("-9223372036854775809", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at -9223372036854775809"
        return
    end if
    call decToInt64TrueError("-92233720368547758080", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at -92233720368547758080"
        return
    end if
    ! Overflow
    call decToInt64TrueError("9223372036854775808", n1, errorInt)
    if ( errorInt /= 4 ) then
        print *, "Test 3 fail at 9223372036854775808"
        return
    end if
    call decToInt64TrueError("9223372036854775808", n1, errorInt)
    if ( errorInt /= 4 ) then
        print *, "Test 3 fail at 9223372036854775808"
        return
    end if
    !Incorrect digit.
    call decToInt64TrueError(char(0), n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call decToInt64TrueError("/", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if
    call decToInt64TrueError("92233720368547758070:", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 92233720368547758070:"
        return
    end if
    call decToInt64TrueError("-92233720368547758070a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -92233720368547758070a"
        return
    end if
    call decToInt64TrueError("922337203685477580a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 922337203685477580a"
        return
    end if
    call decToInt64TrueError("922337203685477579a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 9223372036854775879a"
        return
    end if
    call decToInt64TrueError("922337203685477581a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 9223372036854775881a"
        return
    end if   
    ! Signs.
    call decToInt64TrueError("-", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -"
        return
    end if
    call decToInt64TrueError("+", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at +"
        return
    end if
    ! Empty string.
    call decToInt64TrueError("      ", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call decToInt64TrueError("", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if

    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call decToInt64TrueError("months-21478", n1, errorInt, 8, 13)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "months-21478", n2
        return        
    else 
        if ( n1 /= 21478 ) then 
            print *, "Test 4 failed parse at ", "months-21478", n1
            return 
        end if
    end if
    call decToInt64TrueError("data-range:-214789-ext", n1, errorInt, 12, 18)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "data-range:-214789-ext", n1
        return        
    else 
        if ( n1 /= -214789 ) then 
            print *, "Test 4 failed parse at ", "data-range:-214789-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    call decToInt64TrueError("21478", n1, errorInt, 2, 1)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "21478"
        return 
    end if 
    call decToInt64TrueError("1234567", n1, errorInt, 8, 20)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "1234567"
        return 
    end if 
    call decToInt64TrueError("12345678", n2, errorInt, -15, 0)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "12345678"
        return 
    end if 
    print *, "# Test 4: Passed."
    print *, lf, lf, lf
     

    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf, lf
end subroutine

subroutine decCompareTest64(error)
    use fnDecimalUtil64
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "decCompareTest64"
    integer(k_int32) , dimension(15)  ::  rExp
    character(len=70), dimension(15)  ::  lvalue
    character(len=70), dimension(15)  ::  rvalue
    logical                           ::  errorLogical
    integer(k_int32)                  ::  n1, i
    error = .TRUE.

    ! Prep Cases
    ! 1
    lvalue(1) = "23"
    rvalue(1) = "-0"
    rExp(1)   = 1

    lvalue(2) = "8000"
    rvalue(2) = "350"
    rExp(2)   = 1

    lvalue(3) = "8000"
    rvalue(3) = "7999"
    rExp(3)   = 1

    lvalue(4) = "800500000"
    rvalue(4) = "15895875"
    rExp(4)   = 1

    lvalue(5) = "9223372036854775807" 
    rvalue(5) = "-9223372036854775807"
    rExp(5)   = 1

    ! 0
    lvalue(6) = "23"
    rvalue(6) = "+23"
    rExp(6)   = 0

    lvalue(7) = "-23" 
    rvalue(7) = "-23"
    rExp(7)   = 0

    lvalue(8) = "+0" 
    rvalue(8) = "-0"
    rExp(8)   = 0

    lvalue(9) = "-0"
    rvalue(9) = "+00000"
    rExp(9)   = 0

    lvalue(10) = "000000250"
    rvalue(10) = "000250"
    rExp(10)   = 0
    ! -1
    lvalue(11) = "0"
    rvalue(11) = "9223372036854775807"
    rExp(11)   = -1

    lvalue(12) = "35000"
    rvalue(12) = "80000"
    rExp(12)   = -1

    lvalue(13) = "79999" 
    rvalue(13) = "80000"
    rExp(13)   = -1

    lvalue(14) = "-9223372036854775808" 
    rvalue(14) = "9223372036854775807"
    rExp(14)   = -1

    lvalue(15) = "-35550"
    rvalue(15) = "-3550"
    rExp(15)   = -1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
    i = 1
    do while ( i <= 15 )
        call decCompareAsInt64(lvalue(i), rvalue(i), n1, errorLogical)
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
    call decCompareAsInt64("23", "", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "23 to empty"
        return 
    end if
    call decCompareAsInt64("", "23", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "empty to 23"
        return 
    end if
    ! Incorrect format
    call decCompareAsInt64("23a", "748", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "23a to 23"
        return 
    end if
    call decCompareAsInt64("488887", "8742d", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "88887 to 8742d"
        return 
    end if
    ! Underflow/Overflow.
    call decCompareAsInt64("-9223372036854775809", "-9223372036854775808", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "-9223372036854775809 to -9223372036854775808"
        return 
    end if
    call decCompareAsInt64("9223372036854775807", "9223372036854775808", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "9223372036854775807 to 9223372036854775808"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine decInt64OrSmallerTest(error)
    use fnDecimalUtil64
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "decInt64OrSmallerTest"
    integer(k_int32) , dimension(27)  ::  rExp
    character(len=30), dimension(27)  ::  lvalue
    character(len=30), dimension(27)  ::  rvalue
    integer(k_int32)                  ::  i
    error = .TRUE.

    ! Invalid to
    lvalue(1) = "12345a" 
    rvalue(1) = "-"
    rExp(1)   = 0
    
    lvalue(2) = "123457a"
    rvalue(2) = "-92233720368547758070"
    rExp(2)   = -1

    lvalue(3) = "123459457b" 
    rvalue(3) = "92233720368547758070"
    rExp(3)   = -1

    lvalue(4) = "124 87548"
    rvalue(4) = ""
    rExp(4)   = -1

    lvalue(5) = " 1 +124"
    rvalue(5) = "1234"
    rExp(5)   = -1

    ! UnderFlow to
    lvalue(6) = "-92233720368547758070"
    rvalue(6) = "#"
    rExp(6)   = 1

    lvalue(7) = "-92233720368547758070"
    rvalue(7) = "-922337203685477580700"
    rExp(7)   = 0

    lvalue(8) = "-92233720368547758070"
    rvalue(8) = "92233720368547758070"
    rExp(8)   = -1

    lvalue(9) = "-92233720368547758070"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "-92233720368547758070"
    rvalue(10) = "122"
    rExp(10)   = -1

    ! OverFlow to
    lvalue(11) = "92233720368547758070"
    rvalue(11) = "12458784556875421547845215487("
    rExp(11)   = 1

    lvalue(12) = "92233720368547758070"
    rvalue(12) = "-92233720368547758070"
    rExp(12)   = 1

    lvalue(13) = "92233720368547758070"
    rvalue(13) = "+922337203685477580700"
    rExp(13)   = 0

    lvalue(14) = "92233720368547758070"
    rvalue(14) = ""
    rExp(14)   = -1

    lvalue(15) = "+92233720368547758070"
    rvalue(15) = "0"
    rExp(15)   = -1

    ! Empty to
    lvalue(16) = ""
    rvalue(16) = "124a"
    rExp(16)   = 1

    lvalue(17) = "           "
    rvalue(17) = "-92233720368547758070"
    rExp(17)   = 1

    lvalue(18) = "           "
    rvalue(18) = "+92233720368547758070"
    rExp(18)   = 1

    lvalue(19) = "      "
    rvalue(19) = ""
    rExp(19)   = 0

    lvalue(20) = "      "
    rvalue(20) = "12"
    rExp(20)   = -1
    
    ! Valid to
    lvalue(21) = "55"
    rvalue(21) = "44%"
    rExp(21)   = 1

    lvalue(22) = "0" 
    rvalue(22) = "-92233720368547758070"
    rExp(22)   = 1
    
    lvalue(23) = "2487"
    rvalue(23) = "+92233720368547758070"
    rExp(23)   = 1

    lvalue(24) = "397857"
    rvalue(24) = ""
    rExp(24)   = 1

    lvalue(25) = "24"
    rvalue(25) = "35"
    rExp(25)   = -1

    lvalue(26) = "85" 
    rvalue(26) = "85"
    rExp(26)   = 0

    lvalue(27) = "98"
    rvalue(27) = "80"
    rExp(27)   = 1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Case to case."
    i = 1
    do while ( i <= 27 )
        if ( decInt64OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , decInt64OrSmaller(lvalue(i), rvalue(i)), "'"
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

pure subroutine fnBinaryUtil64TestPure()
    use fnDecimalUtil64
    implicit none
    logical           ::  el
    integer(k_int64)  ::  r64
    integer(k_int32)  ::  ei, r

    call decToInt64("0", r64, el)
    call decToInt64TrueError("0",  r64, ei)
    call decCompareAsInt64("0", "0", r, el)
    r = decInt64OrSmaller("1","1")
end subroutine
