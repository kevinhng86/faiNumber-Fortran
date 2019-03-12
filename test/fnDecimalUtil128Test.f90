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

program fniDecimalUtilTest128
    implicit none
    logical  ::  error
    
    call decToInt128Test(error)
    if ( error ) stop 1
    call decToInt128TrueErrorTest(error)
    if ( error ) stop 1
    call decCompareTest128(error)
    if ( error ) stop 1
    call decInt128OrSmallerTest(error)
    if ( error ) stop 1
end program

subroutine decToInt128Test(error)
    use fnDecimalUtil128
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "decToInt128Test"
    integer(k_int32)   , parameter         ::  rCase = 5000000, slen = 150
    integer(k_int32)   , parameter         ::  maxdigit = 9, maxlen = 39, maxlastdigit = 0
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    integer(k_int32)                       ::  getRandom32, i, i2, start, &
                                               nlen, llen
    integer(k_int128)                      ::  n1, n2
    logical                                ::  errorLogical
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ; llen = 0
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                             " &
                       // "                                             " &
                       // "                                             " &
                       // "               "
    
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
        
        call decToInt128(trim(nString), n1, errorLogical)
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
    call decToInt128("170141183460469231731687303715884105727", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail max value(170141183460469231731687303715884105727)."
        print *, n1
        return 
    end if
    if ( n1 /= 170141183460469231731687303715884105727_k_int128 ) then
        print *, "Test Failed. Parsing not match max value(170141183460469231731687303715884105727)."
        print *, n1 
        return 
    end if
    ! Min value
    call decToInt128("-170141183460469231731687303715884105728", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail min value(-170141183460469231731687303715884105728)."
        print *, n1
        return 
    end if
    if ( n1 /= (-170141183460469231731687303715884105727_k_int128 - 1_k_int128) ) then
        print *, "Test Failed. Parsing not match max value(-170141183460469231731687303715884105728)."
        print *, n1
        return
    end if
    print *, "# Test 1: Passed."
    print *, lf, lf, lf

    print *, "# Test 2: Untrimmed strings, ", rCase, " random cases. Included leading zeroes."
    i = 1
    do while( i <= rCase )
        nString = nStringArray(i)
        
        call decToInt128(nString, n1, errorLogical)
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
    call decToInt128("-170141183460469231731687303715884105729", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then 
        print* , "Fail test 3 at ", "-170141183460469231731687303715884105729"
        return
    end if
    call decToInt128("170141183460469231731687303715884105728", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "170141183460469231731687303715884105728"
        return
    end if
    ! 1 digit too many.
    call decToInt128("-1701411834604692317316873037158841057280", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "-1701411834604692317316873037158841057280"
        return
    end if
    call decToInt128("1701411834604692317316873037158841057270", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "1701411834604692317316873037158841057270"
        return
    end if 
    ! Incorrect digit.
    call decToInt128(char(0), n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", char(0)
        return
    end if
    call decToInt128("/", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "/"
        return
    end if
    call decToInt128(":", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", ":"
        return
    end if
    call decToInt128("12345a", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "12345a"
        return
    end if
    ! Signs.
    call decToInt128("-", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "-"
        return
    end if
    call decToInt128("+", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at ", "+"
        return
    end if
    ! Empty string.
    call decToInt128(" ", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at empty strings (space)"
        return
    end if
    call decToInt128("", n2, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print* , "Fail test 3 at empty strings (0 length)"
        return
    end if
    print *, "# TEST 3 Passed"
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call decToInt128("months-21478", n2, errorLogical, 8, 13)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-21478", n2
        return        
    else 
        if ( n2 /= 21478 ) then 
            print *, "Test 4 failed parse at ", "months-21478", n2
            return 
        end if
    end if
    call decToInt128("data-range:-214789-ext", n2, errorLogical, 12, 18)
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
    call decToInt128("21478", n2, errorLogical, 2, 1)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "21478"
        return 
    end if
    call decToInt128("1234567", n2, errorLogical, 8, 20)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "1234567"
        return 
    end if 
    call decToInt128("12345678", n2, errorLogical, -15, 0)
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

subroutine decToInt128TrueErrorTest(error)
    use fnDecimalUtil128
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "decToInt128TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 5000000, slen = 150
    integer(k_int32)   , parameter         ::  maxdigit = 9, maxlen = 39, maxlastdigit = 0
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    integer(k_int32)                       ::  getRandom32, i, i2, start, &
                                               nlen, llen, errorInt
    integer(k_int128)                      ::  n1, n2
    error = .TRUE.
    
    print *, "##### Start test ", testName, " #####"
    print *, "# Generating random data"
    i = 1 ;
    do while( i <= rCase )
        i2 = 1;
        
        nStringArray(i) = "                                             " &
                       // "                                             " &
                       // "                                             " &
                       // "               "
    
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
        
        call decToInt128TrueError(trim(nString), n1, errorInt)
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
    call decToInt128TrueError("170141183460469231731687303715884105727", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail max value(170141183460469231731687303715884105727)."
        print *, n1
        return 
    end if
    if ( n1 /= 170141183460469231731687303715884105727_k_int128 ) then
        print *, "Test Failed. Parsing not match max value(170141183460469231731687303715884105727)."
        print *, n1 
        return 
    end if
    ! Min value
    call decToInt128TrueError("-170141183460469231731687303715884105728", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail min value(-170141183460469231731687303715884105728)."
        print *, n1
        return 
    end if
    if ( n1 /= (-170141183460469231731687303715884105727_k_int128 - 1_k_int128) ) then
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

        call decToInt128TrueError(nString, n1, errorInt)
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
    call decToInt128TrueError("-170141183460469231731687303715884105729", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at -170141183460469231731687303715884105729"
        return
    end if
    call decToInt128TrueError("-1701411834604692317316873037158841057270", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at -1701411834604692317316873037158841057270"
        return
    end if
    ! Overflow
    call decToInt128TrueError("170141183460469231731687303715884105728", n1, errorInt)
    if ( errorInt /= 4 ) then
        print *, "Test 3 fail at 170141183460469231731687303715884105728"
        return
    end if
    call decToInt128TrueError("1701411834604692317316873037158841057280", n1, errorInt)
    if ( errorInt /= 4 ) then
        print *, "Test 3 fail at 1701411834604692317316873037158841057280"
        return
    end if
    !Incorrect digit.
    call decToInt128TrueError(char(0), n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call decToInt128TrueError("/", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if
    call decToInt128TrueError("1701411834604692317316873037158841057270:", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 1701411834604692317316873037158841057270:"
        return
    end if
    call decToInt128TrueError("-1701411834604692317316873037158841057270a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -1701411834604692317316873037158841057270a"
        return
    end if
    call decToInt128TrueError("17014118346046923173168730371588410572a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 17014118346046923173168730371588410572a"
        return
    end if
    call decToInt128TrueError("17014118346046923173168730371588410571a", n1, errorInt)  
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 17014118346046923173168730371588410571a"
        return
    end if
    call decToInt128TrueError("17014118346046923173168730371588410573a", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 17014118346046923173168730371588410573a"
        return
    end if   
    ! Signs.
    call decToInt128TrueError("-", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -"
        return
    end if
    call decToInt128TrueError("+", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at +"
        return
    end if
    ! Empty string.
    call decToInt128TrueError("      ", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call decToInt128TrueError("", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if

    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call decToInt128TrueError("months-21478", n1, errorInt, 8, 13)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "months-21478", n2
        return        
    else 
        if ( n1 /= 21478 ) then 
            print *, "Test 4 failed parse at ", "months-21478", n1
            return 
        end if
    end if
    call decToInt128TrueError("data-range:-214789-ext", n1, errorInt, 12, 18)
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
    call decToInt128TrueError("21478", n1, errorInt, 2, 1)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "21478"
        return 
    end if
    call decToInt128TrueError("1234567", n1, errorInt, 8, 20)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "1234567"
        return 
    end if 
    call decToInt128TrueError("12345678", n2, errorInt, -15, 0)
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

subroutine decCompareTest128(error)
    use fnDecimalUtil128
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "decCompareTest128"
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

    lvalue(5) = "170141183460469231731687303715884105727"
    rvalue(5) = "-170141183460469231731687303715884105727"
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
    rvalue(11) = "170141183460469231731687303715884105727"
    rExp(11)   = -1

    lvalue(12) = "35000"
    rvalue(12) = "80000"
    rExp(12)   = -1

    lvalue(13) = "79999" 
    rvalue(13) = "80000"
    rExp(13)   = -1

    lvalue(14) = "-170141183460469231731687303715884105727"
    rvalue(14) = "170141183460469231731687303715884105727"
    rExp(14)   = -1

    lvalue(15) = "-35550"
    rvalue(15) = "-3550"
    rExp(15)   = -1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Right format comparision."
    i = 1
    do while ( i <= 15 )
        call decCompareAsInt128(lvalue(i), rvalue(i), n1, errorLogical)
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
    call decCompareAsInt128("23", "", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "23 to empty"
        return 
    end if
    call decCompareAsInt128("", "23", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "empty to 23"
        return 
    end if
    ! Incorrect format
    call decCompareAsInt128("23a", "748", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "23a to 23"
        return 
    end if
    call decCompareAsInt128("488887", "8742d", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "88887 to 8742d"
        return 
    end if
    ! Underflow/Overflow.
    call decCompareAsInt128("-170141183460469231731687303715884105729", &
                            "-170141183460469231731687303715884105727", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "-170141183460469231731687303715884105729 to -170141183460469231731687303715884105727"
        return 
    end if
    call decCompareAsInt128("170141183460469231731687303715884105728", &
                            "170141183460469231731687303715884105728", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "170141183460469231731687303715884105727 to 170141183460469231731687303715884105728"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine decInt128OrSmallerTest(error)
    use fnDecimalUtil128
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "decInt128OrSmallerTest"
    integer(k_int32) , dimension(28)  ::  rExp
    character(len=70), dimension(28)  ::  lvalue
    character(len=70), dimension(28)  ::  rvalue
    integer(k_int32)                  ::  i
    error = .TRUE.

    ! Invalid to
    lvalue(1) = "12345a" 
    rvalue(1) = "-"
    rExp(1)   = 0
    
    lvalue(2) = "123457a"
    rvalue(2) = "-170141183460469231731687303715884105727"
    rExp(2)   = -1

    lvalue(3) = "123459457b" 
    rvalue(3) = "1701411834604692317316873037158841057270"
    rExp(3)   = -1

    lvalue(4) = "124 87548"
    rvalue(4) = ""
    rExp(4)   = -1

    lvalue(5) = " 1 +124"
    rvalue(5) = "1234"
    rExp(5)   = -1

    ! UnderFlow to
    lvalue(6) = "-1701411834604692317316873037158841057270"
    rvalue(6) = "#"
    rExp(6)   = 1

    lvalue(7) = "-1701411834604692317316873037158841057270"
    rvalue(7) = "-17014118346046923173168730371588410572700"
    rExp(7)   = 0

    lvalue(8) = "-1701411834604692317316873037158841057270"
    rvalue(8) = "17014118346046923173168730371588410572700"
    rExp(8)   = -1

    lvalue(9) = "-1701411834604692317316873037158841057270"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "-1701411834604692317316873037158841057270"
    rvalue(10) = "122"
    rExp(10)   = -1

    ! OverFlow to
    lvalue(11) = "1701411834604692317316873037158841057270"
    rvalue(11) = "12458784556875421547845215487("
    rExp(11)   = 1

    lvalue(12) = "1701411834604692317316873037158841057270"
    rvalue(12) = "-1701411834604692317316873037158841057270"
    rExp(12)   = 1

    lvalue(13) = "1701411834604692317316873037158841057270"
    rvalue(13) = "+17014118346046923173168730371588410572700"
    rExp(13)   = 0

    lvalue(14) = "1701411834604692317316873037158841057270"
    rvalue(14) = ""
    rExp(14)   = -1

    lvalue(15) = "1701411834604692317316873037158841057270"
    rvalue(15) = "0"
    rExp(15)   = -1

    ! Empty to
    lvalue(16) = ""
    rvalue(16) = "124a"
    rExp(16)   = 1

    lvalue(17) = "           "
    rvalue(17) = "-1701411834604692317316873037158841057270"
    rExp(17)   = 1

    lvalue(18) = "           "
    rvalue(18) = "+1701411834604692317316873037158841057270"
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
    rvalue(22) = "-1701411834604692317316873037158841057270"
    rExp(22)   = 1
    
    lvalue(23) = "2487"
    rvalue(23) = "+1701411834604692317316873037158841057270"
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
        if ( decInt128OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , decInt128OrSmaller(lvalue(i), rvalue(i)), "'"
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

pure subroutine fnDecimalUtil128TestPure()
    use fnDecimalUtil128
    implicit none
    logical            ::  el
    integer(k_int128)  ::  r128
    integer(k_int32)   ::  ei, r

    call decToInt128("0", r128, el)
    call decToInt128TrueError("0",  r128, ei)
    call decCompareAsInt128("0", "0", r, el)
    r = decInt128OrSmaller("1","1")
end subroutine
