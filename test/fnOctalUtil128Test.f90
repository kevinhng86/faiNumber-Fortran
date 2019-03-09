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

! This test file test fnOctalUtil128 plus the int128ToOctalAsU and the 
! int128ToOctal function
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

program fnOctalUtil128Test
    implicit none
    logical  ::  error
    
    call octalToInt128Test(error)
    if ( error ) stop
    call octalToInt128TrueErrorTest(error)
    if ( error ) stop
    call octalCompareAsInt128Test(error)
    if ( error ) stop    
    call octalInt128OrSmallerTest(error)
    if ( error ) stop
end program

subroutine octalToInt128Test(error)
    use fnOctalUtil128
    use fnInt128Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "octalToInt128Test"
    integer(k_int32)   , parameter         ::  rCase = 5000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 7, maxlen = 43, maxlastdigit = 3
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=43)                      ::  oString
    character(len=44)                      ::  oString2
    integer(k_int32)                       ::  getRandom32, i, i2, start, nlen, llen
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
                       // "          "

        start = getRandom32(1, slen - maxlen)
        nlen = getRandom32(1, maxlen)
        if ( nlen > llen ) llen = nlen

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
        call octalToInt128(TRIM(nString), n1, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            oString2 = int128ToOctal(n1)
            if ( oString2(44:44) == charspace ) then
                print *, "Something is wrong with the octal function"
                print *, n1, lf, oString2
                return
            end if     
            nString = ADJUSTL(oString2)
        else
            oString = int128ToOctalAsU(n1)
            if ( oString(43:43) == charspace ) then
                print *, "Something is wrong with the octal function"
                print *, n1, lf, oString
                return
            end if
            nString = ADJUSTL(oString)
        end if
        
        call octalToInt128(TRIM(nString), n2, errorLogical)
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
    ! 170141183460469231731687303715884105727
    call octalToInt128("1777777777777777777777777777777777777777777", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(170141183460469231731687303715884105727)."
        print *, n1
        return 
    end if
    if ( n1 /= 170141183460469231731687303715884105727_k_int128 ) then
        print *, "Test Failed. Parsing not match value(170141183460469231731687303715884105727)."
        print *, n1 
        return 
    end if
    ! -1
    call octalToInt128("3777777777777777777777777777777777777777777", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1_k_int128 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0 
    call octalToInt128("0", n1, errorLogical)
    if ( errorLogical .eqv. .TRUE. ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0_k_int128 ) then
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
        call octalToInt128(nString, n1, errorLogical)
        if ( errorLogical .eqv. .TRUE. ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i), lf, nString, lf, n1
            error = .TRUE.
            return 
        end if
        
        nString = int128ToOctalAsU(n1)
        call octalToInt128(nString, n2, errorLogical)
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
    ! 1 too many
    call octalToInt128("4000000000000000000000000000000000000000000", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    ! 1 digit too many
    call octalToInt128("37777777777777777777777777777777777777777770", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 1 digit too many"
        return
    end if    
    !Incorrect digit.
    call octalToInt128(char(0), n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call octalToInt128("/", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at /"
        return
    end if
    call octalToInt128("8", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 8"
        return
    end if   
    ! Signs where unsigned.
    call octalToInt128("-0", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at -0"
        return
    end if
    call octalToInt128("+0", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    call octalToInt128("      ", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call octalToInt128("", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf
    
    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call octalToInt128("months-72735", n1, errorLogical, 8, 13)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "months-72735", n1
        return        
    else 
        if ( n1 /= 30173_k_int128 ) then 
            print *, "Test 4 failed parse at ", "months-72735", n1
            return 
        end if
    end if
    call octalToInt128("data-range:7172417-ext", n1, errorLogical, 12, 18)
    if ( errorLogical .neqv. .FALSE. ) then
        print *, "Test 4 failed logical at ", "data-range:7172417-ext", n1
        return        
    else 
        if ( n1 /= 1897743_k_int128 ) then 
            print *, "Test 4 failed parse at ", "data-range:7172417-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    call octalToInt128("00000", n1, errorLogical, 2, 1)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "00000"
        return 
    end if
    call octalToInt128("0000000", n1, errorLogical, 8, 20)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 4 failed at ", "0000000"
        return 
    end if
    call octalToInt128("0000000000000000", n2, errorLogical, -15, 0)
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

subroutine octalToInt128TrueErrorTest(error)
    use fnOctalUtil128
    use fnInt128Util
    implicit none
    logical            , intent(out)       ::  error
    character(len=2)   , parameter         ::  lf = char(10) // char(13)
    character(len=*)   , parameter         ::  testName = "octalToInt128TrueErrorTest"
    integer(k_int32)   , parameter         ::  rCase = 10000000, slen = 100
    integer(k_int32)   , parameter         ::  maxdigit = 7, maxlen = 43, maxlastdigit = 3
    character(len=slen), dimension(rCase)  ::  nStringArray
    character(len=slen)                    ::  nString
    character(len=43)                      ::  oString
    character(len=44)                      ::  oString2    
    integer(k_int32)                       ::  getRandom32, i, i2, & 
                                               start, nlen, llen, errorInt
    integer(k_int128)                      ::  n1, n2
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
        
        call octalToInt128TrueError(TRIM(nString), n1, errorInt)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nStringArray(i)
            print *, TRIM(nString), lf, n1 , lf, n2
            return 
        end if
        
        if ( n1 > -1 ) then
            oString2 = int128ToOctal(n1)
            if ( oString2(44:44) == charspace ) then
                print *, "Something is wrong with the octal function"
                print *, n1, lf, oString2
                return
            end if     
            nString = ADJUSTL(oString2)
        else
            oString = int128ToOctalAsU(n1)
            if ( oString(43:43) == charspace ) then
                print *, "Something is wrong with the octal function"
                print *, n1, lf, oString
                return
            end if
            nString = ADJUSTL(oString)
        end if
        
        call octalToInt128TrueError(TRIM(nString), n2, errorInt)
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
    ! 170141183460469231731687303715884105727
    call octalToInt128TrueError("1777777777777777777777777777777777777777777", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(170141183460469231731687303715884105727)."
        print *, n1
        return 
    end if
    if ( n1 /= 170141183460469231731687303715884105727_k_int128 ) then
        print *, "Test Failed. Parsing not match value(170141183460469231731687303715884105727)."
        print *, n1 
        return 
    end if
    ! -1
    call octalToInt128TrueError("3777777777777777777777777777777777777777777", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(-1)."
        print *, n1
        return 
    end if
    if ( n1 /= -1_k_int128 ) then
        print *, "Test Failed. Parsing not match value(-1)."
        print *, n1 
        return 
    end if
    ! 0
    call octalToInt128TrueError("0", n1, errorInt)
    if ( errorInt /= 0 ) then
        print *, "Test Failed. Logical fail value(0)."
        print *, n1
        return 
    end if
    if ( n1 /= 0_k_int128 ) then
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
        call octalToInt128TrueError(nString, n1, errorInt)
        if ( errorInt /= 0 ) then
            print *, "Test Failed. Error reported."
            print *, nString, lf, n1 , lf, n2
            error = .TRUE.
            return 
        end if

        nString = int128ToOctalAsU(n1)
        call octalToInt128TrueError(nString, n2, errorInt)
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
    ! 1 too many
    call octalToInt128TrueError("4000000000000000000000000000000000000000000", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at 1 too many"
        return
    end if
    ! 1 digit too many
    call octalToInt128TrueError("37777777777777777777777777777777777777777770", n1, errorInt)
    if ( errorInt /= 3 ) then
        print *, "Test 3 fail at 1 digit too many"
        return
    end if 
    !Incorrect digit
    call octalToInt128TrueError(char(0), n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at null char"
        return
    end if
    call octalToInt128TrueError("/", n1, errorInt)   
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at /"
        return
    end if
    call octalToInt128TrueError("40000000000000000000000000000000000000000008", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 40000000000000000000000000000000000000000008"
        return
    end if
    call octalToInt128TrueError("4000000000000000000000000000000000000000000:", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at 4000000000000000000000000000000000000000000:"
        return
    end if
    ! Signs where unsigned
    call octalToInt128TrueError("-0", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at -0"
        return
    end if
    call octalToInt128TrueError("+0", n1, errorInt)
    if ( errorInt /= 2 ) then
        print *, "Test 3 fail at +0"
        return
    end if
    ! Empty string.
    call octalToInt128TrueError("      ", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at empty"
        return
    end if
    call octalToInt128TrueError("", n1, errorInt)
    if ( errorInt /= 1 ) then
        print *, "Test 3 fail at 0 length"
        return
    end if
    print *, "# TEST 3 Passed."
    print *, lf, lf, lf

    print *, "# Test 4: Positioning test."
    ! Sucess cases
    call octalToInt128TrueError("months-72735", n1, errorInt, 8, 13)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "months-72735", n2
        return        
    else 
        if ( n1 /= 30173_k_int128 ) then 
            print *, "Test 4 failed parse at ", "months-72735", n1
            return 
        end if
    end if
    call octalToInt128TrueError("data-range:7172417-ext", n1, errorInt, 12, 18)
    if ( errorInt /= 0 ) then
        print *, "Test 4 failed logical at ", "data-range:7172417-ext", n1
        return        
    else 
        if ( n1 /= 1897743_k_int128 ) then 
            print *, "Test 4 failed parse at ", "data-range:7172417-ext", n1
            return 
        end if
    end if
    ! Fail Cases
    call octalToInt128TrueError("00000000", n1, errorInt, 2, 1)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "00000000"
        return 
    end if
    call octalToInt128TrueError("0000", n1, errorInt, 8, 20)
    if ( errorInt /= 5 ) then
        print *, "Test 4 failed at ", "0000"
        return 
    end if
    call octalToInt128TrueError("12345678", n2, errorInt, -15, 0)
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

subroutine octalCompareAsInt128Test(error)
    use fnOctalUtil128
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "octalCompareAsInt128Test"
    integer(k_int32) , dimension(15)  ::  rExp
    character(len=70), dimension(15)  ::  lvalue
    character(len=70), dimension(15)  ::  rvalue
    logical                           ::  errorLogical
    integer(k_int32)                  ::  n1, i
    error = .TRUE.

    ! Prep Cases
    ! 1
    lvalue(1) = "27"
    rvalue(1) = "0"
    rExp(1)   = 1

    lvalue(2) = "17500"
    rvalue(2) = "535"
    rExp(2)   = 1

    lvalue(3) = "17542"
    rvalue(3) = "17541"
    rExp(3)   = 1

    lvalue(4) = "1774242"
    rvalue(4) = "147342"
    rExp(4)   = 1

    ! Note that octal are being parsed as they are bit representation
    ! to the signed int type. Thus, the first octal number below is smaller than
    ! the second number.
    !
    ! 2000000000000000000000000000000000000000001(-170141183460469231731687303715884105727) 
    ! 1777777777777777777777777777777777777777777(170141183460469231731687303715884105727)
    lvalue(5) = "1777777777777777777777777777777777777777777"
    rvalue(5) = "2000000000000000000000000000000000000000001"
    rExp(5)   = 1

    ! 0
    lvalue(6) = "72"
    rvalue(6) = "72"
    rExp(6)   = 0

    lvalue(7) = "3777777777777777777777777777777777777777777"
    rvalue(7) = "3777777777777777777777777777777777777777777"
    rExp(7)   = 0

    lvalue(8) = "257000432"
    rvalue(8) = "257000432"
    rExp(8)   = 0

    lvalue(9) = "0"
    rvalue(9) = "00000"
    rExp(9)   = 0

    lvalue(10) = "000000"
    rvalue(10) = "000"
    rExp(10)   = 0

    ! -1
    lvalue(11) = "0"
    rvalue(11) = "27"
    rExp(11)   = -1

    lvalue(12) = "721"
    rvalue(12) = "1721"
    rExp(12)   = -1

    lvalue(13) = "12420"
    rvalue(13) = "13310"
    rExp(13)   = -1

    lvalue(14) = "3777777777777777177777777777777777777777777"
    rvalue(14) = "3777777777777777777777777777777777777777777"
    rExp(14)   = -1

    lvalue(15) = "5430507044"
    rvalue(15) = "15051215105"
    rExp(15)   = -1
    
    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Right format comparision."
    i = 1
    do while ( i <= 15 )
        call octalCompareAsInt128(lvalue(i), rvalue(i), n1, errorLogical)
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
    call octalCompareAsInt128("10", "", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "10 to empty"
        return 
    end if
    call octalCompareAsInt128("", "10", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "empty to 10"
        return 
    end if
    ! Incorrect format
    call octalCompareAsInt128("118", "110", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "118", "110"
        return 
    end if
    call octalCompareAsInt128("110", "11/", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at ", "110", "11/"
        return 
    end if
    ! Beyond capacity
    call octalCompareAsInt128("11111", "4000000000000000000000000000000000000000000", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "11111", "4000000000000000000000000000000000000000000"
        return 
    end if
    call octalCompareAsInt128("4000000000000000000000000000000000000000000", "11111", n1, errorLogical)
    if ( errorLogical .neqv. .TRUE. ) then
        print *, "Test 2 fail at", "4000000000000000000000000000000000000000000", "11111"
        return 
    end if
    print *, "# Test 2: Passed."
    print *, lf, lf
     
    error = .FALSE.
    print *, "##### Test ", testName, " Passed. #####"
    print *, lf, lf
end subroutine

subroutine octalInt128OrSmallerTest(error)
    use fnOctalUtil128
    implicit none
    logical          , intent(out)    ::  error
    character(len=2) , parameter      ::  lf = char(10) // char(13)
    character(len=*) , parameter      ::  testName = "octalInt128OrSmallerTest"
    integer(k_int32) , dimension(23)  ::  rExp
    character(len=70), dimension(23)  ::  lvalue
    character(len=70), dimension(23)  ::  rvalue
    integer(k_int32)                  ::  i
    error = .TRUE.
    
    ! Invalid to
    lvalue(1) = "18"
    rvalue(1) = "-0"
    rExp(1)   = 0

    lvalue(2) = "111:"
    rvalue(2) = "4000000000000000000000000000000000000000000"
    rExp(2)   = -1

    lvalue(3) = "01/"
    rvalue(3) = ""
    rExp(3)   = -1

    lvalue(4) = "01a"
    rvalue(4) = "     "
    rExp(4)   = -1

    lvalue(5) = " 1 1 "
    rvalue(5) = "7"
    rExp(5)   = -1

    ! Beyond capacity to
    lvalue(6) = "4000000000000000000000000000000000000000000"
    rvalue(6) = "/1"
    rExp(6)   = 1

    lvalue(7) = "4000000000000000000000000000000000000000000"
    rvalue(7) = "40000000000000000000000000000000000000000000"
    rExp(7)   = 0

    lvalue(8) = "4000000000000000000000000000000000000000000"
    rvalue(8) = "      "
    rExp(8)   = -1

    lvalue(9) = "4000000000000000000000000000000000000000000"
    rvalue(9) = ""
    rExp(9)   = -1

    lvalue(10) = "4000000000000000000000000000000000000000000"
    rvalue(10) = "7"
    rExp(10)   = -1

    ! Empty to
    lvalue(11) = ""
    rvalue(11) = "118"
    rExp(11)   = 1

    lvalue(12) = "           "
    rvalue(12) = "118"
    rExp(12)   = 1

    lvalue(13) = "           "
    rvalue(13) = "4000000000000000000000000000000000000000000"
    rExp(13)   = 1

    lvalue(14) = "      "
    rvalue(14) = ""
    rExp(14)   = 0

    lvalue(15) = "      "
    rvalue(15) = "7"
    rExp(15)   = -1
    ! Valid to
    lvalue(16) = "73"
    rvalue(16) = "111a"
    rExp(16)   = 1

    lvalue(17) = "73"
    rvalue(17) = "4000000000000000000000000000000000000000000"
    rExp(17)   = 1
    
    lvalue(18) = "73"
    rvalue(18) = ""
    rExp(18)   = 1

    lvalue(19) = "73"
    rvalue(19) = "    "
    rExp(19)   = 1

    lvalue(20) = "73"
    rvalue(20) = "0073"
    rExp(20)   = 0

    lvalue(21) = "0073"
    rvalue(21) = "73"
    rExp(21)   = 0

    lvalue(22) = "2112342"
    rvalue(22) = "1527543"
    rExp(22)   = 1

    lvalue(23) = "152731"
    rvalue(23) = "172731"
    rExp(23)   = -1

    print *, "##### Start test ", testName, " #####"
    print *, lf, lf
  
    print *, "# TEST 1: Case to case."
    i = 1
    do while ( i <= 23 )
        if ( octalInt128OrSmaller(lvalue(i), rvalue(i)) /= rExp(i) ) then
            print *, "---Test 2 fail result at case '", i ,"'. Got unexpected value '" &
                   , octalInt128OrSmaller(lvalue(i), rvalue(i)), "'"
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

pure subroutine fnOctalUtil128TestPure()
    use fnOctalUtil128
    implicit none
    logical            ::  el
    integer(k_int128)  ::  r128
    integer(k_int32)   ::  ei, r

    call octalToInt128("0", r128, el)
    call octalToInt128TrueError("0",  r128, ei)
    call octalCompareAsInt128("0", "0", r, el)
    r = octalInt128OrSmaller("1","1")
end subroutine
