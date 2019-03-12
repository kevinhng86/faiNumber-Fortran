program example
    ! For 64/128, use fnDecimalUtil64/fnDecimalUtil128.
    ! To use procedures of 64/128, The right module have to be called.
    use fnDecimalUtil   
    implicit none
    ! For 64/128, integer kind are k_int64/k_int128.
    integer(k_int32)  ::  resultValue, startpos, endpos
    ! Where there is an error code return, it will always be an int32 value.
    integer(k_int32)  ::  errorInt
    logical           ::  errorLogical

    ! For 64/128, call decToInt64/decToInt128.
    call decToInt32("123", resultValue, errorLogical)
    if ( errorLogical .eqv. .FALSE. ) then
        print *, resultValue
    else 
        print *, "There was an error during parsing."
    end if
    
    startpos = 13
    endpos = 17
    call decToInt32(" This here($12345)can be parse with start and end", &
                     resultValue, errorLogical, startpos, endpos)
 
    if ( errorLogical .eqv. .FALSE. ) then
        print *, resultValue
    else 
        print *, "There was an error during parsing."
    end if

    ! This procedure below is where you need to know what was wrong
    ! during parsing the input string.
    !
    ! This may run slower if the strings are long. The TrueError procedure
    ! has exactly the same feature as the normal one, they are just 
    ! different by how errors are handled.
    !
    ! Empty string will be checked first then error 5.
    !
    ! If error 5 is encountered, nothing else will be check. For error
    ! 5, startpos will be checked first before endpos.
    !
    ! For 64/128, call decToInt64TrueError/decToInt128TrueError
    startpos = 12
    call decToInt32TrueError("  line 24: 1278421", resultValue, errorInt, startpos) ! startpos can be used without endpos,
    
    if ( errorInt == 0 ) then
        print *, resultValue
    else if ( errorInt == 1 ) then
        print *, "The input string was empty."
    else if ( errorInt == 2 ) then
        print *, "The input string contained an invalid decimal integer."
    else if ( errorInt == 3 ) then
        print *, "The input string contained a value that is smaller than the minimum value of the data type."
    else if ( errorInt == 4 ) then
        print *, "The input string contained a value that is larger than the maximum value of the data type."
    else if ( errorInt == 5 ) then
        print *, "It was either startpos > length, endpos < startpos, or endpos < 1."
    end if
end program example
