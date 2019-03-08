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

!| author: Khang Hoang Nguyen
!  license: <a href="https://github.com/kevinhng86/faiNumber-Fortran/blob/master/LICENSE">MIT</a>
!  since: 1.0.0.f
! 
!  <p>This module, <code>fnDecimalUtil128</code> contains procedures for
!  working with decimal strings and the int128 data type.
!  </p>
!
!  @note  Unless stated otherwise, procedures of this module are pure
!         procedures.
module fnDecimalUtil128
    use fnConsts
    use fnConsts128
    implicit none
    integer(k_int128), parameter  ::  dmax128 = 9_k_int128
    integer(k_int32), parameter   ::  dmax32 = 9_k_int32
    private                       ::  dmax128, dmax32
contains
    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Parse the `input` string as a signed decimal integer string
    !  to an int128 value.
    !
    !  <p>This subroutine ignores leading and trailing whitespaces.
    !
    !  <p>This subroutine considers an error to has occurred:<br>
    !  1. If the `input` string only contains empty spaces.<br>
    !  2. If the `input` string have a length of zero.<br>
    !  3. If the value for either the `startpos` or `endpos` arguments
    !     is incorrect.<br>
    !  4. If the `input` string contains a value that is smaller than
    !     the min value of the int128 data type or larger then the max
    !     value of the int128 data type.<br>
    !  5. If the `input` string is not a valid signed decimal integer
    !     string.
    !  </p>
    !
    !  @see  <a href="//lib.fai.host/fortran/faiNumber/v1/startpos-endpos-explanation.html">
    !        startpos & endpos explanation</a>.
    pure subroutine decToInt128(input, output, error, startpos, endpos)
        implicit none 
        character(len=*) , intent(in)            ::  input              !! A string to be parsed as a signed decimal integer string to an int128 value.
        integer(k_int128), intent(out)           ::  output             !! An int128 value of the <code>input</code> string if no error has occurred during parsing.
        logical          , intent(out)           ::  error              !! A value of `.TRUE.` if an error has occurred during parsing, or `.FALSE.`, otherwise.
        integer(k_int32) , intent(in), optional  ::  startpos           !! An int32 value of the position(inclusive) of where to start parsing.
        integer(k_int32) , intent(in), optional  ::  endpos             !! An int32 value of the position(inclusive) of where to end parsing.
        integer(k_int32)                         ::  c, length, start, runlen, ch1
        output = 0_k_int128 ; error = .TRUE.
        
        length = len(input) ; start = 1
        
        if ( present(endpos) ) then
            if ( endpos < 1 ) return
            if ( endpos < length ) length = endpos
        end if
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit 
            length = length - 1
        end do

        if ( present(startpos) ) then
            if ( startpos > 1 ) start = startpos
        end if
        do while( start <= length )
            if ( input(start:start) /= charspace ) exit 
            start = start + 1
        end do
        if ( start > length ) return

        ch1 = ICHAR(input(start:start))
        if ( ch1 == cneg32 .OR. ch1 == cpos32 ) then
            start = start + 1
            if ( start > length ) return
        end if
        
        do while ( start <= length )
            if ( input(start:start) /= charzero ) exit
            start = start + 1
        end do

        runlen = (length + 1) - start
        if ( runlen == 0 ) then
            error = .FALSE.
            return
        end if        
        
        output = IEOR(ICHAR(input(start:start)), czero32)
        if ( output > dmax128 ) return
        start = start + 1
 
        if ( runlen > 38 ) then
            if ( runlen > 39 ) return
            if ( output > 1_k_int128 ) return

            do while ( start < length )
               c = IEOR(ICHAR(input(start:start)), czero32)
               if ( c > dmax32 ) return
               output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
               start = start + 1
            end do
            
            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) return

            if ( output > 17014118346046923173168730371588410571_k_int128 ) then
                if ( output > 17014118346046923173168730371588410572_k_int128 ) return
                if ( c > 7 ) then
                   if ( c > 8 ) return
                   if ( ch1 /= cneg32 ) return
                   output = not(ISHFT(output, 1) + ISHFT(output, 3)) + 1_k_int128
                   output = output - INT(c, k_int128)
                   error = .FALSE.
                   return 
                end if
            end if
            
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
            if ( ch1 == cneg32 ) output = not(output) + 1_k_int128
            error = .FALSE.
            return
        end if

        do while ( start <= length )
            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) return
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
            start = start + 1
        end do

        if ( ch1 == cneg32 ) output = not(output) + 1_k_int128
        error = .FALSE.
    end subroutine decToInt128

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Parse the `input` string as a signed decimal integer string 
    !  to an int128 value.
    !
    !  <p>This subroutine ignores leading and trailing whitespaces.
    !
    !  <p>Error codes:<br>
    !  0 - none <br>
    !  1 - empty strings<br>
    !  2 - invalid format<br>
    !  3 - underflow<br>
    !  4 - overflow<br>
    !  5 - Invalid argument endpos/startpos
    !  </p>
    !
    !  @see  <a href="//lib.fai.host/fortran/faiNumber/v1/startpos-endpos-explanation.html">
    !        startpos & endpos explanation</a>.
    !
    !  @note  This subroutine may take longer on unsuccessful parse cases.    
    pure subroutine decToInt128TrueError(input, output, error, startpos, endpos)
        implicit none 
        character(len=*) , intent(in)            ::  input              !! A string to be parsed as a signed decimal integer string to an int128 value.
        integer(k_int128), intent(out)           ::  output             !! An int128 value of the <code>input</code> string if no error has occurred during parsing.
        integer(k_int32) , intent(out)           ::  error              !! An int32 value of 0 on successful parse cases or a true error code on unsuccessful parse cases. 
        integer(k_int32) , intent(in), optional  ::  startpos           !! An int32 value of the position(inclusive) of where to start parsing.
        integer(k_int32) , intent(in), optional  ::  endpos             !! An int32 value of the position(inclusive) of where to end parsing.
        integer(k_int32)                         ::  c, length, start, runlen, ch1
        output = 0_k_int128 ; error = 0
        
        length = len(input) ; start = 1

        if ( present(endpos) ) then
            if ( endpos < 1 ) goto 5
            if ( present(startpos) ) then
                if ( endpos < startpos ) goto 5
            end if
            if ( endpos < length ) length = endpos
        end if
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        
        if ( present(startpos) ) then
            if ( startpos > length ) goto 5
            if ( startpos > 1 ) start = startpos
        end if
        do while( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) goto 1
        
        ch1 = ICHAR(input(start:start))
        if ( ch1 == cneg32 .OR. ch1 == cpos32 ) then
            start = start + 1
            if ( start > length ) goto 2
        end if

        do while ( start <= length )
            if ( input(start:start) /= charzero ) exit
            start = start + 1
        end do

        runlen = (length + 1) - start
        if ( runlen == 0 ) return
        
        output = IEOR(ICHAR(input(start:start)), czero32)
        if ( output > dmax128 ) goto 2
        start = start + 1
 
        if ( runlen > 38 ) then
            if ( runlen > 39 ) goto 10
            if ( output > 1_k_int128 ) goto 10
            
            do while ( start < length )
               c = IEOR(ICHAR(input(start:start)), czero32)
               if ( c > dmax32 ) goto 2
               output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
               start = start + 1
            end do

            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) goto 2
                          
            if ( output > 17014118346046923173168730371588410571_k_int128 ) then
                if ( output > 17014118346046923173168730371588410572_k_int128 ) goto 11
                if ( c > 7 ) then
                   if ( c > 8 ) goto 11
                   if ( ch1 /= cneg32 ) goto 4
                   output = not(ISHFT(output, 1) + ISHFT(output, 3)) + 1_k_int128
                   output = output - INT(c, k_int128)
                   return
                end if
            end if

            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
            if ( ch1 == cneg32 ) output = not(output) + 1
            return
        end if

        do while ( start <= length )
            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) goto 2
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int128)
            start = start + 1
        end do

        if ( ch1 == cneg32 ) output = not(output) + 1_k_int128
        return

        1 continue
            error = 1
            return
        2 continue
            error = 2
            return
        3 continue
            error = 3
            return        
        4 continue
            error = 4
            return
        5 continue
            error = 5
            return
        10 continue
            do while ( start <= length )
                if ( IEOR(ICHAR(input(start:start)), czero32) > dmax32 ) goto 2
                start = start + 1
            end do
        11 continue 
            if ( ch1 == cneg32 ) goto 3
            goto 4
    end subroutine decToInt128TrueError

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Compare two strings as signed decimal integer strings by 
    !  parsing them to int128 values first.
    !  </p>
    !
    !  @see  [[decToInt128]]
    pure subroutine decCompareAsInt128(firstString, secondString, output, error) 
        implicit none
        character(len=*), intent(in)   ::  firstString                  !! A string to be compared to the string <code>secondString</code>.
        character(len=*), intent(in)   ::  secondString                 !! A string to be compared to the string <code>firstString</code>.
        integer(k_int32), intent(out)  ::  output                       !! An int32 value of 1 if the `firstString` is larger than the `secondString`, 0 if they are both equal, or -1 if the `firstString` is smaller than the `secondString`. This value may not be a correct value if an error has occurred during parsing either one of the `input` strings.
        logical         , intent(out)  ::  error                        !! A value of `.TRUE.` if an error has occurred during parsing either one of the `input` strings. Otherwise, a value of `.FALSE.`.
        integer(k_int128)              ::  n1, n2 
        logical                        ::  e1, e2
        output = 0 ; error = .FALSE.

        call decToInt128(firstString, n1, e1)
        call decToInt128(secondString, n2, e2)
        
        if ( (e1 .eqv. .TRUE.) .OR. (e2 .eqv. .TRUE.) ) then
            error = .TRUE.
            return
        end if
        
        if ( n1 > n2 ) output = 1
        if ( n1 < n2 ) output = -1
    end subroutine decCompareAsInt128

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Compare two strings bases on the content of the strings 
    !  reference to int128 values. If the strings are valid signed decimal
    !  integer strings that can be parsed to int128 values then they will
    !  be compared base on their int128 values. Otherwise, the strings
    !  will be compared base on the priority ranking order below.
    !
    !  <p>This function return an int32 value of 1 if the `firstString`
    !  is larger than the `secondString`, 0 if they are both equal, or
    !  -1 if the `firstString` is smaller than the `secondString`.
    !
    !  <p>Priority order ranking: (lo - hi)<br />
    !  0 - invalid format<br />
    !  1 - underflow<br />
    !  2 - overflow<br />
    !  3 - empty string (0 length or empty space)<br />
    !  4 - valid int128
    !  </p>
    !
    !  @see  [[decToInt128TrueError]]
    pure integer(k_int32) function decInt128OrSmaller(firstString, secondString) result(int32Out)
        implicit none
        character(len=*), intent(in)  ::  firstString                   !! A string to be compared to the string <code>secondString</code>.
        character(len=*), intent(in)  ::  secondString                  !! A string to be compared to the string <code>firstString</code>.
        integer(k_int128)             ::  n1, n2
        integer(k_int32)              ::  e1, e2
        int32Out = 0

        call decToInt128TrueError(firstString, n1, e1)
        call decToInt128TrueError(secondString, n2, e2)

        if ( e1 == 1 ) then
            if ( e2 == 1 ) return
            
            if ( e2 /= 0 ) then
                int32Out = 1
            else 
                int32Out = -1
            end if
            
            return
        else if ( e2 == 1 ) then
            if ( e1 /= 0 ) then
                int32Out = -1
            else 
                int32Out = 1            
            end if

            return
        end if
        
        if ( e1 == 0  ) then
            if ( e2 == 0 ) then
                if ( n1 > n2 ) int32Out = 1
                if ( n1 < n2 ) int32Out = -1
                return
            end if 
            
            int32Out = 1
            return
        else if ( e2 == 0 ) then
            int32Out = -1
            return
        end if

        if ( e1 > e2 ) int32Out = 1
        if ( e1 < e2 ) int32Out = -1
    end function decInt128OrSmaller
end module fnDecimalUtil128
