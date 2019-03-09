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
!  since: 1n.0.0.f
! 
!  <p>This module, <code>fniDecimalUtil64</code> contains procedures for
!  working with decimal strings and the int64 data type.
!  </p>
!
!  @note  Procedures of this module are not pure procedures.
module fniDecimalUtil64
    use fniConsts
    use fniConsts64
    implicit none
    integer(k_int64), parameter  ::  dmax64 = 9_k_int64
    integer(k_int32), parameter  ::  dmax32 = 9_k_int32
    private                      ::  dmax64, dmax32
contains
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Parse the `input` string as a signed decimal integer string
    !  to an int64 value.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function return a logical value of `.TRUE.` if an error
    !  has occurred during parsing, or `.FALSE.`, otherwise.
    ! 
    !  <p>This function considers an error to has occurred:<br>
    !  1. If the `input` string only contains empty spaces.<br>
    !  2. If the `input` string have a length of zero.<br>
    !  3. If the value for either the `startpos` or `endpos` arguments
    !     is incorrect.<br>
    !  4. If the `input` string contains a value that is smaller than
    !     the min value of the int64 data type or larger then the max
    !     value of the int64 data type.<br>
    !  5. If the `input` string is not a valid signed decimal integer
    !     string.
    !  </p>
    !
    !  @see  <a href="|url|/page/startpos-endpos-explanation.html">
    !        startpos & endpos explanation</a>
    logical function decToInt64(input, output, startpos, endpos) result(error)
        implicit none
        character(len=*), intent(in)            ::  input               !! A string to be parsed as a signed decimal integer string to an int64 value.
        integer(k_int64), intent(out)           ::  output              !! An int64 value of the <code>input</code> string if no error has occurred during parsing.
        integer(k_int32), intent(in), optional  ::  startpos            !! An int32 value of the position(inclusive) of where to start parsing.
        integer(k_int32), intent(in), optional  ::  endpos              !! An int32 value of the position(inclusive) of where to end parsing.
        integer(k_int32)                        ::  c, length, start, runlen, ch1
        output = 0_k_int64 ; error = .TRUE.
        
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

        ! Since ch1 is not going to be used in any equation, ch1
        ! can be int32 and can compare to int32 values. On the otherhand,
        ! output should be compared or process to values of int64.
        ! This is to avoid values from being up convert to int64 
        ! multiple times or constant values from being up convert to
        ! int64. 
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
        if ( output > dmax64 ) return
        start = start + 1
 
        if ( runlen > 18 ) then
            if ( runlen > 19 ) return

            do while ( start < length )
               c = IEOR(ICHAR(input(start:start)), czero32)
               if ( c > dmax32 ) return
               output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
               start = start + 1
            end do

            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) return

            if ( output > 922337203685477579_k_int64 ) then
                if ( output > 922337203685477580_k_int64 ) return
                if ( c > 7 ) then
                   if ( c > 8 ) return
                   if ( ch1 /= cneg32 ) return
                   output = not(ISHFT(output, 1) + ISHFT(output, 3)) + 1_k_int64
                   output = output - INT(c, k_int64)
                   error = .FALSE.
                   return
                end if
            end if
            
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
            if ( ch1 == cneg32 ) output = not(output) + 1_k_int64
            error = .FALSE.
            return
        end if

        do while ( start <= length )
            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) return
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
            start = start + 1
        end do

        if ( ch1 == cneg32 ) output = not(output) + 1_k_int64
        error = .FALSE.
    end function decToInt64
    
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Parse the `input` string as a signed decimal integer string 
    !  to an int64 value.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function return an int32 value of 0 on successful parse
    !  cases or a true error code on unsuccessful parse cases. 
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
    !  @note  This function may take longer on unsuccessful parse cases.
    !
    !  @see  <a href="|url|/page/startpos-endpos-explanation.html">
    !        startpos & endpos explanation</a>
    integer(k_int32) function decToInt64TrueError(input, output, startpos, endpos) result(error)
        implicit none 
        character(len=*), intent(in)            ::  input               !! A string to be parsed as a signed decimal integer string to an int64 value.
        integer(k_int64), intent(out)           ::  output              !! An int64 value of the <code>input</code> string if no error has occurred during parsing.
        integer(k_int32), intent(in), optional  ::  startpos            !! An int32 value of the position(inclusive) of where to start parsing.
        integer(k_int32), intent(in), optional  ::  endpos              !! An int32 value of the position(inclusive) of where to end parsing.
        integer(k_int32)                        ::  c, length, start, runlen, ch1
        output = 0_k_int64 ; error = 0
        
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
        if ( output > dmax64 ) goto 2
        start = start + 1
 
        if ( runlen > 18 ) then
            if ( runlen > 19 ) goto 10
         
            do while ( start < length )
               c = IEOR(ICHAR(input(start:start)), czero32)
               if ( c > dmax32 ) goto 2
               output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
               start = start + 1
            end do
            
            c = IEOR(ICHAR(input(start:start)), czero32)
            if ( c > dmax32 ) goto 2

            if ( output > 922337203685477579_k_int64 ) then
                if ( output > 922337203685477580_k_int64 ) goto 11
                if ( c > 7 ) then
                   if ( c > 8 ) goto 11
                   if ( ch1 /= cneg32 ) goto 4
                   output = not(ISHFT(output, 1) + ISHFT(output, 3)) + 1_k_int64
                   output = output - INT(c, k_int64)
                   return 
                end if
            end if
            
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
            if ( ch1 == cneg32 ) output = not(output) + 1_k_int64
            return
        end if

        do while ( start <= length )
            c = IEOR(ICHAR(input(start:start)), czero32) 
            if ( c > dmax32 ) goto 2
            output = ISHFT(output, 1) + ISHFT(output, 3) + INT(c, k_int64)
            start = start + 1
        end do

        if ( ch1 == cneg32 ) output = not(output) + 1_k_int64
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
    end function decToInt64TrueError

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings as signed decimal integer strings by 
    !  parsing them to int64 values first.
    !
    !  <p>This function will return a logical value of `.TRUE.` if
    !  an error has occurred during parsing either one of the `input`
    !  strings. Otherwise, a logical `.FALSE.` value will be returned.
    !  </p>
    !
    !  @see  [[decToInt64]]
    logical function decCompareAsInt64(firstString, secondString, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  firstString                  !! A string to be compared to the string <code>secondString</code>.
        character(len=*), intent(in)   ::  secondString                 !! A string to be compared to the string <code>firstString</code>.
        integer(k_int32), intent(out)  ::  output                       !! An int32 value of 1 if the `firstString` is larger than the `secondString`, 0 if they are both equal, or -1 if the `firstString` is smaller than the `secondString`. This value may not be a correct value if an error has occurred during parsing either one of the `input` strings.
        integer(k_int64)               ::  n1, n2 
        output = 0 ; error = .FALSE.
        
        if ( (decToInt64(firstString, n1) .eqv. .TRUE.) .OR. &
             (decToInt64(secondString, n2) .eqv. .TRUE.) ) then
            error = .TRUE.
            return
        end if
        
        if ( n1 > n2 ) output = 1
        if ( n1 < n2 ) output = -1
    end function decCompareAsInt64

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings bases on the content of the strings 
    !  reference to int64 values. If the strings are valid signed decimal
    !  integer strings that can be parsed to int64 values then they will
    !  be compared base on their int64 values. Otherwise, the strings
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
    !  4 - valid int64
    !  </p>
    !
    !  @see  [[decToInt64TrueError]]
    integer(k_int32) function decInt64OrSmaller(firstString, secondString)
        implicit none
        character(len=*), intent(in)  ::  firstString                   !! A string to be compared to the string <code>secondString</code>.
        character(len=*), intent(in)  ::  secondString                  !! A string to be compared to the string <code>firstString</code>.
        integer(k_int64)              ::  n1, n2
        integer(k_int32)              ::  e1, e2
        decInt64OrSmaller = 0
        
        e1 = decToInt64TrueError(firstString, n1)
        e2 = decToInt64TrueError(secondString, n2)
        
        if ( e1 == 1 ) then
            if ( e2 == 1 ) return
            
            if ( e2 /= 0 ) then
                decInt64OrSmaller = 1
            else 
                decInt64OrSmaller = -1
            end if
            
            return
        else if ( e2 == 1 ) then
            if ( e1 /= 0 ) then
                decInt64OrSmaller = -1
            else 
                decInt64OrSmaller = 1            
            end if

            return
        end if
        
        if ( e1 == 0  ) then
            if ( e2 == 0 ) then
                if ( n1 > n2 ) decInt64OrSmaller = 1
                if ( n1 < n2 ) decInt64OrSmaller = -1
                return
            end if 
            
            decInt64OrSmaller = 1
            return
        else if ( e2 == 0 ) then
            decInt64OrSmaller = -1
            return
        end if 

        if ( e1 > e2 ) decInt64OrSmaller = 1
        if ( e1 < e2 ) decInt64OrSmaller = -1
    end function decInt64OrSmaller
end module fniDecimalUtil64
