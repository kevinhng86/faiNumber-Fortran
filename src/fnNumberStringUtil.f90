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
!  <p>This module, <code>fnNumberStringUtil</code> contains procedures
!  for validating and comparing numerical strings.
!  
!  For comparing numerical strings, this module has some very powerful
!  and flexible procedures that can compare numerical strings of
!  unlimited length. These procedures, [[assumeCompare]],
!  [[assumeCompareAllBase]], [[fnStringCompare]], and
!  [[fnStringCompareAsBase]] can compare numerical strings without
!  needing to parse the strings to values of a data type. Under the best
!  case scenario, the mentioned procedures can be extremely fast.
!  </p>
!
!  @note  Procedures of this module may not be pure procedures.
module fnNumberStringUtil
    use fnConsts
    implicit none
contains
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid signed decimal integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid signed decimal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  signed decimal integer string.
    !  </p>
    logical function isInteger(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start
        isInteger = .FALSE.

        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do       
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return
        
        if ( input(start:start) == charneg .OR. input(start:start) == charpos ) then
            start = start + 1
            if ( start > length ) return
        else
        end if

        do while( start <= length )
            if ( IEOR(ICHAR(input(start:start)), czero32) > 9) return
            start = start + 1
        end do

        isInteger = .TRUE.
    end function isInteger

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid unsigned decimal integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid unsigned decimal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  unsigned decimal integer string.
    !  </p>
    logical function isUnsignedInteger(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start 
        isUnsignedInteger = .FALSE.

        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        if ( length == 0 ) return
        
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return

        do while( start <= length )
            if ( IEOR(ICHAR(input(start:start)), czero32) > 9 ) return
            start = start + 1
        end do

        isUnsignedInteger = .TRUE.
    end function isUnsignedInteger
    
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid signed binary integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid signed binary integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  signed binary integer string.
    !  </p>  
    logical function isBinary(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start
        isBinary = .FALSE.

        length = len(input) ; start = 1

        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do       
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return
        
        if ( input(start:start) == charneg .OR. input(start:start) == charpos ) then 
            start = start + 1
            if ( start > length ) return
        end if

        do while( start <= length )
            if ( input(start:start) /= '0' .AND. input(start:start) /= '1' ) return
            start = start + 1
        end do

        isBinary = .TRUE.
    end function isBinary
    
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid unsigned binary integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid unsigned binary integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  unsigned binary integer string.
    !  </p>
    logical function isUnsignedBinary(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start
        isUnsignedBinary = .FALSE.

        length = len(input) ; start = 1

        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do       
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return

        do while( start <= length )
            if ( input(start:start) /= '0' .AND. input(start:start) /= '1' ) return
            start = start + 1
        end do

        isUnsignedBinary = .TRUE.
    end function isUnsignedBinary
    
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid signed octal integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid signed octal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  signed octal integer string.
    !  </p>
    logical function isOctal(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start
        isOctal = .FALSE.
        
        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return
        
        if ( input(start:start) == charneg .OR. input(start:start) == charpos ) then 
            start = start + 1
            if ( start > length ) return 
        end if 
        
        do while( start <= length )
            if ( IEOR(ICHAR(input(start:start)), czero32) > 7) return 
            start = start + 1
        end do

        isOctal = .TRUE.
    end function isOctal

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid unsigned octal integer
    !  string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid unsigned octal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  unsigned octal integer string.
    !  </p>
    logical function isUnsignedOctal(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start
        isUnsignedOctal = .FALSE.
        
        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do       
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return

        do while( start <= length )
            if ( IEOR(ICHAR(input(start:start)), czero32) > 7 ) return
            start = start + 1
        end do 

        isUnsignedOctal = .TRUE.
    end function isUnsignedOctal

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid signed hexadecimal
    !  integer string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid signed hexadecimal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  signed hexadecimal integer string.
    !  </p>
    logical function isHex(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, start, c
        isHex = .FALSE.
        
        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do       
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return
        
        if ( input(start:start) == charneg .OR. input(start:start) == charpos ) then
            start = start + 1
            if ( start > length ) return
        end if
        
        do while( start <= length )
            c = ICHAR(input(start:start))

            if ( c > 96 .AND. c < 103 ) then
            else if ( c > 64 .AND. c < 71 ) then
            else 
                c = IEOR(c, czero32)
                if ( c > 9 ) return
            end if
            
            start = start + 1
        end do

        isHex = .TRUE.
    end function isHex

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid unsigned hexadecimal
    !  integer string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function returns a logical value of `.TRUE.` if the `input`
    !  string is a valid unsigned hexadecimal integer string.
    !
    !  <p>A logical value of `.FALSE.` will be returned where the `input`
    !  string is empty(only spaces), 0 length, or is not a valid
    !  unsigned hexadecimal integer string.
    !  </p>
    logical function isUnsignedHex(input)
        implicit none
        character(len=*), intent(in)  ::  input                         !! A string.
        integer(k_int32)              ::  length, c, start
        isUnsignedHex = .FALSE.

        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return

        do while( start <= length )
            c = ICHAR(input(start:start))

            if ( c > 96 .AND. c < 103 ) then
            else if ( c > 64 .AND. c < 71 ) then
            else 
                c = IEOR(c, czero32)
                if ( c > 9 ) return
            end if
            
            start = start + 1            
        end do 

        isUnsignedHex = .TRUE.
    end function isUnsignedHex

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid signed integer string
    !  of the numbering system with the radix that is defined by the 
    !  value of the argument `base`.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !  
    !  <p>This function support numbering system with radix from 2 to 36.
    ! 
    !  <p>This function returns a logical value of `.TRUE.` on error. 
    !  Otherwise, a logical value of `.FALSE.` is returned.
    !
    !  <p>This function considers an error to has occurred when the 
    !  value of the argument `base` is smaller than 2 or larger than 36.
    !  </p>
    logical function isBase(input, base, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  input                        !! A string.
        integer(k_int32), intent(in)   ::  base                         !! An int32 value that define the radix.
        logical         , intent(out)  ::  output                       !! `.TRUE.` if the `input` string is a valid signed integer string of the defined numbering system.
                                                                        !! `.FALSE.` if the `input` string is empty(only spaces), 0 length, or is not a valid signed integer string of the defined numbering system.
        integer(k_int32)               ::  length, start, c
        output = .FALSE. ; error = .FALSE.
        
        if ( base < 2 .OR. base > 36 ) then 
            error = .TRUE.
            return
        end if

        length = len(input) ; start = 1

        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return

        if ( input(start:start) == charneg .OR. input(start:start) == charpos ) then 
            start = start + 1
            if ( start > length ) return 
        end if 

        do while (start <= length)
            c = ICHAR(input(start:start))

            if ( c > 96 ) then
                c = c - 87
            else if ( c > 64 ) then
                c = c - 55
            else 
                c = IEOR(c, czero32)
                if ( c > 9 ) return
            end if

            if ( .NOT. (c < base)  ) return
            start = start + 1
        end do
        
        output = .TRUE.
    end function isBase

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check if the `input` string is a valid unsigned integer string
    !  of the numbering system with the radix that is defined by the
    !  value of the argument `base`.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !  
    !  <p>This function support numbering system with radix from 2 to 36.
    ! 
    !  <p>This function returns a logical value of `.TRUE.` on error. 
    !  Otherwise, a logical value of `.FALSE.` is returned.
    !
    !  <p>This function considers an error to has occurred when the 
    !  value of the argument `base` is smaller than 2 or larger than 36.
    !  </p>
    logical function isUnsignedBase(input, base, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  input                        !! A string.
        integer(k_int32), intent(in)   ::  base                         !! An int32 value that define the radix.
        logical         , intent(out)  ::  output                       !! `.TRUE.` if the `input` string is a valid unsigned integer string of the defined numbering system.
                                                                        !! `.FALSE.` if the `input` string is empty(only spaces), 0 length, or is not a valid unsigned integer string of the defined numbering system.
        integer(k_int32)               ::  length, c, start
        output = .FALSE. ; error = .FALSE.
        
        if ( base < 2 .OR. base > 36 ) then 
            error = .TRUE.
            return
        end if

        length = len(input) ; start = 1
        
        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        do while ( start <= length )
            if ( input(start:start) /= charspace ) exit
            start = start + 1
        end do
        if ( start > length ) return
        
        do while (start <= length)
            c = ICHAR(input(start:start))

            if ( c > 96 ) then
                c = c - 87
            else if ( c > 64 ) then
                c = c - 55
            else 
                c = IEOR(c, czero32)
                if ( c > 9 ) return
            end if 

            if ( .NOT. (c < base) ) return
            start = start + 1
        end do
        
        output = .TRUE.
    end function isUnsignedBase

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check the `input` string to see if it holds an odd value with
    !  the assumption that the string is a valid decimal integer string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !  
    !  <p>This function returns a logical value of `.TRUE.` on error. 
    !  Otherwise, a logical value of `.FALSE.` is returned.
    !
    !  <p>This function considers an error to has occurred if the `input`
    !  string is empty(only spaces) or is 0 length.
    !  </p>
    logical function assumeIsOdd(input, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  input                        !! A string.
        logical         , intent(out)  ::  output                       !! `.TRUE.` if the rightmost character(exclude trailing spaces) is an odd value, or otherwise, `.FALSE.`.
        integer(k_int32)               ::  length
        output = .FALSE. ; error = .FALSE.
        
        length = len(input)

        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        if ( length == 0 ) then
            error = .TRUE.
            return
        end if
        
        output = BTEST(ICHAR(input(length:length)), 0)
    end function assumeIsOdd

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Check the `input` string to see if it holds an even value with
    !  the assumption that the string is a valid decimal integer string.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !  
    !  <p>This function returns a logical value of `.TRUE.` on error. 
    !  Otherwise, a logical value of `.FALSE.` is returned.
    !
    !  <p>This function considers an error to has occurred if the `input`
    !  string is empty(only spaces) or is 0 length.
    !  </p>
    logical function assumeIsEven(input, output) result(error)
        character(len=*), intent(in)   ::  input                        !! A string.
        logical         , intent(out)  ::  output                       !! `.TRUE.` if the rightmost character(exclude trailing spaces) is an even value, or otherwise, `.FALSE.`.
        integer(k_int32)               ::  length
        output = .FALSE. ; error = .FALSE.

        length = len(input)

        do while ( length > 0 )
            if ( input(length:length) /= charspace ) exit
            length = length - 1
        end do
        if ( length == 0 ) then 
            error = .TRUE.
            return
        end if
        
        output = .NOT. BTEST(ICHAR(input(length:length)), 0)
    end function assumeIsEven

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings with the assumption that both strings are
    !  valid decimal integers.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function evaluates both, negative and positive values. This
    !  function may produce unwanted result if the previous is not met or
    !  if the number formatting is incorrect.
    !
    !  <p>This function considers empty strings as smallest. This function
    !  disregards leading zeroes. This function considers a string with
    !  only a '-' or a '+' sign as 0.
    !    
    !  <p>This function return an int32 value of 1 if the `firstString`
    !  is larger than the `secondString`, 0 if they are both equal, or
    !  -1 if the `firstString` is smaller than the `secondString`.
    !  </p>
    integer(k_int32) function assumeCompare(firstString, secondString)
        implicit none
        character(len=*), intent(in)  ::  firstString                   !! A string to be compared to the string `secondString`.
        character(len=*), intent(in)  ::  secondString                  !! A string to be compared to the string `firstString`.
        integer(k_int32)              ::  str1length, str2length
        integer(k_int32)              ::  start1, start2, runlen1, runlen2, c1, c2
        logical                       ::  neg1, neg2
        assumeCompare = 0

        neg1 = .FALSE. ; neg2 = .FALSE.
        start1 = 1 ; start2 = 1
        str1length = len(firstString) ; str2length = len(secondString) 

        do while ( str1length > 0 )
            if ( firstString(str1length:str1length) /= charspace ) exit
            str1length = str1length - 1
        end do        
        do while ( start1 <= str1length )
            if ( firstString(start1:start1) /= charspace ) exit
            start1 = start1 + 1
        end do
        
        do while ( str2length > 0 )
            if ( secondString(str2length:str2length) /= charspace ) exit
            str2length = str2length - 1
        end do
        do while ( start2 <= str2length )
            if ( secondString(start2:start2) /= charspace ) exit
            start2 = start2 + 1
        end do

        runlen1 = str1length - start1
        runlen2 = str2length - start2 
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) return
            assumeCompare = -1
            return
        else if ( runlen2 == -1 ) then
            assumeCompare = 1
            return
        end if

        c1 = ICHAR(firstString(start1:start1))
        if( c1 == cneg32 ) then
            start1 = start1 + 1
            neg1 = .TRUE.
        else if ( c1 == cpos32 ) then
            start1 = start1 + 1
        end if
            
        c2 = ICHAR(secondString(start2:start2))
        if( c2 == cneg32 ) then
            start2 = start2 + 1
            neg2 = .TRUE.
        else if ( c2 == cpos32 ) then
            start2 = start2 + 1
        end if

        do while( start1 <= str1length )
            if ( firstString(start1:start1) /= charzero ) exit
            start1 = start1 + 1
        end do
        do while( start2 <= str2length )
            if ( secondString(start2:start2) /= charzero ) exit
            start2 = start2 + 1
        end do

        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) return

            if ( neg2 .eqv. .TRUE. ) then
                assumeCompare = 1
            else
                assumeCompare = -1
            end if

            return
        else if ( runlen2 == -1 ) then
            if ( neg1 .eqv. .TRUE. ) then
                assumeCompare = -1
            else
                assumeCompare = 1
            end if

            return
        end if

        if ( neg1 .neqv. neg2 ) then
            if ( neg1 .eqv. .FALSE. ) then
                assumeCompare = 1
            else
                assumeCompare = -1
            end if

            return
        end if

        if ( neg1 .eqv. .TRUE. ) then
            if ( runlen1 /= runlen2 ) then
                if ( runlen1 > runlen2 ) then
                    assumeCompare = -1
                else
                    assumeCompare = 1
                end if
                
                return
            end if

            do while ( runlen1 > -1 )
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))

                if ( c1 < c2 ) then
                    assumeCompare = 1
                    return
                end if
                if ( c1 > c2 ) then
                    assumeCompare = -1
                    return
                end if

                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        else
            if ( runlen1 /= runlen2 ) then
                if ( runlen1 > runlen2 ) then
                    assumeCompare = 1
                else
                    assumeCompare = -1
                end if

                return
            end if

            do while( runlen1 > -1 )
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))
                
                if ( c1 > c2 ) then
                    assumeCompare = 1
                    return
                end if
                if ( c1 < c2 ) then
                    assumeCompare = -1
                    return
                end if

                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        end if
    end function assumeCompare

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings with the assumption that both strings are
    !  valid integers of any radix between 2 and 36.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function evaluates both, negative and positive values.
    !  This function will work, if both strings are integers of the same
    !  radix and the radix is from 2 to 36. This function may produce
    !  unwanted result if the previous are not met or if the formatting
    !  is incorrect.
    !
    !  <p>Digits of radix larger than 10 are 'a' to 'z' with
    !  'a'  being 10, and 'z' being 35. A is equal to a, Z is equal to
    !  z, and so forth.
    !
    !  <p>This function considers empty strings as smallest. This function
    !  disregards leading zeroes. This function considers a string with
    !  only a '-' or a '+' sign as 0.
    !  
    !  <p>This function return an int32 value of 1 if the `firstString`
    !  is larger than the `secondString`, 0 if they are both equal, or
    !  -1 if the `firstString` is smaller than the `secondString`.
    !  </p>
    integer(k_int32) function assumeCompareAllBase(firstString, secondString)
        implicit none
        character(len=*), intent(in)  ::  firstString                   !! A string to be compared to the string `secondString`.
        character(len=*), intent(in)  ::  secondString                  !! A string to be compared to the string `firstString`.
        integer(k_int32)              ::  str1length, str2length
        integer(k_int32)              ::  start1, start2, runlen1, runlen2, c1, c2
        logical                       ::  neg1, neg2
        assumeCompareAllBase = 0

        neg1 = .FALSE. ; neg2 = .FALSE.
        start1 = 1 ; start2 = 1
        str1length = len(firstString) ; str2length = len(secondString)
        
        do while ( str1length > 0 )
            if ( firstString(str1length:str1length) /= charspace ) exit
            str1length = str1length - 1
        end do
        do while ( start1 <= str1length )
            if ( firstString(start1:start1) /= charspace ) exit
            start1 = start1 + 1
        end do
        
        do while ( str2length > 0 )
            if ( secondString(str2length:str2length) /= charspace ) exit
            str2length = str2length - 1
        end do
        do while ( start2 <= str2length )
            if ( secondString(start2:start2) /= charspace ) exit
            start2 = start2 + 1
        end do

        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) return
            assumeCompareAllBase = -1
            return
        else if ( runlen2 == -1 ) then
            assumeCompareAllBase = 1
            return
        end if

        c1 = ICHAR(firstString(start1:start1))
        if( c1 == cneg32 ) then
            start1 = start1 + 1
            neg1 = .TRUE.
        else if ( c1 == cpos32 ) then
            start1 = start1 + 1
        end if
            
        c2 = ICHAR(secondString(start2:start2))
        if( c2 == cneg32 ) then
            start2 = start2 + 1
            neg2 = .TRUE.
        else if ( c2 == cpos32 ) then
            start2 = start2 + 1
        end if

        do while( start1 <= str1length )
            if ( firstString(start1:start1) /= charzero  ) exit
            start1 = start1 + 1
        end do 
        do while( start2 <= str2length ) 
            if ( secondString(start2:start2) /= charzero ) exit
            start2 = start2 + 1
        end do  
                      
        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) return
            
            if ( neg2 .eqv. .TRUE. ) then
                assumeCompareAllBase = 1
            else
                assumeCompareAllBase = -1
            end if
            
            return
        else if ( runlen2 == -1 ) then
            if ( neg1 .eqv. .TRUE. ) then
                assumeCompareAllBase = -1
            else
                assumeCompareAllBase = 1
            end if

            return
        end if 

        if ( neg1 .neqv. neg2 ) then
            if ( neg1 .eqv. .FALSE. ) then
                assumeCompareAllBase = 1
            else 
                assumeCompareAllBase = -1
            end if

            return
        end if

        if ( neg1 .eqv. .TRUE. ) then
            if ( runlen1 /= runlen2 ) then
                if ( runlen1 > runlen2 ) then
                    assumeCompareAllBase = -1
                else
                    assumeCompareAllBase = 1
                end if

                return
            end if

            do while ( runlen1 > -1 ) 
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))

                if ( c1 > 96) then
                    c1 = c1 - 87
                else if ( c1 > 64) then
                    c1 = c1 - 55
                else
                    c1 = IEOR(c1, czero32)
                end if
                
                if ( c2 > 96) then
                    c2 = c2 - 87
                else if ( c2 > 64) then
                    c2 = c2 - 55
                else 
                    c2 = IEOR(c2, czero32)
                end if

                if ( c1 < c2 ) then 
                    assumeCompareAllBase = 1
                    return 
                end if 
                if ( c1 > c2 ) then 
                    assumeCompareAllBase = -1
                    return
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        else 
            if ( runlen1 /= runlen2 ) then
                if ( runlen1 > runlen2 ) then
                    assumeCompareAllBase = 1
                else
                    assumeCompareAllBase = -1
                end if
                
                return
            end if

            do while( runlen1 > -1 )
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))
                
                if ( c1 > 96) then
                    c1 = c1 - 87
                else if ( c1 > 64) then 
                    c1 = c1 - 55
                else 
                    c1 = IEOR(c1, czero32)
                end if
                
                if ( c2 > 96) then
                    c2 = c2 - 87
                else if ( c2 > 64) then
                    c2 = c2 - 55
                else 
                    c2 = IEOR(c2, czero32)
                end if
                
                if ( c1 > c2 ) then 
                    assumeCompareAllBase = 1
                    return
                end if
                if ( c1 < c2 ) then
                    assumeCompareAllBase = -1
                    return
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        end if
    end function assumeCompareAllBase
 
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings as decimal integers. This function evaluates
    !  both, negative and positive values. There isn't a maximum length
    !  for the strings. Nonetheless, the strings can't be empty. 
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function disregards leading zeroes.
    !
    !  <p>This function return an int32 value of 0 if it did not encounter
    !  an error during the comparison process. Otherwise, it will return
    !  a true error code.
    !
    !  <p>Error code:<br />
    !  0 - no error<br />
    !  1 - empty string `firstString`(spaces only or 0 length)<br />
    !  2 - empty string `secondString`(spaces only or 0 length)<br />
    !  3 - bad format `firstString`<br />
    !  4 - bad format `secondString`
    !  </p>
    integer(k_int32) function fnStringCompare(firstString, secondString, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  firstString                  !! A string to be compared to the string `secondString`.
        character(len=*), intent(in)   ::  secondString                 !! A string to be compared to the string `firstString`.
        integer(k_int32), intent(out)  ::  output                       !! An int32 value of 1 if the `firstString` is larger than the `secondString`, 0 if they are both equal, or -1 if the `firstString` is smaller than the `secondString`. This value may not be a correct value if an error has occurred during the comparison process.
        integer(k_int32)               ::  str1length, str2length
        integer(k_int32)               ::  c1, c2
        integer(k_int32)               ::  runlen1, runlen2, start1, start2
        logical                        ::  neg1, neg2
        output = 0 ; error = 0

        neg1 = .FALSE. ; neg2 = .FALSE.
        start1 = 1 ; start2 = 1
        str1length = len(firstString) ; str2length = len(secondString) 

        do while ( str1length > 0 )
            if ( firstString(str1length:str1length) /= charspace ) exit
            str1length = str1length - 1
        end do
        do while ( start1 <= str1length )
            if ( firstString(start1:start1) /= charspace ) exit
            start1 = start1 + 1
        end do
        
        do while ( str2length > 0 )
            if ( secondString(str2length:str2length) /= charspace ) exit
            str2length = str2length - 1
        end do
        do while ( start2 <= str2length )
            if ( secondString(start2:start2) /= charspace ) exit
            start2 = start2 + 1
        end do

        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) goto 1
        if ( runlen2 == -1 ) goto 2
        
        c1 = ICHAR(firstString(start1:start1))
        if( c1 == cneg32 ) then
            start1 = start1 + 1
            if ( start1 > str1length ) goto 3
            neg1 = .TRUE.
        else if( c1 == cpos32 ) then
            start1 = start1 + 1
            if ( start1 > str1length ) goto 3
        end if

        c2 = ICHAR(secondString(start2:start2))
        if( c2 == cneg32 ) then
            start2 = start2 + 1
            if ( start2 > str2length ) goto 4
            neg2 = .TRUE.
        else if( c2 == cpos32 ) then
            start2 = start2 + 1
            if ( start2 > str2length ) goto 4
        end if

        do while( start1 <= str1length )
            if ( firstString(start1:start1) /= charzero ) exit 
            start1 = start1 + 1
        end do 
        do while( start2 <= str2length )
            if ( secondString(start2:start2) /= charzero ) exit
            start2 = start2 + 1
        end do
        
        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) then 
                output = 0
            else if ( neg2 .eqv. .TRUE. ) then 
                output = 1
            else 
                output = -1
            end if
            
            goto 555
        else if ( runlen2 == -1 ) then
            if ( neg1 .eqv. .TRUE. ) then
                output = -1
            else 
                output = 1
            end if
            
            goto 555
        end if
 
        if ( neg1 .neqv. neg2 ) then
            if ( neg1 .eqv. .FALSE. ) then 
                output = 1
            else 
                output = -1
            end if 
            
            goto 555
        end if
        
        if ( neg1 .eqv. .TRUE. ) then
            if ( runlen1 /= runlen2 ) then 
                if ( runlen1 > runlen2 ) then
                    output = -1
                else 
                    output = 1
                end if 
                
                goto 555
            end if
 
            do while ( runlen1 > -1 ) 
                c1 = IEOR(ICHAR(firstString(start1:start1)), czero32)
                c2 = IEOR(ICHAR(secondString(start2:start2)), czero32)
                if ( c1 > 9 ) goto 3
                if ( c2 > 9 ) goto 4
                                                
                if ( c1 < c2 ) then
                    output = 1
                    goto 555
                end if
                if ( c1 > c2 ) then
                    output = -1
                    goto 555
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        else 
            if ( runlen1 /= runlen2 ) then 
                if ( runlen1 > runlen2 ) then 
                    output = 1
                else 
                    output = -1
                end if
                
                goto 555
            end if

            do while( runlen1 > -1 )
                c1 = IEOR(ICHAR(firstString(start1:start1)), czero32)
                c2 = IEOR(ICHAR(secondString(start2:start2)), czero32)
                if ( c1 > 9 ) goto 3
                if ( c2 > 9 ) goto 4

                if ( c1 > c2 ) then
                    output = 1
                    goto 555
                end if
                if ( c1 < c2 ) then
                    output = -1
                    goto 555
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        end if

        555 continue
        do while ( start1 <= str1length )
            if ( IEOR(ICHAR(firstString(start1:start1)), czero32) > 9 ) goto 3
            start1 = start1 + 1
        end do 
        do while ( start2 <= str2length )
            if ( IEOR(ICHAR(secondString(start2:start2)), czero32) > 9 ) goto 4
            start2 = start2 + 1
        end do
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
    end function fnStringCompare
    
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Compare two strings as integer strings of a numbering system 
    !  with the radix that is defined by the value of the argument `base`.
    !  There isn't a maximum length for the strings. Nonetheless, the
    !  strings can't be empty. 
    !
    !  <p>This function support numbering system with radix from 2 to 36.
    !  This function evaluates both, negative and positive values.
    !
    !  <p>This function ignores leading and trailing whitespaces.
    !
    !  <p>This function disregards leading zeroes.
    !
    !  <p>This function return an int32 value of 0 if it did not encounter
    !  an error during the comparison process. Otherwise, it will return
    !  a true error code.
    !
    !  <p>Error code:<br />
    !  0 - no error<br />
    !  1 - empty `firstString`<br />
    !  2 - empty `secondString`<br />
    !  3 - bad format `firstString`<br />
    !  4 - bad format `secondString`<br />
    !  5 - `base` < 2 or `base` > 36
    !  </p>
    integer(k_int32) function fnStringCompareAsBase(firstString, secondString, base, output) result(error)
        implicit none
        character(len=*), intent(in)   ::  firstString                  !! A string to be compared to the string `secondString`.
        character(len=*), intent(in)   ::  secondString                 !! A string to be compared to the string `firstString`.
        integer(k_int32), intent(in)   ::  base                         !! An int32 value that define the radix.
        integer(k_int32), intent(out)  ::  output                       !! An int32 value of 1 if the `firstString` is larger than the `secondString`, 0 if they are both equal, or -1 if the `firstString` is smaller than the `secondString`. This value may not be a correct value if an error has occurred during the comparison process.
        integer(k_int32)               ::  str1length, str2length
        integer(k_int32)               ::  c1, c2
        integer(k_int32)               ::  runlen1, runlen2, start1, start2
        logical                        ::  neg1, neg2
        if ( base < 2 .OR. base > 36 ) goto 5
        output = 0; error = 0

        neg1 = .FALSE. ; neg2 = .FALSE.
        start1 = 1 ; start2 = 1
        str1length = len(firstString) ; str2length = len(secondString) 

        do while ( str1length > 0 )
            if ( firstString(str1length:str1length) /= charspace ) exit
            str1length = str1length - 1
        end do
        do while ( start1 <= str1length )
            if ( firstString(start1:start1) /= charspace ) exit
            start1 = start1 + 1
        end do

        do while ( str2length > 0 )
            if ( secondString(str2length:str2length) /= charspace ) exit
            str2length = str2length - 1
        end do
        do while ( start2 <= str2length )
            if ( secondString(start2:start2) /= charspace ) exit
            start2 = start2 + 1
        end do

        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) goto 1
        if ( runlen2 == -1 ) goto 2
        
        c1 = ICHAR(firstString(start1:start1))
        if( c1 == cneg32 ) then
            start1 = start1 + 1
            if ( start1 > str1length ) goto 3
            neg1 = .TRUE.
        else if( c1 == cpos32 ) then
            start1 = start1 + 1
            if ( start1 > str1length ) goto 3
        end if

        c2 = ICHAR(secondString(start2:start2))
        if( c2 == cneg32 ) then
            start2 = start2 + 1
            if ( start2 > str2length ) goto 4
            neg2 = .TRUE.
        else if( c2 == cpos32 ) then
            start2 = start2 + 1
            if ( start2 > str2length ) goto 4
        end if

        do while( start1 <= str1length )
            if ( firstString(start1:start1) /= charzero ) exit
            start1 = start1 + 1
        end do 
        do while( start2 <= str2length )
            if ( secondString(start2:start2) /= charzero ) exit
            start2 = start2 + 1
        end do
        
        runlen1 = str1length - start1
        runlen2 = str2length - start2
        if ( runlen1 == -1 ) then
            if ( runlen2 == -1 ) then
                output = 0
            else if ( neg2 .eqv. .TRUE. ) then
                output = 1
            else
                output = -1
            end if
            
            goto 555
        else if ( runlen2 == -1 ) then
            if ( neg1 .eqv. .TRUE. ) then
                output = -1
            else 
                output = 1
            end if
            
            goto 555
        end if
 
        if ( neg1 .neqv. neg2 ) then
            if ( neg1 .eqv. .FALSE. ) then
                output = 1
            else
                output = -1
            end if
            
            goto 555
        end if
        
        if ( neg1 .eqv. .TRUE. ) then
            if ( runlen1 /= runlen2 ) then
                if ( runlen1 > runlen2 ) then
                    output = -1
                else
                    output = 1
                end if 
                
                goto 555
            end if
 
            do while ( runlen1 > -1 ) 
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))
                
                if ( c1 > 96 ) then 
                    c1 = c1 - 87
                else if ( c1 > 64 ) then
                    c1 = c1 - 55
                else 
                    c1 = IEOR(c1, czero32)
                    if ( c1 > 9 ) goto 3
                end if                 
                if ( .NOT. (c1 < base) ) goto 3

                if ( c2 > 96 ) then 
                    c2 = c2 - 87
                else if ( c2 > 64 ) then 
                    c2 = c2 - 55
                else 
                    c2 = IEOR(c2, czero32)
                    if ( c2 > 9 ) goto 4
                end if
                if ( .NOT. (c2 < base) ) goto 4
                                                
                if ( c1 < c2 ) then
                    output = 1
                    goto 555
                end if 
                
                if ( c1 > c2 ) then
                    output = -1
                    goto 555
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        else 
            if ( runlen1 /= runlen2 ) then 
                if ( runlen1 > runlen2 ) then 
                    output = 1
                else 
                    output = -1
                end if 
                
                goto 555
            end if

            do while( runlen1 > -1 )
                c1 = ICHAR(firstString(start1:start1))
                c2 = ICHAR(secondString(start2:start2))
                
                if ( c1 > 96 ) then 
                    c1 = c1 - 87
                else if ( c1 > 64 ) then
                    c1 = c1 - 55
                else 
                    c1 = IEOR(c1, czero32)
                    if ( c1 > 9 ) goto 3
                end if
                if ( .NOT. (c1 < base) ) goto 3

                if ( c2 > 96 ) then 
                    c2 = c2 - 87
                else if ( c2 > 64 ) then 
                    c2 = c2 - 55
                else
                    c2 = IEOR(c2, czero32)
                    if ( c2 > 9 ) goto 4
                end if
                if ( .NOT. (c2 < base) ) goto 4
                
                if ( c1 > c2 ) then
                    output = 1
                    goto 555
                end if
                        
                if ( c1 < c2 ) then
                    output = -1
                    goto 555
                end if
                
                start1 = start1 + 1 ; start2 = start2 + 1
                runlen1 = runlen1 - 1
            end do
        end if

        555 continue
        do while ( start1 <= str1length )
            c1 = ICHAR(firstString(start1:start1))
            if ( c1 > 96 ) then 
                c1 = c1 - 87
            else if ( c1 > 64 ) then
                c1 = c1 - 55
            else 
                c1 = IEOR(c1, czero32)
                if ( c1 > 9 ) goto 3
            end if 
            
            if ( .NOT. (c1 < base) ) goto 3
            start1 = start1 + 1
        end do 
        do while ( start2 <= str2length )
            c2 = ICHAR(secondString(start2:start2))
            if ( c2 > 96 ) then
                c2 = c2 - 87
            else if ( c2 > 64 ) then
                c2 = c2 - 55
            else 
                c2 = IEOR(c2, czero32)
                if ( c2 > 9 ) goto 4
            end if
            
            if ( .NOT. (c2 < base) ) goto 4
            start2 = start2 + 1
        end do
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
            output = 0
            error = 5
    end function fnStringCompareAsBase
end module fnNumberStringUtil
