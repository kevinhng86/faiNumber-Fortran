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
!  <p>This module, <code>fniInt32Util</code> contains procedures for
!  converting int32 values to various type of numerical strings.
!  </p>
!
!  @note  Procedures of this module may not be pure procedures.
module fniInt32Util
    use fniConsts
    implicit none
    integer(k_int32), parameter  :: int32_min = -2147483647 - 1
    private                      :: int32_min
contains
    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to an unsigned binary
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 32. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    character(len=32) function int32ToBinaryAsU(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si, i
        strOut = "                               "
        
        if ( input == 0 ) then
            strOut(32:32) = charzero
            return
        end if

        v = input
        
        if ( input < 0 ) then
            strOut(29:32) = fnbinv(INT(IBITS(v, 0, 4), k_int32))        
            strOut(25:28) = fnbinv(INT(IBITS(v, 4, 4), k_int32))
            strOut(21:24) = fnbinv(INT(IBITS(v, 8, 4), k_int32))        
            strOut(17:20) = fnbinv(INT(IBITS(v, 12, 4), k_int32))
            strOut(13:16) = fnbinv(INT(IBITS(v, 16, 4), k_int32))
            strOut(9:12)  = fnbinv(INT(IBITS(v, 20, 4), k_int32))
            strOut(5:8)   = fnbinv(INT(IBITS(v, 24, 4), k_int32))
            strOut(1:4)   = fnbinv(INT(IBITS(v, 28, 4), k_int32))
            return
        end if

        si = 1
        
        do while( IBITS(v,(29 - si), 4) == 0 )
            si = si + 4
        end do
        strOut(si:si+3) = fnbinv(IBITS(v, (29 - si), 4))
        i = si + 4
        do while ( si < i ) 
            if ( strOut(si:si) == charzero ) then
                strOut(si:si) = charspace
                si = si + 1
            else 
                si = i
            end if 
        end do
        do while( si < 32 )
            strOut(si:si+3) = fnbinv(IBITS(v, (29 - si), 4))
            si = si + 4 
        end do
    end function int32ToBinaryAsU

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to a signed binary string.
    !
    !  <p>This function returns a signed binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 33. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    character(len=33) function int32ToBinary(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si, i
        strOut = "                                "

        if ( input == 0 ) then
            strOut(33:33) = charzero
            return
        else if ( input == int32_min ) then
            strOut = "-10000000000000000000000000000000"
            return
        end if

        if ( input < 0 ) then
            v = not(input) + 1
        else 
            v = input
        end if

        si = 2

        do while( IBITS(v,(30 - si), 4) == 0 )
            si = si + 4
        end do
        strOut(si:si+3) = fnbinv(IBITS(v, (30 - si), 4))
        i = si + 4
        do while ( si < i ) 
            if ( strOut(si:si) == charzero ) then
                strOut(si:si) = charspace
                si = si + 1
            else 
                if ( input < 0 ) then
                    si = si - 1
                    strOut(si:si) = charneg
                end if
                si = i
            end if
        end do
        do while( si < 32 )
            strOut(si:si+3) = fnbinv(IBITS(v, (30 - si), 4))
            si = si + 4
        end do
    end function int32ToBinary

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to an unsigned octal string
    !  as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 11. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    character(len=11) function int32ToOctalAsU(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si
        strOut = "          "

        if ( input == 0 ) then
            strOut(11:11) = charzero 
            return
        end if

        si = 11
        strOut(si:si) = fndigits(IBITS(input, 0, 3))
        v = ISHFT(input, -3)

        do while( v /= 0 )
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 0, 3))
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 3, 3))

            v = ISHFT(v, -6)
        end do

        if ( strOut(si:si) == charzero ) strOut(si:si) = charspace
    end function int32ToOctalAsU

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to a signed octal string.
    !
    !  <p>This function returns a signed octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 12. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    character(len=12) function int32ToOctal(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si
        strOut = "           "
        
        if ( input == 0 ) then
            strOut(12:12) = charzero
            return
        else if ( input == int32_min ) then
            strOut = "-20000000000"
            return
        end if

        si = 12
        if ( input < 0 ) then
            v = not(input) + 1
            strOut(si:si) = fndigits(IBITS(v, 0, 3))
            v = ISHFT(v, -3)
        else
            strOut(si:si) = fndigits(IBITS(input, 0, 3))
            v = ISHFT(input, -3)
        end if

        do while( v /= 0 )
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 0, 3))
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 3, 3))

            v = ISHFT(v, -6)
        end do

        if ( strOut(si:si) == charzero ) then
            strOut(si:si) = charspace
        else 
            si = si - 1
        end if

        if ( input < 0 ) strOut(si:si) = charneg
    end function int32ToOctal

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to an unsigned hexadecimal
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 8. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    character(len=8) function int32ToHexAsU(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si
        strOut = "       "
        
        if ( input == 0 ) then
            strOut(8:8) = charzero
            return
        end if
        
        v = input ; si = 9
        
        do while( v /= 0 )
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 0, 4))
            si = si - 1            
            strOut(si:si) = fndigits(IBITS(v, 4, 4))
            
            v = ISHFT(v, -8)
        end do

        if ( strOut(si:si) == charzero ) strOut(si:si) = charspace
    end function int32ToHexAsU

    !| author: Khang Hoang Nguyen
    !  since: 1n.0.0.f
    !
    !  <p>Convert the `input` of type int32 to a signed hexadecimal
    !  string.
    !
    !  <p>This function returns a signed hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 9. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    character(len=9) function int32ToHex(input) result(strOut)
        implicit none
        integer(k_int32), intent(in)  ::  input                         !! An int32 value.
        integer(k_int32)              ::  v
        integer(k_int32)              ::  si
        strOut = "        "
        
        if ( input == 0 ) then
            strOut(9:9) = charzero
            return
        else if ( input == int32_min ) then
            strOut = "-80000000"
            return
        end if

        if ( input < 0 ) then
            v = not(input) + 1
        else
            v = input
        end if

        si = 10

        do while( v /= 0 )
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 0, 4))
            si = si - 1
            strOut(si:si) = fndigits(IBITS(v, 4, 4))

            v = ISHFT(v, -8)
        end do

        if ( strOut(si:si) == charzero ) then
            strOut(si:si) = charspace
        else 
            si = si - 1
        end if
        
        if ( input < 0 ) strOut(si:si) = charneg
    end function int32ToHex
end module fniInt32Util
