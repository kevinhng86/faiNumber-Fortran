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
!  <p>This module, <code>fnInt64Util</code> contains procedures for
!  converting int64 values to various type of numerical strings.
!  </p>
!
!  @note  Unless stated otherwise, procedures of this module are pure
!         procedures.
module fnInt64Util
    use fnConsts
    use fnConsts64
    implicit none
    integer(k_int64), parameter  ::  int64_min = -9223372036854775807_k_int64 - 1_k_int64
    private                      ::  int64_min
contains
    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to an unsigned binary
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 64. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=64) function int64ToBinaryAsU(input) result(strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si, i
        strOut = "                                                           "
        
        if ( input == 0_k_int64 ) then
            strOut(64:64) = charzero
            return
        end if

        ! Get value from v was tested to operate stably faster than directly
        ! from input
        v = input

        if ( v < 0_k_int64 ) then
            strOut(61:64) = fnbinv(INT(IBITS(v, 0, 4), k_int32)) 
            strOut(57:60) = fnbinv(INT(IBITS(v, 4, 4), k_int32))
            strOut(53:56) = fnbinv(INT(IBITS(v, 8, 4), k_int32))        
            strOut(49:52) = fnbinv(INT(IBITS(v, 12, 4), k_int32))
            strOut(45:48) = fnbinv(INT(IBITS(v, 16, 4), k_int32))        
            strOut(41:44) = fnbinv(INT(IBITS(v, 20, 4), k_int32))
            strOut(37:40) = fnbinv(INT(IBITS(v, 24, 4), k_int32))        
            strOut(33:36) = fnbinv(INT(IBITS(v, 28, 4), k_int32))
            strOut(29:32) = fnbinv(INT(IBITS(v, 32, 4), k_int32))        
            strOut(25:28) = fnbinv(INT(IBITS(v, 36, 4), k_int32))
            strOut(21:24) = fnbinv(INT(IBITS(v, 40, 4), k_int32))        
            strOut(17:20) = fnbinv(INT(IBITS(v, 44, 4), k_int32))
            strOut(13:16) = fnbinv(INT(IBITS(v, 48, 4), k_int32))
            strOut(9:12)  = fnbinv(INT(IBITS(v, 52, 4), k_int32))
            strOut(5:8)   = fnbinv(INT(IBITS(v, 56, 4), k_int32))
            strOut(1:4)   = fnbinv(INT(IBITS(v, 60, 4), k_int32))
            return
        end if
        
        if ( v < 4294967296_k_int64 ) then
            si = 33
        else 
            si = 1
        end if

        do while( IBITS(v,(61 - si), 4) == 0_k_int64 )
            si = si + 4
        end do
        strOut(si:si+3) = fnbinv(INT(IBITS(v, (61 - si), 4), k_int32))
        i = si + 4
        do while ( si < i ) 
            if ( strOut(si:si) == charzero ) then
                strOut(si:si) = charspace
                si = si + 1
            else 
                si = i
            end if            
        end do
        do while( si < 64 )
            strOut(si:si+3) = fnbinv(INT(IBITS(v, (61 - si), 4), k_int32))
            si = si + 4 
        end do
    end function int64ToBinaryAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to a signed binary string.
    !
    !  <p>This function returns a signed binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 65. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    pure character(len=65) function int64ToBinary(input) result(strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si, i
        strOut = "                                                                "
        
        if ( input == 0_k_int64 ) then
            strOut(65:65) = charzero
            return
        else if ( input == int64_min ) then
            strOut = "-1000000000000000000000000000000000000000000000000000000000000000"
            return
        end if
            
        if ( input < 0_k_int64 ) then
            v = not(input) + 1_k_int64
        else
            v = input
        end if
        
        if ( v < 4294967296_k_int64 ) then
            si = 34 
        else 
            si = 2
        end if

        do while( IBITS(v,(62 - si), 4) == 0_k_int64 )
            si = si + 4
        end do
        strOut(si:si+3) = fnbinv(INT(IBITS(v, (62 - si), 4), k_int32))
        i = si + 4
        do while ( si < i ) 
            if ( strOut(si:si) == charzero ) then
                strOut(si:si) = charspace
                si = si + 1
            else 
                if ( input < 0_k_int64 ) then
                    si = si - 1
                    strOut(si:si) = charneg
                end if 
                si = i
            end if            
        end do
        do while( si < 64 )
            strOut(si:si+3) = fnbinv(INT(IBITS(v, (62 - si), 4), k_int32))
            si = si + 4 
        end do
    end function int64ToBinary

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to an unsigned octal string
    !  as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 22. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>    
    pure character(len=22) function int64ToOctalAsU(input) result(strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si
        strOut = "                     "
        
        if ( input == 0_k_int64 ) then
            strOut(22:22) = charzero
            return
        end if

        v = input
        
        if ( input < 0_k_int64 ) then
            strOut(22:22) = fndigits(INT(IBITS(v, 0, 3), k_int32)) 
            strOut(21:21) = fndigits(INT(IBITS(v, 3, 3), k_int32)) 
            strOut(20:20) = fndigits(INT(IBITS(v, 6, 3), k_int32)) 
            strOut(19:19) = fndigits(INT(IBITS(v, 9, 3), k_int32)) 
            strOut(18:18) = fndigits(INT(IBITS(v, 12, 3), k_int32)) 
            strOut(17:17) = fndigits(INT(IBITS(v, 15, 3), k_int32)) 
            strOut(16:16) = fndigits(INT(IBITS(v, 18, 3), k_int32)) 
            strOut(15:15) = fndigits(INT(IBITS(v, 21, 3), k_int32))
            strOut(14:14) = fndigits(INT(IBITS(v, 24, 3), k_int32))        
            strOut(13:13) = fndigits(INT(IBITS(v, 27, 3), k_int32))
            strOut(12:12) = fndigits(INT(IBITS(v, 30, 3), k_int32))        
            strOut(11:11) = fndigits(INT(IBITS(v, 33, 3), k_int32))
            strOut(10:10) = fndigits(INT(IBITS(v, 36, 3), k_int32))        
            strOut(9:9) = fndigits(INT(IBITS(v, 39, 3), k_int32))
            strOut(8:8) = fndigits(INT(IBITS(v, 42, 3), k_int32))        
            strOut(7:7) = fndigits(INT(IBITS(v, 45, 3), k_int32))
            strOut(6:6) = fndigits(INT(IBITS(v, 48, 3), k_int32))        
            strOut(5:5) = fndigits(INT(IBITS(v, 51, 3), k_int32))
            strOut(4:4) = fndigits(INT(IBITS(v, 54, 3), k_int32))
            strOut(3:3) = fndigits(INT(IBITS(v, 57, 3), k_int32))
            strOut(2:2) = fndigits(INT(IBITS(v, 60, 3), k_int32))
            strOut(1:1) = '1'
            return
        end if 
        
        si = 23

        do while( v /= 0_k_int64 )
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 3), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 3, 3), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 6, 3), k_int32))

            v = ISHFT(v, -9)
        end do
        
        do while ( strOut(si:si) == charzero )
            strOut(si:si) = charspace
            si = si + 1
        end do 
    end function int64ToOctalAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to a signed octal string.
    !
    !  <p>This function returns a signed octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 23. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    pure character(len=23) function int64ToOctal(input) result (strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si
        strOut = "                      "
        
        if ( input == 0_k_int64 ) then
            strOut(23:23) = charzero
            return
        else if ( input == int64_min ) then
            strOut = "-1000000000000000000000"
            return
        end if

        if ( input < 0_k_int64 ) then
            v = not(input) + 1_k_int64
        else
            v = input
        end if
        
        si = 24

        do while( v /= 0_k_int64 )
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 3), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 3, 3), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 6, 3), k_int32))

            v = ISHFT(v, -9)
        end do
        
        do while ( strOut(si:si) == charzero )
            strOut(si:si) = charspace
            si = si + 1
        end do         
        
        if ( input < 0_k_int64 ) then
            if ( strOut(si:si) /= charzero ) si = si - 1
            strOut(si:si) = charneg
        end if
    end function int64ToOctal

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to an unsigned hexadecimal
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 16. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=16) function int64ToHexAsU(input) result(strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si
        strOut = "               " ; 

        if ( input == 0_k_int64 ) then
            strOut(16:16) = charzero
            return
        end if
        
        v = input

        if ( input < 0_k_int64 ) then
            strOut(16:16) = fndigits(INT(IBITS(v, 0, 4), k_int32)) 
            strOut(15:15) = fndigits(INT(IBITS(v, 4, 4), k_int32))
            strOut(14:14) = fndigits(INT(IBITS(v, 8, 4), k_int32))        
            strOut(13:13) = fndigits(INT(IBITS(v, 12, 4), k_int32))
            strOut(12:12) = fndigits(INT(IBITS(v, 16, 4), k_int32))        
            strOut(11:11) = fndigits(INT(IBITS(v, 20, 4), k_int32))
            strOut(10:10) = fndigits(INT(IBITS(v, 24, 4), k_int32))        
            strOut(9:9) = fndigits(INT(IBITS(v, 28, 4), k_int32))
            strOut(8:8) = fndigits(INT(IBITS(v, 32, 4), k_int32))        
            strOut(7:7) = fndigits(INT(IBITS(v, 36, 4), k_int32))
            strOut(6:6) = fndigits(INT(IBITS(v, 40, 4), k_int32))        
            strOut(5:5) = fndigits(INT(IBITS(v, 44, 4), k_int32))
            strOut(4:4) = fndigits(INT(IBITS(v, 48, 4), k_int32))
            strOut(3:3) = fndigits(INT(IBITS(v, 52, 4), k_int32))
            strOut(2:2) = fndigits(INT(IBITS(v, 56, 4), k_int32))
            strOut(1:1) = fndigits(INT(IBITS(v, 60, 4), k_int32))
            return
        end if 

        si = 17
        
        do while( v /= 0_k_int64 )
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 4), k_int32))
            si = si - 1            
            strOut(si:si) = fndigits(INT(IBITS(v, 4, 4), k_int32))
            
            v = ISHFT(v, -8)
        end do

        if ( strOut(si:si) == charzero ) strOut(si:si) = charspace
    end function int64ToHexAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int64 to a signed hexadecimal
    !  string.
    !
    !  <p>This function returns a signed hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 17. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    pure character(len=17) function int64ToHex(input) result(strOut)
        implicit none
        integer(k_int64), intent(in)  ::  input                         !! An int64 value.
        integer(k_int64)              ::  v
        integer(k_int32)              ::  si
        strOut = "                "
        
        if ( input == 0_k_int64 ) then
            strOut(17:17) = charzero
            return
        else if ( input == int64_min ) then
            strOut = "-8000000000000000"
            return
        end if

        if ( input < 0_k_int64 ) then
            v = not(input) + 1_k_int64
        else
            v = input
        end if
        
        si = 18

        do while( v /= 0_k_int64 )
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 4), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 4, 4), k_int32))

            v = ISHFT(v, -8)
        end do
        
        if ( strOut(si:si) == charzero ) then
            strOut(si:si) = charspace
        else 
            si = si - 1
        end if
        
        if ( input < 0_k_int64 ) strOut(si:si) = charneg
    end function int64ToHex
end module fnInt64Util
