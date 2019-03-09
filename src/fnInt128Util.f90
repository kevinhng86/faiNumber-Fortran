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
!  <p>This module, <code>fnInt128Util</code> contains procedures for
!  converting int128 values to various type of numerical strings.
!  </p>
!
!  @note  Unless stated otherwise, procedures of this module are pure
!         procedures.
module fnInt128Util
    use fnConsts
    use fnConsts128
    implicit none
    integer(k_int128) , parameter  ::  int128_min = -170141183460469231731687303715884105727_k_int128 &
                                                    - 1_k_int128
    private                        ::  int128_min
contains
    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to an unsigned binary
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 128. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=128) function int128ToBinaryAsU(input) result(strOut)
        implicit none
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut = "                              " &
              // "                              " &
              // "                              " &
              // "                              " &
              // "       "

        if ( input == 0_k_int128 ) then
            strOut(128:128) = '0'
            return
        end if

        v = input

        if ( v < 0_k_int128 ) then
            strOut(125:128) = fnbinv(INT(IBITS(v, 0, 4), k_int32))
            strOut(121:124) = fnbinv(INT(IBITS(v, 4, 4), k_int32))
            strOut(117:120) = fnbinv(INT(IBITS(v, 8, 4), k_int32))
            strOut(113:116) = fnbinv(INT(IBITS(v, 12, 4), k_int32))
            strOut(109:112) = fnbinv(INT(IBITS(v, 16, 4), k_int32))
            strOut(105:108) = fnbinv(INT(IBITS(v, 20, 4), k_int32))
            strOut(101:104) = fnbinv(INT(IBITS(v, 24, 4), k_int32))
            strOut(97:100)  = fnbinv(INT(IBITS(v, 28, 4), k_int32))
            strOut(93:96)   = fnbinv(INT(IBITS(v, 32, 4), k_int32))
            strOut(89:92)   = fnbinv(INT(IBITS(v, 36, 4), k_int32))
            strOut(85:88)   = fnbinv(INT(IBITS(v, 40, 4), k_int32))
            strOut(81:84)   = fnbinv(INT(IBITS(v, 44, 4), k_int32))
            strOut(77:80)   = fnbinv(INT(IBITS(v, 48, 4), k_int32))
            strOut(73:76)   = fnbinv(INT(IBITS(v, 52, 4), k_int32))
            strOut(69:72)   = fnbinv(INT(IBITS(v, 56, 4), k_int32))
            strOut(65:68)   = fnbinv(INT(IBITS(v, 60, 4), k_int32))
            strOut(61:64)   = fnbinv(INT(IBITS(v, 64, 4), k_int32))
            strOut(57:60)   = fnbinv(INT(IBITS(v, 68, 4), k_int32))
            strOut(53:56)   = fnbinv(INT(IBITS(v, 72, 4), k_int32))
            strOut(49:52)   = fnbinv(INT(IBITS(v, 76, 4), k_int32))
            strOut(45:48)   = fnbinv(INT(IBITS(v, 80, 4), k_int32))
            strOut(41:44)   = fnbinv(INT(IBITS(v, 84, 4), k_int32))
            strOut(37:40)   = fnbinv(INT(IBITS(v, 88, 4), k_int32))
            strOut(33:36)   = fnbinv(INT(IBITS(v, 92, 4), k_int32))
            strOut(29:32)   = fnbinv(INT(IBITS(v, 96, 4), k_int32))
            strOut(25:28)   = fnbinv(INT(IBITS(v, 100, 4), k_int32))
            strOut(21:24)   = fnbinv(INT(IBITS(v, 104, 4), k_int32))
            strOut(17:20)   = fnbinv(INT(IBITS(v, 108, 4), k_int32))
            strOut(13:16)   = fnbinv(INT(IBITS(v, 112, 4), k_int32))
            strOut(9:12)    = fnbinv(INT(IBITS(v, 116, 4), k_int32))
            strOut(5:8)     = fnbinv(INT(IBITS(v, 120, 4), k_int32))
            strOut(1:4)     = fnbinv(INT(IBITS(v, 124, 4), k_int32))
            return
        end if

        si = 129

        do while( v /= 0_k_int128 )
            si = si - 4 
            strOut(si:si+3) = fnbinv(INT(IBITS(v, 0, 4), k_int32))
            v = ISHFT(v, -4)
        end do

        do while( strOut(si:si) == charzero ) 
            strOut(si:si) = charspace
            si = si + 1
        end do 
    end function int128ToBinaryAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to a signed binary string.
    !
    !  <p>This function returns a signed binary string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 129. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=129) function int128ToBinary(input) result(strOut)
        implicit none        
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut =  "                              " &
               // "                              " &
               // "                              " &
               // "                              " &
               // "        "

        if ( input == 0_k_int128 ) then
            strOut(129:129) = charzero
            return
        else if ( input == int128_min ) then
            strOut = "-1000000000000000000000000000000000000000000000000000000000000000" &
                  //  "0000000000000000000000000000000000000000000000000000000000000000"
            return
        end if
            
        si = 130
        if ( input < 0_k_int128 ) then
            v = not(input) + 1_k_int128
        else
            v = input
        end if

        do while( v /= 0_k_int128 )
            si = si - 4 
            strOut(si:si+3) = fnbinv(INT(IBITS(v, 0, 4), k_int32))
            v = ISHFT(v, -4)
        end do

        do while( strOut(si:si) == charzero ) 
            strOut(si:si) = charspace
            si = si + 1
        end do 
        
        if ( input < 0_k_int128 ) then
            if ( strOut(si:si) /= charspace ) si = si - 1
            strOut(si:si) = charneg
        end if
    end function int128ToBinary

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to an unsigned octal string
    !  as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 43. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=43) function int128ToOctalAsU(input) result(strOut)
        implicit none
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut = "                                          "
                
        if ( input == 0_k_int128 ) then
            strOut(43:43) = charzero
            return
        end if
        
        v = input
        
        if ( v < 0_k_int128 ) then
            strOut(43:43) = fndigits(INT(IBITS(v, 0, 3), k_int32))
            strOut(42:42) = fndigits(INT(IBITS(v, 3, 3), k_int32))
            strOut(41:41) = fndigits(INT(IBITS(v, 6, 3), k_int32))
            strOut(40:40) = fndigits(INT(IBITS(v, 9, 3), k_int32))
            strOut(39:39) = fndigits(INT(IBITS(v, 12, 3), k_int32))
            strOut(38:38) = fndigits(INT(IBITS(v, 15, 3), k_int32))
            strOut(37:37) = fndigits(INT(IBITS(v, 18, 3), k_int32))
            strOut(36:36) = fndigits(INT(IBITS(v, 21, 3), k_int32))
            strOut(35:35) = fndigits(INT(IBITS(v, 24, 3), k_int32))
            strOut(34:34) = fndigits(INT(IBITS(v, 27, 3), k_int32))
            strOut(33:33) = fndigits(INT(IBITS(v, 30, 3), k_int32))
            strOut(32:32) = fndigits(INT(IBITS(v, 33, 3), k_int32))
            strOut(31:31) = fndigits(INT(IBITS(v, 36, 3), k_int32))
            strOut(30:30) = fndigits(INT(IBITS(v, 39, 3), k_int32))
            strOut(29:29) = fndigits(INT(IBITS(v, 42, 3), k_int32))
            strOut(28:28) = fndigits(INT(IBITS(v, 45, 3), k_int32))
            strOut(27:27) = fndigits(INT(IBITS(v, 48, 3), k_int32))
            strOut(26:26) = fndigits(INT(IBITS(v, 51, 3), k_int32))
            strOut(25:25) = fndigits(INT(IBITS(v, 54, 3), k_int32))
            strOut(24:24) = fndigits(INT(IBITS(v, 57, 3), k_int32))
            strOut(23:23) = fndigits(INT(IBITS(v, 60, 3), k_int32))
            strOut(22:22) = fndigits(INT(IBITS(v, 63, 3), k_int32))
            strOut(21:21) = fndigits(INT(IBITS(v, 66, 3), k_int32))
            strOut(20:20) = fndigits(INT(IBITS(v, 69, 3), k_int32))
            strOut(19:19) = fndigits(INT(IBITS(v, 72, 3), k_int32))
            strOut(18:18) = fndigits(INT(IBITS(v, 75, 3), k_int32))
            strOut(17:17) = fndigits(INT(IBITS(v, 78, 3), k_int32))
            strOut(16:16) = fndigits(INT(IBITS(v, 81, 3), k_int32))
            strOut(15:15) = fndigits(INT(IBITS(v, 84, 3), k_int32))
            strOut(14:14) = fndigits(INT(IBITS(v, 87, 3), k_int32))
            strOut(13:13) = fndigits(INT(IBITS(v, 90, 3), k_int32))
            strOut(12:12) = fndigits(INT(IBITS(v, 93, 3), k_int32))
            strOut(11:11) = fndigits(INT(IBITS(v, 96, 3), k_int32))
            strOut(10:10) = fndigits(INT(IBITS(v, 99, 3), k_int32))
            strOut(9:9)   = fndigits(INT(IBITS(v, 102, 3), k_int32))
            strOut(8:8)   = fndigits(INT(IBITS(v, 105, 3), k_int32))
            strOut(7:7)   = fndigits(INT(IBITS(v, 108, 3), k_int32))
            strOut(6:6)   = fndigits(INT(IBITS(v, 111, 3), k_int32))
            strOut(5:5)   = fndigits(INT(IBITS(v, 114, 3), k_int32))
            strOut(4:4)   = fndigits(INT(IBITS(v, 117, 3), k_int32))
            strOut(3:3)   = fndigits(INT(IBITS(v, 120, 3), k_int32))
            strOut(2:2)   = fndigits(INT(IBITS(v, 123, 3), k_int32))
            strOut(1:1)   = fndigits(INT(IBITS(v, 126, 2), k_int32))
            return
        end if 

        si = 43
        strOut(si:si) = fndigits(INT(IBITS(v, 0, 3), k_int32))
        v = ISHFT(v, -3) 

        do while( v /= 0_k_int128 )
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
    end function int128ToOctalAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to a signed octal string.
    !
    !  <p>This function returns a signed octal string of the `input`
    !  value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 44. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=44) function int128ToOctal(input) result(strOut)
        implicit none
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut = "                                           "
        
        if ( input == 0_k_int128 ) then
            strOut(44:44) = charzero 
            return
        else if ( input == int128_min ) then
            strOut = "-2000000000000000000000000000000000000000000"
            return
        end if

        si = 44
        if ( input < 0_k_int128 ) then
            v = not(input) + 1_k_int128
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 3), k_int32))
            v = ISHFT(v, -3)
        else
            strOut(si:si) = fndigits(INT(IBITS(input, 0, 3), k_int32))
            v = ISHFT(input, -3)
        end if

        do while( v /= 0_k_int128 )
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

        if ( input < 0_k_int128 ) then
            if ( strOut(si:si) /= charzero ) si = si - 1
            strOut(si:si) = charneg
        end if
    end function int128ToOctal

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to an unsigned hexadecimal
    !  string as if the bits are unsigned bits.
    !
    !  <p>This function returns an unsigned hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 32. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified.
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=32) function int128ToHexAsU(input) result(strOut)
        implicit none
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut = "                               "
        
        if ( input == 0_k_int128 ) then
            strOut(32:32) = charzero
            return
        end if

        v = input

        if ( v < 0_k_int128 ) then
            strOut(32:32) = fndigits(INT(IBITS(v, 0, 4), k_int32)) 
            strOut(31:31) = fndigits(INT(IBITS(v, 4, 4), k_int32))
            strOut(30:30) = fndigits(INT(IBITS(v, 8, 4), k_int32))        
            strOut(29:29) = fndigits(INT(IBITS(v, 12, 4), k_int32))
            strOut(28:28) = fndigits(INT(IBITS(v, 16, 4), k_int32))        
            strOut(27:27) = fndigits(INT(IBITS(v, 20, 4), k_int32))
            strOut(26:26) = fndigits(INT(IBITS(v, 24, 4), k_int32))        
            strOut(25:25) = fndigits(INT(IBITS(v, 28, 4), k_int32))
            strOut(24:24) = fndigits(INT(IBITS(v, 32, 4), k_int32))        
            strOut(23:23) = fndigits(INT(IBITS(v, 36, 4), k_int32))
            strOut(22:22) = fndigits(INT(IBITS(v, 40, 4), k_int32))        
            strOut(21:21) = fndigits(INT(IBITS(v, 44, 4), k_int32))
            strOut(20:20) = fndigits(INT(IBITS(v, 48, 4), k_int32))
            strOut(19:19) = fndigits(INT(IBITS(v, 52, 4), k_int32))
            strOut(18:18) = fndigits(INT(IBITS(v, 56, 4), k_int32))
            strOut(17:17) = fndigits(INT(IBITS(v, 60, 4), k_int32))
            strOut(16:16) = fndigits(INT(IBITS(v, 64, 4), k_int32)) 
            strOut(15:15) = fndigits(INT(IBITS(v, 68, 4), k_int32))
            strOut(14:14) = fndigits(INT(IBITS(v, 72, 4), k_int32))        
            strOut(13:13) = fndigits(INT(IBITS(v, 76, 4), k_int32))
            strOut(12:12) = fndigits(INT(IBITS(v, 80, 4), k_int32))        
            strOut(11:11) = fndigits(INT(IBITS(v, 84, 4), k_int32))
            strOut(10:10) = fndigits(INT(IBITS(v, 88, 4), k_int32))        
            strOut(9:9)   = fndigits(INT(IBITS(v, 92, 4), k_int32))
            strOut(8:8)   = fndigits(INT(IBITS(v, 96, 4), k_int32))        
            strOut(7:7)   = fndigits(INT(IBITS(v, 100, 4), k_int32))
            strOut(6:6)   = fndigits(INT(IBITS(v, 104, 4), k_int32))        
            strOut(5:5)   = fndigits(INT(IBITS(v, 108, 4), k_int32))
            strOut(4:4)   = fndigits(INT(IBITS(v, 112, 4), k_int32))
            strOut(3:3)   = fndigits(INT(IBITS(v, 116, 4), k_int32))
            strOut(2:2)   = fndigits(INT(IBITS(v, 120, 4), k_int32))
            strOut(1:1)   = fndigits(INT(IBITS(v, 124, 4), k_int32))
            return
        end if 
        
        si = 33
        
        do while ( v /= 0_k_int128 )
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 0, 4), k_int32))
            si = si - 1
            strOut(si:si) = fndigits(INT(IBITS(v, 4, 4), k_int32))
            
            v = ISHFT(v, -8)
        end do

        if ( strOut(si:si) == charzero ) strOut(si:si) = charspace
    end function int128ToHexAsU

    !| author: Khang Hoang Nguyen
    !  since: 1.0.0.f
    !
    !  <p>Convert the `input` of type int128 to a signed hexadecimal
    !  string.
    !
    !  <p>This function returns a signed hexadecimal string of the
    !  `input` value.
    !
    !  <p>The string that is returned by this function will always have
    !  a length of 33. The return string will never have leading zeroes.
    !  Values within the return string will always be right justified. 
    !  Whitespaces will fill the left side of the return string if there
    !  is not enough values to fill the string.
    !  </p>
    pure character(len=33) function int128ToHex(input) result(strOut)
        implicit none
        integer(k_int128), intent(in)  ::  input                        !! An int128 value.
        integer(k_int128)              ::  v
        integer(k_int32)               ::  si
        strOut = "                                "
        
        if ( input == 0_k_int128 ) then
            strOut(33:33) = charzero
            return
        else if ( input == int128_min ) then
            strOut = "-80000000000000000000000000000000"
            return
        end if

        if ( input < 0_k_int128 ) then
            v = not(input) + 1_k_int128
        else
            v = input
        end if

        si = 34

        do while ( v /= 0_k_int128 )
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
        
        if ( input < 0_k_int128 ) strOut(si:si) = charneg
    end function int128ToHex
end module fnInt128Util
