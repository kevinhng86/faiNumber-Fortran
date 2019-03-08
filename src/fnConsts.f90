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
!  <p>This module, <code>fnConsts</code> contains constant values for
!  faiNumber library, and constant values for modules that deal with 
!  32-bit ints.
!  </p>
module fnConsts
    implicit none
    integer, parameter           ::  k_int32 = selected_int_kind(9)     !! An integer value for int32's kind.
    
    integer(k_int32), parameter  ::  czero32 = 48_k_int32               !! An int32 value of the character '0'.
    integer(k_int32), parameter  ::  cneg32 = 45_k_int32                !! An int32 value of the character '-'.
    integer(k_int32), parameter  ::  cpos32 = 43_k_int32                !! An int32 value of the character '+'.
    integer(k_int32), parameter  ::  cspace32 = 32_k_int32              !! An int32 value of space.
    
    character       , parameter  ::  charspace = char(32)               !! A character value of space.
    character       , parameter  ::  charneg   = char(45)               !! A character value of char '-'.
    character       , parameter  ::  charpos   = char(43)               !! A character value of char '+'.
    character       , parameter  ::  charzero  = char(48)               !! A character value of char '0'.

    !| An array of character for representing digits in strings. The first
    !  index is 0 and the last index is 35. Digits of up to base 36 
    !  numbering system can be represented with this array.
    character , dimension(0:35), parameter  :: fndigits = & 
    (/ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',  &
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',  &
       'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',  &
       'u', 'v', 'w', 'x', 'y', 'z'                       &
    /)
    
    !| An array of character for representing value of 0 - 15 in binary
    !  string. Each string is length 4. The first index is 0 and the last
    !  index is 15.
    character(len=4), dimension(0:15) , parameter  ::  fnbinv =                & 
    (/ "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", &
       "1001", "1010", "1011", "1100", "1101", "1110", "1111"                  &
    /)
    
    !| faiNumber's version in string format.
    character(len=*),               parameter  ::  fnVersion = "1.0.0.f"
    
    !| An int32 array of 4 that represents the version number of faiNumber
    !  in integer values.
    integer(k_int32), dimension(4), parameter  ::  fnVersionNo = &
    (/ 1, 0, 0, 0 /)
    
    !| A logical value to determine if this version build is a 
    !  all pure or mostly pure procedures build.
    !
    logical,                        parameter  ::  fnPure = .TRUE.
end module fnConsts
