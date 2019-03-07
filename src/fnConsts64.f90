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
!  <p>This module, <code>fnConsts64</code> contains constant values for
!  modules that deal with 64-bit ints.
module fnConsts64
    implicit none
    integer         , parameter  ::  k_int64 = selected_int_kind(18)    !! An integer value for int64's kind.

    integer(k_int64), parameter  ::  czero64 = 48_k_int64               !! An int64 value of the character '0'.
    integer(k_int64), parameter  ::  cneg64 = 45_k_int64                !! An int64 value of the character '-'.
    integer(k_int64), parameter  ::  cpos64 = 43_k_int64                !! An int64 value of the character '+'.
    integer(k_int64), parameter  ::  cspace64 = 32_k_int64              !! An int64 value of space.
end module fnConsts64
