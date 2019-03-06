## Benchmark

How fast is faiNumber-Fortran?

The below are benchmark results for faiNumber-Fortran(v.1.0.0.f)
comparing to the read() method of Fortran. Each test ran 3 times and the
average result from all 3 runs is used as the result. Results are in
seconds with 3 decimal placement. For the result, the lower is the better.

Although this benchmark test was run as a build for f2018, other versions
of Fortran(from gfortran's -std=legacy to f2003) were tested lightly and
did show to operate at almost similar or as the same speed as the
benchmark score below.

###### CPU: AMD® A10-7800 radeon r7, 12 compute cores 4c+8g × 4
###### OS: Ubuntu 18.04.1 LTS
###### Compiler: gfortran 8
###### Fotran Version: f2018
<br>

##### Parsed a string 10,000,000 times.
##### The string: "-2147483648"
##### 2 side spaces: length 50 with 20 spaces to the left
##### Right side spaces: length 50
| | Exact Length  | 2 Sides Spaces | Right Spaces |
|---|---|---|---|
| read()                 | 7.815s | 11.239s | 7.704s |
| decToInt32()           | 0.526s |  1.467s | 1.764s |
| decToInt32TrueError()  | 0.521s |  1.463s | 1.749s |

##### Parsed a string 10,000,000 times.
##### The string: "-9223372036854775808"
##### 2 side spaces: length 100 with 20 spaces to the left
##### Right side spaces: length 100
| | Exact Length  | 2 Sides Spaces | Right Spaces |
|---|---|---|---|
| read()                | 10.097s | 13.646s | 10.052s |
| decToInt64()          |  0.881s |  3.174s |  3.282s |
| decToInt64TrueError() |  0.873s |  3.163s |  3.251s |

##### Parsed a string 10,000,000 times.
##### The string: "-170141183460469231731687303715884105728"
##### 2 side spaces: length 100 with 20 spaces to the left
##### Right side spaces: length 100
| | Exact Length  | 2 Sides Spaces | Right Spaces |
|---|---|---|---|
| read()                 | 16.603s | 20.358s | 16.664s |
| decToInt128()          |  3.631s |  5.331s |  5.588s |
| decToInt128TrueError() |  3.580s |  5.434s |  5.485s |
