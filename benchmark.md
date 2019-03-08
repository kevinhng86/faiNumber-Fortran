## Benchmark

How fast is faiNumber-Fortran?

The below are benchmark results for faiNumber-Fortran(Version 1.0.0.f)
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
| read()                 | 7.821s | 11.287s | 7.740s |
| decToInt32()           | 0.551s |  1.492s | 1.741s |
| decToInt32TrueError()  | 0.538s |  1.485s | 1.748s |

##### Parsed a string 10,000,000 times.
##### The string: "-9223372036854775808"
##### 2 side spaces: length 100 with 20 spaces to the left
##### Right side spaces: length 100
| | Exact Length  | 2 Sides Spaces | Right Spaces |
|---|---|---|---|
| read()                |  9.915s | 13.515s | 10.026s |
| decToInt64()          |  0.900s |  3.204s |  3.319s |
| decToInt64TrueError() |  0.914s |  3.184s |  3.298s |

##### Parsed a string 10,000,000 times.
##### The string: "-170141183460469231731687303715884105728"
##### 2 side spaces: length 100 with 20 spaces to the left
##### Right side spaces: length 100
| | Exact Length  | 2 Sides Spaces | Right Spaces |
|---|---|---|---|
| read()                 | 16.508s | 20.159s | 16.529s |
| decToInt128()          |  3.846s |  5.565s |  5.506s |
| decToInt128TrueError() |  3.880s |  5.602s |  5.698s |
