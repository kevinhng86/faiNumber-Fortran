# faiNumber-Fortran

faiNumber-Fortran is a fast, flexible, and secured numerical library for
Fortran.

faiNumber-Fortran mainly deals with converting numerical strings to a 
value of an int data type, converting a value of an int data type to a
numerical string, validating numerical strings or comparing strings 
pertain to the mathematical aspect.

Be it whether if it is an overflow, empty strings where it shouldn't be, 
or an invalid input, faiNumber-Fortran's procedures were designed to
always give a notification when an error has occurred where it is
possible to have an error. Also, faiNumber-Fortran number parsers are
secured against invalid format such as " 1 1 ", " 2 b", and so on. Those
formats are parsable and will not yield an error if use the built-in
read() procedure (tested on gfortran8, from legacy up to Fortran 2018).
Thus, faiNumber-Fortran library is a secured library.

faiNumber-Fortran is also flexible when it comes to numerical strings 
parsing. The input strings can either be trimmed or untrimed of both
leading and trailing whitespaces. An optional startpos and endpos
implementation also give the flexibility of where to start in the strings.
faiNumber-Fortran also provides procedures for comparing numerical
strings of unlimited length (constrain by memory and time) length.
When it comes to errors, faiNumber-Fortran give back an error code or
an error status so that the user can handle errors based on their
specific needs.

faiNumber-Fortran is an extremely fast numerical library. For Fortran,
faiNumber-Fortran may be the fastest library for numerical string
parsing. faiNumber-Fortran decimal integer strings parser is faster than
the built-in read() procedure. All other procedures of faiNumber-Fortran
are also very fast, and faiNumber-Fortran always thrive to make them
faster, if possible.

faiNumber-Fortran is written purely in Fortran, there isn't any other
code nor preprocessor from another language. This library also does
not have any dependency. faiNumber-Fortran was also written to provide
support to older(legacy, f95, f2003) versions of Fortran.

This library does not remove any API that was released in any final
build versions without a notice spanning 3 major release versions.

Before using some of the procedures of this library, it is recommended to
read the documentation for what they do before using them as some of the
procedures of this library were built for expert usage. The previous is
especially true for any assume**** procedures that provide by this library.

## Benchmark
<a href="https://github.com/kevinhng86/faiNumber-Fortran/blob/v1/benchmark.md">1.0.0.f</a><br />
<a href="https://github.com/kevinhng86/faiNumber-Fortran/blob/v1n/benchmark.md">1n.0.0.f</a><br />

## License
<a href="https://github.com/kevinhng86/faiNumber-Fortran/blob/master/LICENSE">MIT</a>

## Released Versions
<a href="https://github.com/kevinhng86/faiNumber-Fortran/tree/v1">1.0.0.f</a>
(<a href="https://lib.fai.host/fortran/faiNumber/v1/">Documentation</a>)<br />
<a href="https://github.com/kevinhng86/faiNumber-Fortran/tree/v1n/">1n.0.0.f</a>
(<a href="https://lib.fai.host/fortran/faiNumber/v1n/">Documentation</a>)<br />
