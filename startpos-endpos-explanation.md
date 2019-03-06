# startpos & endpos explanation

This is the general explanation for the `startpos` and `endpos` optional
arguments feature where available. Unless otherwise stated, all procedures
that offer the `startpos` and `endpos` optional arguments will behave
similarly to the description.

### Description

If the optional arguments `startpos` and/or `endpos` are presented, 
the procedure will start operating on the `input` value from the
`startpos` position to the `endpos` position, inclusive of both positions.
If the procedure ignore leading and trailing empty spaces it will still
ignore the begining spaces starting from the `startpos` position and
the ending spaces starting from the `endpos` position.

If the `startpos` position is smaller than 1 then the procedure will start
at the first index.

An error is considered to have occurred if the `startpos` position is larger
than the length of the `input` value. How errors are handled is based on the
procedure.

If the `endpos` position is larger than the length of the `input` value
then the procedure will stop at the last position and inclusive of the 
last position.

An error is considered to have occurred if the `endpos` position is smaller
than 1. How errors are handled is based on the procedure.
