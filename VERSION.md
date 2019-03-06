# Version Explanation

For the first version of faiNumber, there will be two releases, version
1.0.0.f, and version 1n.0.0.f. Unless stated otherwise, all procedures
of version 1.0.0.f are pure procedures. On the other hand, all
procedures of version 1n.0.0.f are not or may not be pure procedures.
Fainumber will give support to both none pure and pure build for the
first version. 

This concept of release for the first version is to let the developer
have time to thoroughly test the performance of both, the pure and none
pure procedures to determine if there is a performance loss for having
all the procedures as pure procedures.

If the performance losses(if there is) to have all the procedures as
pure procedures is not considerable(less than 100ms per 10 million executions),
the future version of faiNumber will only support pure procedures.

For the fnConst module's fnVersionNo, both versions, 1 and 1n will have
the same version control number. However, fnPure of the same module
if `.TRUE.` is version 1 and if `.FALSE.` is version 1n. fnPure will be
removed from the second release if faiNumber's procedures are all or
mostly pure procedures.

Take note that the method of how to call an API of the pure build
version and the none pure build version may not be the same. Thus,
it is recommended to read the documentation before usage.

### Version Control Schematic

Version number: Major.Minor.Security/BugFix.Build

Build: (b: Beta), (rc-N: Release Candidate No.), (f: Final).

Take note that the official online web docs for any major release version
may always use the (n).0.0.f for that version and its minor, security/bugfix
releases.

Major releases will add new feature(s) and/or remove unnecessary API. Any
deprecated API will not be removed until at least 3 major versions after the
major version that first informed of the removal of the API. However, if it is
simply a name change, the API with the old name may just redirect the call to
a new name.

Minor releases do not alter, add, nor remove any API. Minor releases are for
coding layout improvement, code improvement, and/or code optimization.

Bugfix/Security releases do not alter, add, nor remove any API. Bugfix/Security
releases are releases that will fix discovered bug(s) or security loophole(s).

### fnVersionNo Array

[1] -> Major<br />
[2] -> Minor<br />
[3] -> Security/BugFix<br />
[4] -> Build(-1: Beta, 0: Final, >0: rc-N)<br />
