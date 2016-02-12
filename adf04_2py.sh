#ompile and link test Fortran program
#
f2py3 -c -m  adf04_2py   \
xxdata04_string.for \
i4unit.for \
xxword.for \
xxslen.for \
xxprs1.for \
r8fctn.for \
i4fctn.for \
i4idfl.for \
xxpars.for \
xxrmve.for \
xxcase.for

