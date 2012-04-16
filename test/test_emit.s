@# -*- mode:text -*-
This is a little test!

@d my macro
@{
bla
bla
bla
blo
@}

I will emit this macro another time:

@e my macro

And another time:

@e my macro

This macro will also be written to a file

@o myfile.c
@{
@<my macro@>
@}


