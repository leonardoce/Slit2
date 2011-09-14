@x output_txt
ciao ciao

@d macro
@{
linea uno
linua due
@}

ciao

@+ macro
@{
linea tre
linua quattro
@}


@o output.src
@{
dovrebbero essere quattro righe

@<macro@>
@}
