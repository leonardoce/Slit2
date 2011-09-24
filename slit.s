# :folding=explicit:mode=slitpascal:
@x output_lout
@SysInclude { tbl }
@Include { book }

@Book 
  @Title { Slit - Simple Literate Tool }
  @Author { Leonardo Cecchi }
//

@Introduction
@Title { Introduzione }
@Begin @PP

Slit is a software which can write code and documentation into the same file.
The relation between code and documentation is inverted: if usually we write the code and into this we put the documentation
using Slit we write the documentation and into that the code. @PP

Inoltre a questo il codice può essere diviso in frammenti che si richiamano per comporre
l'intero programma. Per avere maggiori informazioni su questa modalità di lavoro
si può consultare la pagina: @PP

@F { @Verbatim {http://en.wikipedia.org/wiki/Literate_programming} }

@End @Introduction

@i guida.s
@i magazzino.s
@i statocorrente.s
@i ingresso.s
@i drivermagazzino.s
@i drivergenerazionedoc.s
@i outputdocumentazione.s
@i outputsorgenti.s
@i utilita.s
