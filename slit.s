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
the code can be separed in pieces that compose the complete software too. To have other information to this modality of work we can consult the page 
@PP




@F { @Verbatim {http://en.wikipedia.org/wiki/Literate_programming} }

@End @Introduction

@i guida.s
@i magazzino.s
@i statocorrente.s
@i ingresso.s
@i sourceparser.s
@i drivermagazzino.s
@i drivergenerazionedoc.s
@i outputdocumentazione.s
@i outputsorgenti.s
@i utilita.s
