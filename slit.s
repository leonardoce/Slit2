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

Slit {@Char egrave} un programma che permette di scrivere codice e documentazione all'interno dello stesso
file. Il rapporto fra il codice e la documentazione {@Char egrave} per{@Char ograve} ribaltato: se normalmente si
scrive il codice e all'interno di questo si inserisce la documentazione usando Slit si
scrive la documentazione e all'interno di questa il codice. @PP

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
