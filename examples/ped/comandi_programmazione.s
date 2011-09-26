@Chapter
@Title { Comandi per la programmazione }
@Begin @LP

Per comprende anche qualche comando che è utile principalmente per
i programmatori. Due tra questi sono i comandi per l'indentazione
ovvero @F {>>} e @F {<<}, che aggiungono e tolgono spazi dall'inizio
delle righe sulle quali vengono invocati. @PP

@d comandoShiftL
@{
def comandoShiftL( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  try:
    spaziDaGestire = int( comando.txtComando[2:].strip() )
  except ValueError, e:
    spaziDaGestire = 2

  lineeDaGestire = comando.lineaFine - comando.lineaInizio + 1
  if lineeDaGestire > 0:
    comando.ped.setCopyBuf( comando.areaLavoro.getLinee(
      comando.lineaInizio, comando.lineaFine ) )
    comando.areaLavoro.inserisciInUndo( "ShiftL" )
    for l in comando.areaLavoro.getNumeriLineeFra( comando.lineaInizio, comando.lineaFine ):
      orig = comando.areaLavoro.getLinea( l )
      if len(orig) > spaziDaGestire and orig[0:spaziDaGestire].strip()=="":
        comando.areaLavoro.setLinea( l, orig[spaziDaGestire:] )

    comando.stampaSchermo = True
@}

Questo comando è il viceversa del precedente, ovvero serve per indentare di più
una riga. @PP

@d comandoShiftR
@{
def comandoShiftR( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  try:
    spaziDaGestire = int( comando.txtComando[2:].strip() )
  except ValueError, e:
    spaziDaGestire = 2

  lineeDaGestire = comando.lineaFine - comando.lineaInizio + 1
  if lineeDaGestire > 0:
    comando.ped.setCopyBuf( comando.areaLavoro.getLinee(
      comando.lineaInizio, comando.lineaFine ) )
    comando.areaLavoro.inserisciInUndo( "ShiftL" )
    for l in comando.areaLavoro.getNumeriLineeFra( comando.lineaInizio, comando.lineaFine ):
      orig = comando.areaLavoro.getLinea( l )
      comando.areaLavoro.setLinea( l, (" " * spaziDaGestire) + orig )

    comando.stampaSchermo = True
@}

@d comandi programmazione
@{
@<comandoShiftL@>
@<comandoShiftR@>
@}
@End @Chapter
