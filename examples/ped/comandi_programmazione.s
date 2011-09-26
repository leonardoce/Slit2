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

Il comando @F ! permette invece l'esecuzione di un comando del sistema
operativo in uso.  Se il comando inizia con il carattere @F {<} allora l'output del
comando viene redirezionato e inserito in un nuovo buffer temporaneo. @PP

@d comandoSo
@{
def comandoSo( comando ):
  sComandoSo = comando.txtComando[1:].strip()
  if len( sComandoSo ) > 0:
    try:
      if sComandoSo[0] == "<":
        f = os.popen( sComandoSo[1:] )
        linee = f.readlines()
        f.close()

        area = AreaLavoro( comando.ped )
        area.setNomeBuffer( "** " + sComandoSo[1:] + " **" )
        area.inserisciDopo( 0, linee )
        area.resetModificato()

        comando.stampaSchermo = True
        comando.ped.addAreaLavoro( area )
      else:
        os.system( sComandoSo )
    except Exception, e:
      print str(e)
@}

@d comandi programmazione
@{
@<comandoShiftL@>
@<comandoShiftR@>
@<comandoSo@>
@}
@End @Chapter
