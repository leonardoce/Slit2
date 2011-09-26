@Chapter
@Title { Comandi per le aree di lavoro }
@Begin @LP

Questi comandi servono per la gestione delle aree di lavoro. Il primo comando, ovvero
@F {buf}, permette di visualizzare le aree di lavoro presenti oppure di cambiare
area di lavoro corrente:

@d comandoBuf
@{
def comandoBuf(comando):
  if len( comando.txtComando.strip() ) > 4:
    sArea = comando.txtComando.strip()[4:]
  else:
    sArea = ""

  comando.stampaSchermo = False
  if sArea == "":
    aree = comando.ped.getAreeLavoro()
    i = 1
    for a in aree:
      if a == comando.areaLavoro:
        markAttivo = "> "
      else:
        markAttivo = "  "

      if a.isModificato():
        modificato = "<Mod>"
      else:
        modificato = "     "

      print markAttivo, i, "-", a.getNomeBuffer(), modificato
      i += 1
  else:
    try:
      nArea = int( sArea )
      if nArea>=1 and nArea<=len( comando.ped.getAreeLavoro() ):
        comando.ped.setAreaCorrente( nArea )
        comando.stampaSchermo = True
      else:
        print "Numero area non valido"
    except ValueError, e:
      print "Numero area non valido"
@}

Il comando @F {e} serve per aprire un nuovo file in una nuova area di lavoro:

@d comandoE
@{
def comandoE(comando):
  sNomeFile = comando.txtComando[2:].strip()
  if len( sNomeFile ) == 0:
    comando.stampaSchermo = False
    print "Sintassi: e <nomefile>"
    return

  idxAreaLavoro = 0
  for area in comando.ped.getAreeLavoro():
    idxAreaLavoro = idxAreaLavoro + 1
    if os.path.realpath( area.getNomeBuffer() ) == os.path.realpath( sNomeFile ):
      comando.stampaSchermo = True
      comando.ped.setAreaCorrente( idxAreaLavoro )
      return

  oNuovaArea = AreaLavoro( comando.ped )
  if os.path.exists( sNomeFile ):
    try:
      oNuovaArea.leggiFile( sNomeFile )
      comando.stampaSchermo = True
    except IOError, e:
      comando.stampaSchermo = False
      print str(e)
      return
  else:
    oNuovaArea.setNomeBuffer( sNomeFile )
    comando.stampaSchermo = False
    print "New file"

  comando.ped.addAreaLavoro( oNuovaArea )
@}

Il comando @F q permette invece di chiudere il buffer corrente se non
modificato. Se Š modificato il comando deve essere rafforzato con
la versione @F {qq}. Quando tutti i buffer dell'editor sono stati
esauriti l'editor viene terminato. @PP

@d comandoQ
@{
def comandoQ( comando ):
  if comando.areaLavoro.isModificato() and comando.txtComando != "qq":
    print "Buffer modificato. Utilizza 'qq'"
    comando.stampaSchermo = False
  else:
    if len( comando.ped.getAreeLavoro() ) == 1:
      comando.continua = False
    else:
      comando.ped.deleteAreaLavoro( comando.areaLavoro )
      comando.stampaSchermo = True
@}


Il comando @F h permette di visualizzare la storia dei comandi
che l'utente ha dato all'editor. Ogni comando pu• essere richiamato
facendo seguire alla @F h il numero del comando. Il comando @F h non rimane nella storia
dei comandi.

@d comandoH
@{
def comandoH( comando ):
  comando.conservaInHistory = False
  if len( comando.txtComando[1:].strip() ) == 0:
    storia = comando.ped.getStoriaComandi()
    i = 0
    for sComando in storia:
      i = i+1
      print i, sComando
  else:
    try:
      iNumeroComando = int( comando.txtComando[1:].strip() )
    except ValueError, e:
      print "Non valido"

    risultato = comando.ped.eseguiComando( comando.ped.getStoriaComandi()[ iNumeroComando - 1 ] )
    comando.stampaSchermo = risultato.stampaSchermo
@}

@d comandi aree di lavoro
@{
@<comandoBuf@>
@<comandoE@>
@<comandoQ@>
@<comandoH@>
@}

@End @Chapter
