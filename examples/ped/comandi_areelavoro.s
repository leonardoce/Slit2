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

      print markAttivo, i, "-", a.getNomeBuffer()
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
    print "Sintassi: o <nomefile>"
    return

  oNuovaArea = AreaLavoro( comando.ped )
  try:
    oNuovaArea.leggiFile( sNomeFile )
  except IOError, e:
    comando.stampaSchermo = False
    print str(e)
    return

  comando.ped.addAreaLavoro( oNuovaArea )
  comando.stampaSchermo = True
@}

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
@}

@d comandi aree di lavoro
@{
@<comandoBuf@>
@<comandoE@>
@<comandoQ@>
@}

@End @Chapter
