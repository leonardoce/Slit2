@Chapter
@Title { Comandi }
@Begin @LP

Ped ha molti comandi utili per la gestione dei files di testo. I comandi
sono divisi in sezioni in base a cosa fanno.

Supponendo che @F c sia un carattere e che @F n sia un numero, allora i possibili riferimenti
ad una riga hanno la sintassi seguente: @PP

@TaggedList
@DropTagItem { @F @Verbatim { 'c }} { Indica la linea il cui mark è il carattere indicato.
La linea viene cercata a partire dalla linea successiva a quella corrente. }
@DropTagItem { @F @Verbatim { ^c }} { Indica la linea il cui mark è il carattere indicato.
La linea viene cercata, all'indietro, a partire dalla linea che precede quella dove è posizionato
il cursore. }
@DropTagItem { @F @Verbatim { nnnn }} { Dove c'è almeno una cifra indica un certo numero di
  linea specificato }
@DropTagItem { @F @Verbatim { . }} { Indica la linea corrente }
@DropTagItem { @F @Verbatim { $ }} { Indica l'ultima riga del buffer }
@DropTagItem { @F @Verbatim { + }} { Indica la linea successiva, in base a quanti sono
  i + indica anche le righe successive }
@DropTagItem { @F @Verbatim { - }} { Indica la linea precedente, in base a quanti sono
  i - indica anche le altre righe precedenti }
@DropTagItem { @F @Verbatim { /..../ }} { Indica una espressione regolare }
@DropTagItem { @F @Verbatim { ?....? }} { Indica una espressione regolare, ricercata
all'indietro dal cursore }
@EndList

@PP
L'espressione regolare per i mark è quindi @F @Verbatim { '[a-z] }. Quella per le
espressioni regolari è invece @F @Verbatim { /(?:[^\\]|\\.)*?/ }. Questa espressione
regolare è dovuta al fatto che le espressioni regolari al loro interno non devono
avere barre diritte @F @Verbatim { "/" }, oppure se le hanno devono essere precedute
da una barra rovescia di escape @F @Verbatim { "\" }. @PP

L'espressione regolare che indica un riferimento ad una linea è quindi la seguente: @PP

@Display @F @Verbatim {
'[0-9a-z]|\^[0-9a-z]|\.|\$|\++|\-+|/(?:[^\\]|\\.)*?/|\?(?:[^\\]|\\.)*?\?|\d+
}

@d class Comando
@{
class Comando:
  def __init__(self, oPed, oAreaLavoro, sComando):
    self.ped = oPed
    self.areaLavoro = oAreaLavoro

    reLinea = "'[0-9a-z]|\\^[0-9a-z]|\\.|\\$|\\+(?:\d*)|\\-(?:\d*)|/(?:[^\\\\]|\\\\.)*?/|\\?(?:[^\\\\]|\\\\.)*?\\?|\\d+"
    reComando = "(?P<inizio>" + reLinea + ")?(?:,(?P<fine>" + reLinea +"))?(?P<comando>.*)"
    sMatch = re.match( reComando, sComando )

    self.continua = True
    self.stampaSchermo = False
    self.conservaInHistory = True

    if sMatch != None:
      dMatch = sMatch.groupdict()
      self.txtInizio = dMatch['inizio']
      self.txtFine = dMatch['fine']
      self.lineaInizio = self.risolviIndirizzo( self.txtInizio )
      self.lineaFine = self.risolviIndirizzo( self.txtFine )
      self.txtComando = dMatch['comando'].strip()
    else:
      raise ErrorePed( "Sintassi errata: " + sComando )

  def risolviIndirizzo(self, sIndirizzo):
    if sIndirizzo == None:
      return None
    else:
      return self.areaLavoro.risolviIndirizzo( sIndirizzo )

  def stampa(self):
    print "Inizio: (", self.lineaInizio, ") ", self.txtInizio
    print "Fine: (", self.lineaFine, ") ", self.txtFine
    print "Comando: ", self.txtComando

  def esegui(self):
    if self.txtComando == "q" or self.txtComando == "qq":
      comandoQ(self)
    elif self.txtComando == "p" or self.txtComando=="":
      comandoP(self)
    elif self.txtComando == "z":
      comandoZ(self)
    elif self.txtComando == "u":
      comandoU(self)
    elif self.txtComando == "i":
      comandoI(self)
    elif self.txtComando == "a":
      comandoA(self)
    elif self.txtComando == "d":
      comandoD(self)
    elif re.match( "k.", self.txtComando):
      comandoK(self)
    elif self.txtComando == "r":
      comandoR(self)
    elif self.txtComando[0] == "w":
      comandoW(self)
    elif self.txtComando.startswith("g/"):
      comandoG(self)
    elif re.match( "s/((?:[^/]|\\\\.)*?)/((?:[^/]|\\\\.)*?)/(\\d*)", self.txtComando):
      comandoS(self)
    elif self.txtComando == "c":
      comandoC(self)
    elif self.txtComando == "y":
      comandoY(self)
    elif self.txtComando == "bi":
      comandoBI(self)
    elif self.txtComando == "ba":
      comandoBA(self)
    elif self.txtComando.startswith("put "):
      comandoPut( self )
    elif self.txtComando.startswith("buf"):
      comandoBuf( self )
    elif self.txtComando.startswith("e "):
      comandoE( self )
    elif self.txtComando.startswith("<<"):
      comandoShiftL( self )
    elif self.txtComando.startswith(">>"):
      comandoShiftR( self )
    elif self.txtComando.startswith("!"):
      comandoSo( self )
    elif self.txtComando.startswith("h"):
      comandoH( self )
    else:
      comandoMacro( self )

def comandoP( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore
  elif not comando.areaLavoro.isLineaValida( comando.lineaInizio ):
    print "Linea non valida"
  else:
    comando.areaLavoro.setCursore( comando.lineaInizio )
  comando.stampaSchermo = True

def comandoZ( comando ):
  nuovoCursore = comando.areaLavoro.cursore + comando.ped.getDimensioneFinestra() + 1
  if comando.areaLavoro.isLineaValida( nuovoCursore ):
    comando.areaLavoro.setCursore( nuovoCursore)
  else:
    comando.areaLavoro.setCursore( comando.areaLavoro.risolviIndirizzo( "$" ) )
  comando.stampaSchermo = True

def comandoU( comando ):
  comando.areaLavoro.undo()

def comandoC( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  lineeDaCancellare = comando.lineaFine - comando.lineaInizio + 1

  comando.areaLavoro.inserisciInUndo( "Change" )
  comando.areaLavoro.cancellaLinee( comando.lineaInizio, lineeDaCancellare )
  linee = leggiRigheDaUtente()
  comando.areaLavoro.inserisciDopo( comando.lineaInizio-1, linee )

def comandoG( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = 1

  if comando.lineaFine == None:
    comando.lineaFine = len( comando.areaLavoro.buffer )

  match = re.match( "g/((?:[^/]|\\\\.)*?)/(.*)$", comando.txtComando)
  if match == None:
    print "Sintassi errata."
    return

  gruppi = match.groups()
  regEx = gruppi[0]
  sComandoDaEseguire = gruppi[1]

  try:
    prog = re.compile( regEx )
  except Exception, e:
    print str(e)
    return

  for lineaDaControllare in comando.areaLavoro.getNumeriLineeFra( comando.lineaInizio, comando.lineaFine ):
    if prog.search( comando.areaLavoro.getLinea(lineaDaControllare) ):
      if sComandoDaEseguire == None or len( sComandoDaEseguire.strip() ) == 0:
        comando.areaLavoro.stampaLinea( lineaDaControllare )
      else:
        sComandoConLinea = str( lineaDaControllare ) + "k/"
        comando.ped.eseguiComando( sComandoConLinea )

  while comando.areaLavoro.hasMark( '/' ):
    linea = comando.areaLavoro.getMark( '/' )
    sComandoConLinea = str( linea ) + sComandoDaEseguire
    comando.ped.eseguiComando( str( linea ) + "k0" )
    comando.ped.eseguiComando( sComandoConLinea )

def comandoS( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  gruppi = re.match( "s/((?:[^/]|\\\\.)*?)/((?:[^/]|\\\\.)*?)/(\\d*)", comando.txtComando).groups()
  if gruppi[2] == "":
    numeroSost = 1
  else:
    numeroSost = int(gruppi[2])

  regIn = gruppi[0]
  regOut = gruppi[1]

  try:
    prog = re.compile( regIn )
  except Exception, e:
    print str(e)
    return

  lineaDaControllare = comando.lineaInizio

  comando.areaLavoro.inserisciInUndo( "Sostituzione" )

  try:
    while lineaDaControllare <= comando.lineaFine:
      orig = comando.areaLavoro.getLinea(lineaDaControllare)
      dest = prog.sub( regOut, orig, numeroSost )
      if orig!=dest:
        comando.areaLavoro.setLinea( lineaDaControllare, dest )
        comando.areaLavoro.stampaLinea( lineaDaControllare )
      lineaDaControllare+=1
  except Exception, e:
    print str(e)

def comandoPut( comando ):
  comando.stampaSchermo = False

  if comando.txtComando[4:].strip() == "":
    print "Sintassi: put <nomefile>"
    return

  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  lineeDaScrivere = comando.lineaFine - comando.lineaInizio + 1
  nomeFile = comando.txtComando[4:].strip()

  if lineeDaScrivere > 0:
    try:
      f = open( nomeFile, "w" )
      f.writelines( comando.areaLavoro.getLinee(comando.lineaInizio, comando.lineaFine) )
      f.close()
    except IOError, e:
      print str(e)

@<comandi killring@>
@<comandi inserimento@>
@<comandi aree di lavoro@>
@<comandi marks@>
@<comandi programmazione@>
@<comandoMacro@>
@}

@BeginSections

@Section
@Title { Comandi per l'inserimento di contenuti }
@Begin @PP

Il comando A inserisce del testo dopo la linea specificata.

@d comandoA
@{
def comandoA( comando ):
  if comando.lineaInizio != None:
    if not comando.areaLavoro.isLineaValida( comando.lineaInizio ):
      print "Linea non valida"
      return
    comando.areaLavoro.setCursore( comando.lineaInizio )
  else:
    comando.lineaInizio = comando.areaLavoro.cursore

  linee = leggiRigheDaUtente()
  comando.areaLavoro.inserisciInUndo( "Append" )
  comando.areaLavoro.inserisciDopo( comando.lineaInizio, linee )
  comando.areaLavoro.setCursore( comando.lineaInizio + len(linee) )
@}

Il comando I inserisce il testo prima della linea specificata.

@d comandoI
@{
def comandoI( comando ):
  if comando.lineaInizio != None:
    if not comando.areaLavoro.isLineaValida( comando.lineaInizio ):
      print "Linea non valida"
      return
    comando.areaLavoro.setCursore( comando.lineaInizio )
  else:
    comando.lineaInizio = comando.areaLavoro.cursore

  if not comando.areaLavoro.isLineaValida( comando.areaLavoro.cursore ):
    print "Cursore non valido"
    comando.stampaSchermo = False
    return

  linee = leggiRigheDaUtente()
  comando.areaLavoro.inserisciInUndo( "Inserimento" )
  comando.areaLavoro.inserisciDopo( comando.lineaInizio-1, linee )
  comando.areaLavoro.setCursore( comando.lineaInizio + len(linee) )
@}

@d comandi inserimento
@{
@<comandoA@>
@<comandoI@>
@}

@End @Section

@Section
@Title { Comandi di cancellazione e copia e incolla }
@Begin @PP

Ped mantiene un buffer che viene popolato esplicitamente
(comando Y) oppure con le cancellazioni (comando D). @PP

Il comando Y inserisce le righe specificate all'interno del buffer.

@d comandoY
@{
def comandoY( comando ):
  if comando.lineaInizio == None:
    if comando.areaLavoro.hasMark( 'a' ) and comando.areaLavoro.hasMark( 'b' ):
      comando.lineaInizio = comando.areaLavoro.getMark( 'a' )
      comando.lineaFine = comando.areaLavoro.getMark( 'b' )
    else:
      comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  if comando.lineaFine >= comando.lineaInizio:
    l = comando.areaLavoro.getLinee( comando.lineaInizio, comando.lineaFine )
    comando.ped.setCopyBuf( l )
@}

Il comando D invece popola il buffer con le righe selezionate, che
vengono rimosse dal file editato.

@d comandoD
@{
def comandoD( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  lineeDaCancellare = comando.lineaFine - comando.lineaInizio + 1

  if lineeDaCancellare > 0:
    comando.ped.setCopyBuf( comando.areaLavoro.getLinee(
      comando.lineaInizio, comando.lineaFine ) )
    comando.areaLavoro.inserisciInUndo( "Delete" )
    comando.areaLavoro.cancellaLinee( comando.lineaInizio, lineeDaCancellare )

  comando.stampaSchermo = True
@}

Il comando BA inserisce il contenuto del buffer dopo la riga specificata, alla
maniera del comando A.

@d comandoBA
@{
def comandoBA( comando ):
  if comando.lineaInizio != None:
    if not comando.areaLavoro.isLineaValida( comando.lineaInizio ):
      print "Linea non valida"
      return
    comando.areaLavoro.setCursore( comando.lineaInizio )
  else:
    comando.lineaInizio = comando.areaLavoro.cursore

  if not comando.areaLavoro.isLineaValida( comando.areaLavoro.cursore ):
    print "Cursore non valido"
    comando.stampaSchermo = False
    return

  comando.areaLavoro.inserisciInUndo( "Block Append" )
  linee = comando.ped.getCopyBuf()
  comando.areaLavoro.inserisciDopo( comando.lineaInizio, linee )
  comando.areaLavoro.setCursore( comando.lineaInizio + len(linee) )
  comando.stampaSchermo = True
@}

Il comando BI inserisce il contenuto del buffer prima della riga
specificata.

@d comandoBI
@{
def comandoBI( comando ):
  if comando.lineaInizio != None:
    if not comando.areaLavoro.isLineaValida( comando.lineaInizio ):
      print "Linea non valida"
      return
    comando.areaLavoro.setCursore( comando.lineaInizio )
  else:
    comando.lineaInizio = comando.areaLavoro.cursore

  if not comando.areaLavoro.isLineaValida( comando.areaLavoro.cursore ):
    print "Cursore non valido"
    comando.stampaSchermo = False
    return

  comando.areaLavoro.inserisciInUndo( "Block Insert" )
  linee = comando.ped.getCopyBuf()
  comando.areaLavoro.inserisciDopo( comando.lineaInizio-1, linee )
  comando.areaLavoro.setCursore( comando.lineaInizio + len(linee) )
  comando.stampaSchermo = True
@}

@d comandi killring
@{
@<comandoY@>
@<comandoD@>
@<comandoBA@>
@<comandoBI@>
@}

@End @Section

@Section
@Title { Comandi per la gestione dei marks }
@Begin @PP

Ped si conserva, all'interno dell'area di lavoro, un flag di un carattere per
ogni riga del buffer. Questo flag può essere utilizzato per muoversi all'interno del buffer
oppure per indicare genericamente una riga. @PP

La riga successiva con un certo flag è indicata da un apostrofo e dal flag. La riga
precedente viene indicata in modo simile attraverso il carattere "^". @PP

Per impostare un flag su un insieme di righe si utilizza il comando K.

@d comandoK
@{
def comandoK( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  for l in comando.areaLavoro.getNumeriLineeFra( comando.lineaInizio, comando.lineaFine ):
    comando.areaLavoro.setMark( comando.txtComando[1], l )
@}

Per rimuovere i flag impostati su un insieme di righe si utilizza il comando
R.

@d comandoR
@{
def comandoR( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  if comando.lineaFine == None:
    comando.lineaFine = comando.lineaInizio

  comando.areaLavoro.cancellaMarksFra( comando.lineaInizio, comando.lineaFine )
@}

I comandi che lavorano sui marks sono quindi:

@d comandi marks
@{
@<comandoK@>
@<comandoR@>
@}

@End @Section

@EndSections

@End @Chapter
