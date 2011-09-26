@Chapter
@Title { Comandi }
@Begin @LP

Ped ha molti comandi utili per la gestione dei files di testo. I comandi
sono divisi in sezioni in base a cosa fanno.

Supponendo che @F c sia un carattere e che @F n sia un numero, allora i possibili riferimenti
ad una riga hanno la sintassi seguente: @PP

@TagList
@DropTagItem { @F @Verbatim { 'c }} { Indica la linea il cui mark è il carattere indicato }
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
'[a-z]|\.|\$|\++|\-+|/(?:[^\\]|\\.)*?/|\?(?:[^\\]|\\.)*?\?|\d+
}

@d class Comando
@{
class Comando:
  def __init__(self, oPed, oAreaLavoro, sComando):
    self.ped = oPed
    self.areaLavoro = oAreaLavoro

    reLinea = "'[a-z]|\\.|\\$|\\+(?:\d*)|\\-(?:\d*)|/(?:[^\\\\]|\\\\.)*?/|\\?(?:[^\\\\]|\\\\.)*?\\?|\\d+"
    reComando = "(?P<inizio>" + reLinea + ")?(?:,(?P<fine>" + reLinea +"))?(?P<comando>.*)"
    sMatch = re.match( reComando, sComando )

    self.continua = True
    self.stampaSchermo = False

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
    elif self.txtComando[0] == "w":
      comandoW(self)
    elif self.txtComando.startswith("g/") and self.txtComando.endswith("/") and len(self.txtComando)>=3:
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
    else:
      raise ErrorePed( "Comando non valido: " + self.txtComando )

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
  comando.stampaSchermo = True

def comandoU( comando ):
  comando.areaLavoro.undo()

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

def comandoK( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = comando.areaLavoro.cursore

  comando.areaLavoro.setMark( comando.txtComando[1], comando.lineaInizio )
  comando.stampaSchermo = True

def comandoW( comando ):
  nomeFile = comando.txtComando[1:].strip()

  if nomeFile=="":
    nomeFile = comando.areaLavoro.nomeBuffer

  try:
    stato = "=> " + nomeFile
    comando.areaLavoro.salvaSu( nomeFile )
  except IOError, e:
    stato = str(e)

  comando.stampaSchermo = False
  print stato
  if comando.txtComando[1:].strip() == "":
    comando.areaLavoro.resetModificato()

  comando.areaLavoro.setNomeBuffer( nomeFile )

def comandoG( comando ):
  if comando.lineaInizio == None:
    comando.lineaInizio = 1

  if comando.lineaFine == None:
    comando.lineaFine = len( comando.areaLavoro.buffer )

  comando.stampaSchermo = False

  try:
    prog = re.compile( comando.txtComando[2:-1] )
  except Exception, e:
    print str(e)
    return

  lineaDaControllare = comando.lineaInizio

  clearScreen()
  while lineaDaControllare <= comando.lineaFine:
    if prog.search( comando.areaLavoro.getLinea(lineaDaControllare) ):
      comando.areaLavoro.stampaLinea( lineaDaControllare )
    lineaDaControllare+=1

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

  while lineaDaControllare <= comando.lineaFine:
    orig = comando.areaLavoro.getLinea(lineaDaControllare)
    dest = prog.sub( regOut, orig, numeroSost )
    if orig!=dest:
      comando.areaLavoro.setLinea( lineaDaControllare, dest )
      comando.areaLavoro.stampaLinea( lineaDaControllare )
    lineaDaControllare+=1

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

@<comandi aree di lavoro@>
@<comandi programmazione@>
@}

@End @Chapter
