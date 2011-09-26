@Chapter
@Title { Area di lavoro }
@Begin @LP

L'area di lavoro è l'entità fondamentale sulla quale i comandi di Ped lavorano. Essa è composta
principalmente da un buffer di righe che può essere manipolato attraverso i metodi della classe.
Le modifiche vengono tenute in una apposita area che permette di annullarle quando desiderato.
@PP

Inoltre a questo l'area di lavoro dispone di un cursore che punta sempre ad una riga all'interno
del buffer: questa riga è raggiungibile con l'indirizzo @F @Verbatim { "." }. @PP

@d class AreaLavoro
@{
class AreaLavoro:
  def __init__(self, oPed):
    self.ped = oPed
    self.buffer = []
    self.bufferUndoRing = []
    self.cursore = 0
    self.nomeBuffer = ""
    self.marks = {}
    self.rmarks = {}
    self.modificato = False

  def leggiFile(self, sNomeFile):
    f = open( sNomeFile )
    self.buffer = f.readlines()
    f.close()

    self.nomeBuffer = sNomeFile
    self.modificato = False

  def setNomeBuffer(self, sNomeFile):
    self.nomeBuffer = sNomeFile

  def getNomeBuffer(self):
    return self.nomeBuffer

  def risolviIndirizzo(self, sIndirizzo):
    if re.match( "\\d+", sIndirizzo ):
      return int( sIndirizzo )

    elif re.match( "\\'[a-z]", sIndirizzo ) and ( sIndirizzo[1] in self.marks ):
      return self.marks[ sIndirizzo[1] ]

    elif sIndirizzo == ".":
      return self.cursore

    elif sIndirizzo == "$":
      return len( self.buffer )

    elif sIndirizzo[0] == '/' and sIndirizzo[-1] == '/':
      prog = re.compile( sIndirizzo[1:-1] )
      # Ricerca in avanti
      lineaInizio = self.cursore + 1
      lineaFine = len( self.buffer )
      lineaDaControllare = lineaInizio

      while lineaDaControllare <= lineaFine:
        l = self.getLinea( lineaDaControllare )
        if prog.search(l):
          return lineaDaControllare
        lineaDaControllare+=1

      raise ErrorePed( "Pattern non trovato: " + sIndirizzo )

    elif sIndirizzo[0] == '?' and sIndirizzo[-1] == '?':
      prog = re.compile( sIndirizzo[1:-1] )
      # Ricerca in indietro
      lineaInizio = 1
      lineaFine = self.cursore - 1
      lineaDaControllare = lineaInizio

      while lineaDaControllare <= lineaFine:
        l = self.getLinea( lineaDaControllare )
        if prog.search(l):
          return lineaDaControllare
        lineaDaControllare+=1

      raise ErrorePed( "Pattern non trovato: " + sIndirizzo )

    elif sIndirizzo.startswith("+"):
      if len(sIndirizzo)>1:
        offset = int(sIndirizzo[1:])
      else:
        offset = 1

      linea = self.cursore + offset
      if not self.isLineaValida( linea ):
        linea = len(buffer)
      return linea

    elif sIndirizzo.startswith("-"):
      if len(sIndirizzo)>1:
        offset = int(sIndirizzo[1:])
      else:
        offset = 1

      linea = self.cursore - offset
      if not self.isLineaValida( linea ):
        linea = 1
      return linea

    else:
      raise ErrorePed( "Indirizzo di linea non valido: " + sIndirizzo )

  def isLineaValida( self, i ):
    i = i-1
    if i>=0 and i<len(self.buffer):
      return True
    else:
      return False

  def getLinea( self, i ):
    if self.isLineaValida(i):
      return self.buffer[i-1].rstrip()
    else:
      return ""

  def getLinee( self, inizio, fine ):
    if self.isLineaValida(inizio) and self.isLineaValida(fine):
      return self.buffer[inizio-1:fine]
    else:
      return []

  def setLinea( self, i, txt ):
    if self.isLineaValida(i):
      self.buffer[i-1] = txt
    self.modificato = True

  def getNumeriLineeFra( self, inizio, fine ):
    if fine>=inizio:
      for l in xrange(inizio, fine+1):
        if self.isLineaValida( l ):
          yield l

  def stampaSchermo( self ):
    clearScreen()

    if self.modificato:
      modMark = "<Mod>"
    else:
      modMark = "     "

    print self.nomeBuffer, modMark
    print "--"

    lineaInizio = self.cursore - self.ped.getDimensioneFinestra()/2
    if not self.isLineaValida(lineaInizio):
      lineaInizio = 1

    lineaFine = lineaInizio + self.ped.getDimensioneFinestra()
    if not self.isLineaValida( lineaFine ):
      lineaFine = len( self.buffer )

    if (lineaFine - lineaInizio) < self.ped.getDimensioneFinestra():
      if lineaInizio == 1:
        lineaFine = self.ped.getDimensioneFinestra()

      if lineaFine == len( self.buffer ):
        lineaInizio = len( self.buffer ) - self.ped.getDimensioneFinestra()

    for lineaDaStampare in self.getNumeriLineeFra( lineaInizio, lineaFine ):
      self.stampaLinea( lineaDaStampare )

    if self.isLineaValida( self.cursore ):
      print
      print self.getLinea( self.cursore )

  def stampaLinea( self, lineaDaStampare ):
    if self.isLineaValida( lineaDaStampare ):
      if lineaDaStampare in self.rmarks:
        mark = self.rmarks[lineaDaStampare]
      else:
        mark = ' '

      if lineaDaStampare == self.cursore:
        print mark+"<%5d> %s" % ( lineaDaStampare, self.getLinea( lineaDaStampare ) )
      else:
        print mark+" %5d  %s" % ( lineaDaStampare, self.getLinea( lineaDaStampare ) )

  def setCursore( self, i ):
    if self.isLineaValida( i ):
      self.cursore = i

  def inserisciInUndo( self, descr ):
    self.bufferUndoRing.append( {'descr': descr, 'buffer':self.buffer[:] } )

  def aggiungiRighe( self, n ):
    for i in xrange( n ):
      self.buffer.append("")
    self.modificato = True

  def cancellaMarksDopo( self, inizio ):
    cleanRmarks = []
    cleanMarks = []

    for l in self.rmarks:
      if l>=inizio:
        name = self.rmarks[l]
        cleanRmarks.append(l)
        cleanMarks.append(name)

    for l in cleanMarks: del self.marks[l]
    for l in cleanRmarks: del self.rmarks[l]

  def cancellaLinee( self, inizio, linee ):
    self.cancellaMarksDopo( inizio )
    self.buffer=self.buffer[:inizio - 1] + self.buffer[inizio + linee - 1:]
    self.modificato = True

  def inserisciDopo( self, inizio, contenuti ):
    self.cancellaMarksDopo( inizio )
    self.buffer = self.buffer[:inizio] + contenuti + self.buffer[inizio:]
    self.modificato = True

  def hasMark( self, nome ):
    return nome in self.marks

  def getMark( self, nome ):
    if nome in self.marks:
      return self.marks[nome]
    else:
      return 0

  def setMark( self, nome, linea ):
    if nome in self.marks:
      v = self.marks[nome]
      del self.marks[nome]
      del self.rmarks[v]

    self.marks[ nome ] = linea
    self.rmarks[ linea ] = nome

  def salvaSu( self, nome ):
    f = open( nome, "w" )
    for l in self.buffer:
      print >>f, l.rstrip()
    f.close()

  def undo(self):
    if self.bufferUndoRing != []:
      self.buffer = self.bufferUndoRing[-1]['buffer']
      self.bufferUndoRing = self.bufferUndoRing[:-1]

    self.modificato = True

  def resetModificato( self ):
    self.modificato = False

  def isModificato( self ):
    return self.modificato

  @<class AreaLavoro, comandi@>
@}

L'area di lavoro gestisce anche l'esecuzione dei comandi:

@d class AreaLavoro, comandi
@{
def eseguiComando( self, sComando ):
  cmd = Comando( self.ped, self, sComando )
  cmd.esegui()
  return cmd
@}

@End @Chapter
