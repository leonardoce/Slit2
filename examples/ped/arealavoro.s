@Chapter
@Title { Area di lavoro }
@Begin @LP

L'area di lavoro √® l'entit√† fondamentale sulla quale i comandi di Ped lavorano. Essa √® composta
principalmente da un buffer di righe che pu√≤ essere manipolato attraverso i metodi della classe.
Le modifiche vengono tenute in una apposita area che permette di annullarle quando desiderato.
@PP

Inoltre a questo l'area di lavoro dispone di un cursore che punta sempre ad una riga all'interno
del buffer: questa riga √® raggiungibile con l'indirizzo @F @Verbatim { "." }. @PP

Il buffer di righe puï corrispondere ad un file oppure puï essere un buffer
@I {dummy}. Un buffer dummy non ä stato creato leggendo un file ma ä presente
solamente nella memoria dell'editor. I buffer temporanei possono essere comunque
salvati su files concreti. @PP

@d class AreaLavoro
@{
class AreaLavoro:
  def __init__(self, oPed):
    self.ped = oPed
    self.buffer = []
    self.bufferUndoRing = []
    self.cursore = 0
    self.nomeBuffer = "** scratch **"
    self.rmarks = {}
    self.modificato = False
    self.temporaneo = True

  @<class AreaLavoro, metodi@>
@}

Gli attributi della classe possono essere letti e scritti attraverso gli appositi metodi getter e
setter:

@d class AreaLavoro, getter e setter
@{
def resetModificato( self ):
  self.modificato = False

def isModificato( self ):
  return self.modificato

def setNomeBuffer(self, sNomeFile):
  self.nomeBuffer = sNomeFile

def getNomeBuffer(self):
  return self.nomeBuffer

def isTemporaneo( self ):
  return self.temporaneo

def setTemporaneo( self, flag ):
  self.temporaneo = flag

def getBuffer( self ):
  return self.buffer
@}

Il caricamento e lo scaricamento di un file viene direttamente effettuato
da metodi dell'area di lavoro. Oltre al riempimento del buffer questi
metodi impostano i flag dell'area di lavoro in modo appropriato. @PP

@d class AreaLavoro, gestione files
@{
def leggiFile(self, sNomeFile):
  f = open( sNomeFile )
  self.buffer = f.readlines()
  f.close()

  self.nomeBuffer = sNomeFile
  self.modificato = False
  self.temporaneo = False

def salvaSu( self, nome ):
  f = open( nome, "w" )
  for l in self.buffer:
    print >>f, l.rstrip()
  f.close()
  self.temporaneo = False
@}

Le linee dell'area di lavoro possono essere marcate con indicatori di un carattere
alfabetico oppure con un carattere numerico.
Gli indicatori possono indicare anche piu' linee contemporaneamente e l'indicatore "0"
rimuove ogni altro indicatore.
@PP

Il codice dell'area di lavoro non assume che gli indicatori siano sempre
in questo range di caratteri accettati. Questo permette ai comandi e alle
macro di inserire indicatori con caratteri diversi per "marcare" delle linee
in modo temporaneo. @PP


@d class AreaLavoro, marks
@{
def hasMark( self, nome ):
  if nome == '0':
    return False
  else:
    for l in self.rmarks:
      if self.rmarks[ l ] == nome:
        return True
    return False

def getMark( self, nome ):
  if nome == '0':
    return 0
  else:
    for l in self.rmarks:
     if self.rmarks[ l ] == nome:
       return l
    return 0

def setMark( self, nome, linea ):
  if nome == '0':
    if linea in self.rmarks:
      del self.rmarks[ linea ]
  else:
    if linea in self.rmarks:
      prec = self.rmarks[ linea ]
    self.rmarks[ linea ] = nome

def aggiornaMarksDopo( self, inizio, offset ):
  cleanRmarks = {}

  for l in self.rmarks:
    if l>=inizio:
      name = self.rmarks[l]
      cleanRmarks[ l + offset ] = self.rmarks[l]
    else:
      cleanRmarks[ l ] = self.rmarks[ l ]

  self.rmarks = cleanRmarks

def cancellaMarksFra( self, inizio, fine ):
  cleanRmarks = {}

  for l in self.rmarks:
   if inizio <= l and l <= fine:
     pass
   else:
     cleanRmarks[ l ] = self.rmarks[ l ]

  self.rmarks = cleanRmarks
@}

@d class AreaLavoro, metodi
@{
@<class AreaLavoro, gestione files@>
@<class AreaLavoro, getter e setter@>
@<class AreaLavoro, marks@>

def risolviIndirizzo(self, sIndirizzo):
  if re.match( "\\d+", sIndirizzo ):
    return int( sIndirizzo )

  elif re.match( "\\'[a-z0-9]", sIndirizzo ):
    for l in self.rmarks:
      if self.rmarks[l] == sIndirizzo[1]:
        return l
    raise ErrorePed( "mark non trovato" )

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

def cancellaLinee( self, inizio, linee ):
  self.cancellaMarksFra( inizio, inizio + linee - 1 )
  self.aggiornaMarksDopo( inizio, -1 * linee )
  self.buffer=self.buffer[:inizio - 1] + self.buffer[inizio + linee - 1:]
  self.modificato = True

def inserisciDopo( self, inizio, contenuti ):
  self.aggiornaMarksDopo( inizio, len(contenuti) )
  self.buffer = self.buffer[:inizio] + contenuti + self.buffer[inizio:]
  self.modificato = True

def undo(self):
  if self.bufferUndoRing != []:
    self.buffer = self.bufferUndoRing[-1]['buffer']
    self.bufferUndoRing = self.bufferUndoRing[:-1]

  self.modificato = True
@}

@End @Chapter
