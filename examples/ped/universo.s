@Chapter
@Title { Oggetto Ped }
@Begin @LP

L'oggetto Ped è il contenitore di tutti gli altri oggetti dell'editor. Esso
infatti contiene le proprietà generali dell'editor e una lista di aree di
lavoro possibili. @PP

Per selezionare l'area di lavoro corrente si utilizza il comando @F {buf},
che è commentato nel capitolo relativo ai comandi. @PP

Il programma è pensato per avere sempre una area di lavoro: per questo motivo
alla rimozione di una area di lavoro ci si assicura di crearne un'altra se
le aree di lavoro sono finite. @PP

@d class Ped
@{
class Ped:
  def __init__(self):
    self.areeLavoro = [ AreaLavoro( self ) ]
    self.copybuf = []
    self.dimensioneFinestra = 20
    self.dimensioneHistory = 20
    self.areaCorrente = 1
    self.history = []

  @<class Ped, metodi aree lavoro@>
  @<class Ped, buffer@>
  @<class Ped, configurazione@>
  @<class Ped, comandi@>
@}

L'oggetto Ped ha un insieme di metodi che servono per gestire
il contenitore delle aree di lavoro. Questi metodi vengono chiamati
dai vari comandi dell'editor:

@d class Ped, metodi aree lavoro
@{
def getAreeLavoro(self):
  return self.areeLavoro

def deleteAreaLavoro(self, area):
  self.areeLavoro.remove( area )
  if len( self.areeLavoro ) == 0:
    self.areeLavoro = [ AreaLavoro(self) ]
  self.areaCorrente = 1

def addAreaLavoro(self, area):
  self.areeLavoro.append( area )
  self.areaCorrente = len( self.areeLavoro )

def getAreaLavoroCorrente(self):
  return self.areeLavoro[ self.areaCorrente - 1 ]

def setAreaCorrente(self, nArea):
  self.areaCorrente = nArea

@}

Ci sono anche dei metodi che servono per gestire il buffer delle
operazioni di copia e incolla:

@d class Ped, buffer
@{
def setCopyBuf(self, linee):
  self.copybuf = linee

def getCopyBuf(self):
  return self.copybuf
@}

I metodi che seguono servono invece per le informazioni
di configurazione dell'editor:

@d class Ped, configurazione
@{
def getDimensioneFinestra(self):
  return self.dimensioneFinestra

def setDimensioneFinestra(self, d):
  self.dimensioneFinestra = d
@}

I comandi vengono eseguiti direttamente sull'area di lavoro corrente
e ogni comando eseguito viene inserito in buffer per proporre la
storia dei comandi che l'utente ha dato nella sessione.

@d class Ped, comandi
@{
def eseguiComando( self, sComando ):
  cmd = Comando( self, self.getAreaLavoroCorrente(), sComando )
  cmd.esegui()

  if cmd.conservaInHistory:
    self.history = [ sComando ] + self.history
    if len( self.history ) > self.dimensioneHistory:
      self.history = self.history[:self.dimensioneHistory]

  return cmd

def getStoriaComandi( self ):
  return self.history
@}

@End @Chapter
