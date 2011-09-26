@Chapter
@Title { Macro di utilizzo generico }
@Begin @LP

Queste macro non sono pensate per un uso particolare e sono quindi di
ausilio alla globalità degli utenti. @PP

@BeginSections

@Section
@Title { Editing di una singola linea }
@Begin @PP

Questa macro permette di modificare i contenuti di una singola
linea. Viene implementata una modalità di editing "diversa" da
quella di Ped dove i comandi hanno un significato differente. La
linea da editare viene presa direttamente dal numero inserito prima del comando
e se non viene specificata si assume che si indenta editare la linea sulla quale il
cursore è puntato. @PP

@o le.py
@{
import re

@<le, comandoI@>
@<le, comandoA@>
@<le, comandoD@>
@<le, comandoC@>

def main():
  global comando

  @<le, prende la linea da editare@>
  @<le, ciclo di elaborazione dei comandi@>

main()
@}

La linea dei comandi viene rintracciata attraverso il parametro che
viene passato da Ped alla macro. @PP

@d le, prende la linea da editare
@{
if comando.lineaInizio == None:
  comando.lineaInizio = comando.areaLavoro.cursore

lineaCorrente = comando.areaLavoro.getLinea( comando.lineaInizio )
@}

Prima di ogni richiesta di comando all'utente la linea viene stampata:

@d le, ciclo di elaborazione dei comandi
@{
while True:
  print lineaCorrente
  leComando = raw_input()

  if len( leComando.strip() ) == 0:
    break
  elif leComando.strip() == "i":
    lineaCorrente = comandoI( leComando, lineaCorrente )
  elif leComando.strip() == "a":
    lineaCorrente = comandoA( leComando, lineaCorrente )
  elif leComando.strip()[ 0 ] == "d":
    lineaCorrente = comandoD( leComando, lineaCorrente )
  elif leComando.strip()[ 0 ] == "c":
    lineaCorrente = comandoC( leComando, lineaCorrente )

comando.areaLavoro.inserisciInUndo( "lineedit" )
comando.areaLavoro.setLinea( comando.lineaInizio, lineaCorrente )
@}

I comandi operano su un offset che è dato dal numero di spazi presenti
prima del comando. Provare per credere ma funziona. @PP

Il comando "i" e il comando "a" inseriscono dei caratteri all'interno
della riga esattamente come i relativi comandi di Ped:

@d le, comandoI
@{
def comandoI( comando, lineaCorrente ):
  testo = raw_input( "i>" )
  offset = len( comando ) - len( comando.lstrip() )

  if offset < len( lineaCorrente ):
    return lineaCorrente[ : offset ] + testo + lineaCorrente[ offset : ]
@}

@d le, comandoA
@{
def comandoA( comando, lineaCorrente ):
  testo = raw_input( "a>" )
  offset = len( comando ) - len( comando.lstrip() )

  if offset <= len( lineaCorrente ):
    return lineaCorrente[ : offset + 1 ] + testo + lineaCorrente[ offset + 1: ]
@}

I comandi @F D e @F C sono l'equivalente di quelli di ped e operano sulla parte stringa
che sta fra il nome del comando e l'ultimo carattere non bianco.

@d le, comandoC
@{
def comandoC( comando, lineaCorrente ):
  testo = raw_input( "c>" )
  offset = len( comando ) - len( comando.lstrip() )
  qta = len( comando.strip() )

  if offset < len( lineaCorrente ) and ( offset + qta ) <= len( lineaCorrente ):
    return lineaCorrente[ : offset ] + testo + lineaCorrente[ offset + qta : ]
@}

@d le, comandoD
@{
def comandoD( comando, lineaCorrente ):
  offset = len( comando ) - len( comando.lstrip() )
  qta = len( comando.strip() )

  if offset < len( lineaCorrente ):
    return lineaCorrente[ : offset ] + lineaCorrente[ offset + qta : ]
@}

@End @Section

@Section
@Title { Cerca e rimpiazza su files }
@Begin @PP

Questa macro è una funzionalità di cerca e rimpiazza su tutti i files
che rispettano una certa maschera. @PP

@o pgrep.py
@{
import os
import os.path
import re
import fnmatch

@<pgrep, operateOnFile@>

def main():
  @<pgrep, fileCallback@>
  @<pgrep, richiesta dati in ingresso@>
  @<pgrep, lavoro@>

main()
@}

All'utente vengono richieste le informazioni che servono per effettuare
le operazioni:

@d pgrep, richiesta dati in ingresso
@{
sStartingPath = raw_input( "Starting path (.)> ")
sFilesMask = raw_input( "Files mask (*)>" )
sRegex = raw_input( "Regexp to search>" )
sSub = raw_input( "Substitution>" )

if sSub == "":
  sConfirm = raw_input( "Press Y to substitute>" )
  if sConfirm == "Y":
    bSubstitute = True
  else:
    bSubstitute = False
else:
  bSubstitute = True

if sFilesMask == "":
  sFilesMask = "*"
if sRegex == "":
  print "Please insert a regexp."
  return
if sStartingPath == "":
  sStartingPath = "."
@}

Se la stringa da sostituire è vuota e l'utente non seleziona in modo esplicito
la modalità di sostituzione il programma cerca la stringa nei files e
non sostituisce la stringa all'interno delle linee. @PP

Per ogni file viene chiamata una funzione che effettua il lavoro:

@d pgrep, lavoro
@{
os.path.walk( sStartingPath, fileCallback, None )
@}

La funzione che viene chiamata è la @F {fileCallback}, che deve comunque
verificare che il file passato aderisca alla maschera inserita dall'utente:

@d pgrep, fileCallback
@{
def fileCallback( a, sDirname, rsFileNames ):
  for sNomefile in rsFileNames:
    sNomeCompleto = os.path.join( sDirname, sNomefile )
    if fnmatch.fnmatch( sNomefile, sFilesMask ) and os.path.isfile( sNomeCompleto ):
      operateOnFile( os.path.join( sDirname, sNomefile ), sRegex, bSubstitute, sSub )
@}

La funzione @F {operateOnFile} riceve il nome del file sul quale effettuare la
ricerca e la sostituzione, la regexp compilata e l'eventuale sostituzione.

@d pgrep, operateOnFile
@{
def operateOnFile( sNomeFile, sRegex, bSubstitute, sSub ):
  print sNomeFile
  comando.ped.eseguiComando( "e " + sNomeFile )
  if not bSubstitute:
    comando.ped.eseguiComando( "g/" + sRegex + "/" )
    comando.ped.eseguiComando( "q" )
  else:
    comando.ped.eseguiComando( "1,$s/" + sRegex + "/" + sSub + "/" )
    if not comando.ped.getAreaLavoroCorrente().isModificato():
      comando.ped.eseguiComando( "q" )
@}

@End @Section

@EndSections

@End @Chapter
