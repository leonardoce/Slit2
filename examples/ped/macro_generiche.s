@Chapter
@Title { Macro di utilizzo generico }
@Begin @LP

Queste macro non sono pensate per un uso particolare e sono quindi di
ausilio alla globalit… degli utenti. @PP

La prima macro Š una funzionalit… di cerca e rimpiazza su tutti i files
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

Se la stringa da sostituire Š vuota e l'utente non seleziona in modo esplicito
la modalit… di sostituzione il programma cerca la stringa nei files e
non sostituisce la stringa all'interno delle linee. @PP

Per ogni file viene chiamata una funzione che effettua il lavoro:

@d pgrep, lavoro
@{
os.path.walk( sStartingPath, fileCallback, None )
@}

La funzione che viene chiamata Š la @F {fileCallback}, che deve comunque
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

@End @Chapter
