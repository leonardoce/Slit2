@Chapter
@Title { Macro per programmare }
@Begin @LP

Queste sono delle macro utili per programmare in Java e vengono
invocate direttamente da Ped. @PP

@BeginSections

@Section
@Title { Browser fra i files sorgenti }
@Begin @PP

La prima macro Š un semplice browser per i files sorgenti che
propone un elenco dei package e, fra i package, un elenco dei files.
@PP

Quando l'utente seleziona un file quel file viene direttamente
aperto nell'editor. @PP

@o bro.py
@{
import os
import os.path

aEstensioniSorgenti = [ ".java", ".as", ".s", ".py", ".mxml" ]

def main():
  @<bro, scorre directory@>
  @<bro, sceglie package@>
  @<bro, sceglie classe@>
  @<bro, apre editor@>

main()
@}

Le directory vengono scorse dalla funzione callback @F {callbackWalk} iniziando
dalla directory corrente oppure da quella specificata nel comando.
I files all'interno delle directory vengono filtrati in modo da prendere
solamente quelli con estensioni "buone" ovvero con estensioni
presenti nella lista @F {aEstensioniSorgenti}.

@d bro, scorre directory
@{
def callbackWalk(aListaDirectory, sNomeDirectory, aNomiFiles):
  aNomiClassi = filter(lambda x: os.path.splitext(x.lower())[1] in aEstensioniSorgenti, aNomiFiles)
  aNomiClassi = map(lambda x: [ os.path.realpath( os.path.join( sNomeDirectory, x ) ), x ], aNomiClassi)
  sNomePackage = ".".join(sNomeDirectory.split( os.path.sep )[1:])

  dInfo = {}
  dInfo['dirName'] = sNomeDirectory
  dInfo['package'] = sNomePackage
  dInfo['classi'] = aNomiClassi

  if len(aNomiClassi) > 0:
    aListaDirectory.append( dInfo )

aDirectorySorgenti = []
if " " in comando.txtComando.strip():
  sNomeDirectory = comando.txtComando.strip()
  sNomeDirectory = sNomeDirectory[ sNomeDirectory.index(" ") + 1:]
else:
  sNomeDirectory = "."
os.path.walk( sNomeDirectory, callbackWalk, aDirectorySorgenti )
@}

Le informazioni vengono messe in una lista dove ogni elemento Š un
dizionario che rappresenta le informazioni di un package. Le classi
vengono inserite in questo dizionario e ogni classe Š rappresentata da
una lista il cui primo elemento Š la path assoluta della classe
e il secondo Š il nome della classe. @PP

I package vengono proposti all'utente uno per uno:

@d bro, sceglie package
@{
i = 0
for d in aDirectorySorgenti:
  i = i+1
  print i, d['package']

nPackage = int( raw_input( "package> " ) )
aCurrPackage = aDirectorySorgenti[ nPackage-1 ]
@}

A questo punto l'utente sceglie fra le classi presenti:

@d bro, sceglie classe
@{
i = 0
for c in aCurrPackage[ 'classi' ]:
  i = i+1
  print i, c[1]

nClasse = int( raw_input( "classe> " ) ) -1
@}

Manca solo da aprire l'editor direttamente alla classe desiderata.

@d bro, apre editor
@{
sNomeFile = aCurrPackage[ 'classi' ][ nClasse ][0]
risultato = comando.ped.eseguiComando( "e " + sNomeFile )
comando.stampaSchermo = risultato.stampaSchermo
@}

@End @Section

@Section
@Title { tab2spaces - Converte i tabs in spazi }
@Begin @PP

La macro @F {tab2spaces} converte i tabs che sono all'inizio delle righe in
coppie di spazi. Questo serve per togliere tutti i tab che servono per
l'indentazione in un file sorgente e sostituirli con due spazi. Come
discusso altrove (@I {tabs are evil}) gli spazi sono quasi sempre meglio
dei tabs. @PP

La macro in questione lavora sempre sull'intero buffer e sostituisce solamente
gli spazi che sono all'inizio di una riga per evitare di stroncare il file sorgente.
@PP

@o tabs2spaces.py
@{
import re

prog = re.compile( "([\t ]*)(.*)" )

def main():
  comando.areaLavoro.inserisciInUndo( "tab2spaces" )
  for iRiga in comando.areaLavoro.getNumeriLineeFra( 1, len( comando.areaLavoro.getBuffer() ) ):
    @<tabs2spaces, processa riga@>

main()
@}

Ogni riga viene elaborata e divisa in prefisso (spazi e tabs) e suffisso
(altre cose). La riga viene poi ricomposta sostituendo i tabs con i doppi
spazi nel prefisso. @PP

@d tabs2spaces, processa riga
@{
gruppi = prog.match( comando.areaLavoro.getLinea( iRiga ) ).groups()
prefisso = re.sub( "\t", "  ", gruppi[0] )
comando.areaLavoro.setLinea( iRiga, prefisso + gruppi[1] )
@}

@End @Section

@EndSections
@End @Chapter
