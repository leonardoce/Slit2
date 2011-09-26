@Chapter
@Title { Macro per programmare in Java }
@Begin @LP

Queste sono delle macro utili per programmare in Java e vengono
invocate direttamente da Ped. @PP

La prima macro Š un semplice browser per le classi Java che, assuemndo
che la directory corrente sia quella dove sono depositati i sorgenti
propone un elenco dei package e, fra i package, un elenco delle classi.
@PP

Quando l'utente seleziona una classe quella classe viene direttamente
aperta nell'editor. @PP

@o javab.py
@{
import os
import os.path

aEstensioniSorgenti = [ ".java", ".as", ".s", ".py" ]

def main():
  @<macroj, scorre directory@>
  @<macroj, sceglie package@>
  @<macroj, sceglie classe@>
  @<macroj, apre editor@>

main()
@}

Le directory vengono scorse dalla funzione callback @F {callbackWalk} iniziando
dalla directory corrente oppure da quella specificata nel comando.
I files all'interno delle directory vengono filtrati in modo da prendere
solamente quelli con estensioni "buone" ovvero con estensioni
presenti nella lista @F {aEstensioniSorgenti}.

@d macroj, scorre directory
@{
def callbackWalk(aListaDirectory, sNomeDirectory, aNomiFiles):
  aNomiClassi = filter(lambda x: os.path.splitext(x.lower())[1] in aEstensioniSorgenti, aNomiFiles)
  aNomiClassi = map(lambda x: [ os.path.realpath( os.path.join( sNomeDirectory, x ) ), os.path.splitext(x)[0] ], aNomiClassi)
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

@d macroj, sceglie package
@{
i = 0
for d in aDirectorySorgenti:
  i = i+1
  print i, d['package']

nPackage = int( raw_input( "package> " ) )
aCurrPackage = aDirectorySorgenti[ nPackage-1 ]
@}

A questo punto l'utente sceglie fra le classi presenti:

@d macroj, sceglie classe
@{
i = 0
for c in aCurrPackage[ 'classi' ]:
  i = i+1
  print i, c[1]

nClasse = int( raw_input( "classe> " ) ) -1
@}

Manca solo da aprire l'editor direttamente alla classe desiderata.

@d macroj, apre editor
@{
sNomeFile = aCurrPackage[ 'classi' ][ nClasse ][0]
risultato = comando.ped.eseguiComando( "e " + sNomeFile )
comando.stampaSchermo = risultato.stampaSchermo
@}

@End @Chapter
