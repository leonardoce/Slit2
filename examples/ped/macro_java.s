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

def main():
  @<macroj, scorre directory@>
  @<macroj, sceglie package@>
  @<macroj, sceglie classe@>
  @<macroj, apre editor@>

main()
@}

Le directory vengono scorse dalla funzione callback @F {callbackWalk}:

@d macroj, scorre directory
@{
def callbackWalk(aListaDirectory, sNomeDirectory, aNomiFiles):
  aNomiClassi = filter(lambda x: x.lower().endswith(".java"), aNomiFiles)
  aNomiClassi = map(lambda x: [ os.path.realpath( os.path.join( sNomeDirectory, x ) ), x[:-5] ], aNomiClassi)
  sNomePackage = ".".join(sNomeDirectory.split( os.path.sep )[1:])

  dInfo = {}
  dInfo['dirName'] = sNomeDirectory
  dInfo['package'] = sNomePackage
  dInfo['classi'] = aNomiClassi

  if len(aNomiClassi) > 0:
    aListaDirectory.append( dInfo )

aDirectorySorgenti = []
os.path.walk( ".", callbackWalk, aDirectorySorgenti )
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

nClasse = int( raw_input( "classe> " ) )
@}

Manca solo da aprire l'editor direttamente alla classe desiderata.

@d macroj, apre editor
@{
sNomeFile = aCurrPackage[ 'classi' ][ nClasse ][0]
comando.areaLavoro.eseguiComando( "e " + sNomeFile )
@}

@End @Chapter
