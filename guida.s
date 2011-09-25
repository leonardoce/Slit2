@Chapter
@Title { User guide}
@Begin
@LP
Slit's files are composed of text's lines of documentation and instructions. Instructions are lines 
that became to a "@" character and are followed to a letter which determine the directive. @PP
In this document the supported instructions are this: @PP
@BulletList
@ListItem { "@d" is the instruction which define a macro.} @PP
@ListItem { "@o" is the instruction that we use to write a  source file} @PP
@ListItem { "@i" is the instruction who we use to include a slit file to another.}@PP
@EndList
There is a very important thing to remember: the directive are valid only if that are write at the firt character of the line. This to 
avoid that slit don't exchange a string like an instruction. @PP
@BeginSections
@Section @Title { La direttiva "@d" }
@Begin @PP
The directive "@d" is the primary instruction of Slit:
this instruction is used to memorize a text's macro into the sistem.
the macro can be used into others macro or only to write a sourced file. @PP
this is an example of a directive "@d":
@IndentedDisplay @F 
@Verbatim @Begin
 @d funzione sum
@{
function sum (a,b:Integer) return Integer
begin
  return a+b;
end;
@}
@End @Verbatim
How we see the directive "@d" is followed to a  @I scrap. A @I scrap, for the Slit terminology, is
the content of the defined macro. @PP
A scrap became to "@{" and finished with "@}". All that is between this two lines 
is see like the content of the macro that will be define. @PP

In base al tipo di processore scelto per l'output Slit enuncia il
nome della macro e il suo contenuto all'interno della documentazione.
Ad ogni macro viene associato un numero che la identifica e alla fine
della macro vengono enunciati i numeri di macro che utilizzano la macro
appena definita. @PP

Una macro pu{@Char ograve} anche essere non utilizzata, in questo 
caso Slit avverte
l'utente alla fine del processo di produzione dei files sorgenti. @PP

@End @Section

@Section @Title { La direttiva "@o" e sintassi degli scrap }
@Begin @PP

La direttiva "@o" @Char egrave molto simile, dal punto di vista sintattico,
a quella "@d". Infatti viene utilizzata per definire un tipo particolare di
macro, il cui nome viene utilizzato per scrivere un file. @PP

Ad esempio: 

@IndentedDisplay @F 
@Verbatim @Begin
 @o prova.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    printf("Hello world!\n");
    return 0;
}
@}
@End @Verbatim

All'interno di uno scrap si pu{@Char ograve} includere un riferimento ad
un'altro:

@IndentedDisplay @F 
@Verbatim @Begin
 @d saluta
@{
printf("Hello world!\n");
@}
@End @Verbatim

@IndentedDisplay @F 
@Verbatim @Begin
 @o provadue.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    @<saluta@>
    return 0;
}
@}
@End @Verbatim

Nello scrivere il codice sorgente slit ricorda il livello di indentazione dei
riferimenti alle macro e li utilizza per scrivere il codice.

@End @Section

@Section @Title { La direttiva "@i" }
@Begin @PP

La direttiva "@i" invece serve per includere un file slit da un'altro. @PP

Molto spesso i programmi scritti con Slit sono composti da pi{@Char ugrave}
files Slit. La direttiva "@i" dice a Slit di andare a leggere un'altro file
e ritornare al file chiamante quando la lettura viene completata. @PP

Ad esempio:

@IndentedDisplay @F @Verbatim { @i provatre_funzioni.s }
@IndentedDisplay @F @Verbatim { @i provatre_dichiarazioni.s }

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>
@<funzioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@End @Section

@Section @Title { La direttiva "@+" }
@Begin @PP

La direttiva @F "@+" aggiunge uno scrap in coda a una macro gi{@Char agrave}
esistente. Questo serve per ridurre la quantit{@Char agrave} dei nomi di
macro utilizzati e per non essere costretti ad inventarsi nomi non utili.
@PP

Ad esempio:

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @d dichiarazioni
@{
void funzioneUno();
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @+ dichiarazioni
@{
void funzionePrincipale();
@}
@End @Verbatim

@End @Section

@Section @Title { Opzioni (la direttiva "@x") }
@Begin @PP

Slit pu{@Char ograve} essere configurato attraverso delle opzioni. Le opzioni
si comunicano utilizzando la direttiva @F @Verbatim { @x }. @PP
Questa direttiva @Char egrave seguita da una stringa che @Char egrave
il nome dell'opzione stessa. Ad esempio:

@IndentedDisplay @F @Verbatim { @x output_html }

Questa @Char egrave la lista delle opzioni supportate:

@BulletList
@ListItem { @F output_html : seleziona il formato html per l'output }
@ListItem { @F output_txt : seleziona il formato testo per l'output }
@ListItem { @F output_lout : seleziona il formato lout per l'output }
@ListItem { @F section_markers : abilita la produzione dei marcatori di
inizio e fine sezione }
@ListItem { @F no_section_markers : disabilita la produzione dei marcatori
di inizio e fine sezione }
@ListItem { @F line_markers : abilita la produzione dei marcatori di
riga nei file sorgenti generati }
@ListItem { @F no_line_markers : disabilita la produzione dei marcatori
di riga nei file sorgenti generati }
@ListItem { @F comment_markers : questa opzione serve per inserire
i delimitatori di commento per i linguaggi non ancora supportati da Slit
oppure per personalizzare quelli gi{@Char agrave} esistenti. }
@EndList

L'opzione @F comment_markers {@Char egrave} seguita da un parametro
che indica il linguaggio e i marcatori di inizio e di fine commento.
Questo parametro ha la forma @F @Verbatim { 
<separatore><estensione><separatore><inizio><separatore><fine>
}, dove il separatore pu{@Char ograve} essere qualsiasi carattere @PP

Ad esempio la configurazione predefinita per il linguaggio Pascal
potrebbe essere espressa nella seguente forma:
@F @Verbatim @Begin 
 @x comment_markers /.pas/{/}/
@End @Verbatim. @PP

@End @Section

@Section @Title { Documentazione }
@Begin @PP

Tutto quello che non @Char egrave una direttiva viene direttamente passato 
nel file di documentazione generato senza alcuna trasformazione
intermedia. @PP

Slit deve essere utilizzato insieme a un formato di interscambio di testi.
In questo momento si pu{@Char ograve} utilizzare:

@BulletList
@ListItem { il formato HTML, }
@ListItem { il formato testo, }
@ListItem { il sistema di elaborazione testi Lout. }
@EndList

La documentazione deve essere inserita in modo coerente con il formato
scelto. @PP
Il formato Lout permette anche la generazione di una cross-reference
fra le macro inserite: ogni macro viene numerata e al termine della 
macro viene inserita la lista di tutte le macro che la utilizzano. @PP

@End @Section

@EndSections

@End @Chapter


@Chapter
@Title { Il comando Slit }
@Begin @LP

I programmi vengono normalmente scritti in file di testo che possono avere qualunque esensione.
Per separare il codice dalla documentazione di utilizza il comando @F {slit}: @PP

@Display @F @Verbatim {
slit <nomefile>
}

Questo comando processa il file con il nome passato e ne interpreta le direttive. Dal file
passato vengono quindi generati: @PP

@BulletList
@ListItem { i file della documentazione; }
@ListItem { i vari file che sono il codice sorgente. }
@EndList

La procedura principale infatti recita:

@d slit procedura principale
@{
begin
  if ParamCount = 1 then
  begin
    @<slit preparazione dell'ambiente@>
    @<slit riempimento del magazzino delle macro@>
    @<slit calcola riferimenti@>
    @<slit controlla macro non utilizzate@>
    @<slit generazione della documentazione@>
    @<slit generazione del codice sorgente@>
    @<slit pulizia@>
  end
  else
  begin
    writeln('Uso: ', ParamStr(0), ' <nomefile>');
  end;
end.
@}

Il file di ingresso {@Char egrave} rappresentato da un oggetto della classe @F TSlitStream
mentre il file di uscita della documentazione {@Char egrave} rappresentato da una classe
della famiglia @F { TSlitOutputTxt }.

Le macro vengono memorizzate all'interno di un "magazzino" dal quale vengono
poi riprese per scrivere i files sorgenti.

@d slit preparazione dell'ambiente
@{
store := TMacroStore.Create;
stream := TSlitStream.CreateForFile(ParamStr(1));
driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
@}

La documentazione viene elaborata attraverso un metodo dello stream in ingresso:

@d slit riempimento del magazzino delle macro
@{
stream.Driver := driverMagazzinoMacro;
stream.Process();
stream.ResetStream();
@}

Dopo aver popolato il magazino delle macro vengono calcolati i riferimenti:

@d slit calcola riferimenti
@{
store.CalcolaRiferimenti();
@}

Una volta calcolati i riferimenti {@Char egrave} possibile controllare la presenza di macro
mai utilizzate:

@d slit controlla macro non utilizzate
@{
ControllaMacroNonUtilizzate();
@}

Il controllo viene fatto scorrendo tutto il magazzino della macro.

@d slit ControllaMacroNonUtilizzate
@{
procedure ControllaMacroNonUtilizzate;
var
  tempMacro : TMacroRecord;
  i : integer;
begin
  for i := 0 to store.MacroCount-1 do
  begin
    tempMacro := store.GetRecord( i );
    if (tempMacro.macroUsersCount = 0) and (tempMacro.macroType <> FileMacro) then
    begin
      LogErrorMessage('La macro ' + tempMacro.macroName + ' non e'' mai stata utilizzata.');
    end;
  end;
end;
@}

Viene adesso avviata la generazione della documentazione:

@d slit generazione della documentazione
@{
streamOutputDocumentazione := CreaStreamOutputDaOpzioni(ParamStr(1), store);
driverScriviDocumentazione := 
  TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream( streamOutputDocumentazione );

stream.Driver := driverScriviDocumentazione;
stream.Process();
stream.ResetStream();
writeln(store.MacroCount, ' macro processate');
@}

Viene poi avviata l'elaborazione dei file sorgenti:

@d slit generazione del codice sorgente
@{
ProcessaFiles();
@}

Poi vengono deallocati gli oggetti creati:

@d slit pulizia
@{
FreeAndNil(driverMagazzinoMacro);
FreeAndNil(driverScriviDocumentazione);
FreeAndNil(streamOutputDocumentazione);
FreeAndNil(stream);
FreeAndNil(store);
@}

@End @Chapter
